from abc import ABC, abstractmethod

from constants import INDENT
from errors import TracebackError

from lexer import Lexer
from parser import Parser
from engine import *


class InterpreterError(Exception):
    pass # just used as a type-group


class InterpreterNotImplementedError(InterpreterError):
    pass


class InterpreterTracebackError(TracebackError, InterpreterError, ABC):
    def __init__(self, badTraces):
        message = self._generateMessage(badTraces)
        badStarts = map(lambda trace: trace["start"], badTraces)
        badEnds = map(lambda trace: trace["end"], badTraces)
        super().__init__(message, badStarts, badEnds)

    @abstractmethod
    def _generateMessage(self, badTraces):
        return # message string


# not meant to be raised; just inherits from TracebackError for its formatting capabilities
class InterpreterTracebackWarning(InterpreterTracebackError):
    def warn(self, outputFn):
        outputFn(self.message)


class UndefinedIdentifierError(InterpreterTracebackError):
    def _generateMessage(self, badTraces):
        plural = len(badTraces) > 1
        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "{}{}ndefined identifier{} {} given: {}".format(
            "An " if not plural else "",
            "u" if not plural else "U",
            "s" if plural else "",
            "were" if plural else "was",
            ','.join(badIdentifierStrs),
        )


class UnusedArgumentsWarning(InterpreterTracebackWarning):
    def _generateMessage(self, badTraces):
        plural = len(badTraces) > 1
        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "Variable{} {} {} not used in {} template definition".format(
            "s" if plural else "",
            ','.join(badIdentifierStrs),
            "were" if plural else "was",
            "their" if plural else "its",
        )


class InvalidExpressionError(InterpreterTracebackError):
    def _generateMessage(self, badTraces):
        plural = len(badTraces) > 1
        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "The variable{} {} cannot be evaluated in an expression".format(
            "s" if plural else "",
            ','.join(badIdentifierStrs)
        )


class BadTemplateEvaluationError(InterpreterTracebackError):
    def _generateMessage(self, badTraces):
        templateCall = badTraces[0]["obj"]
        templateName = str(templateCall.nameId)
        return "Template {} does not evaluate to an expression; it cannot be used in an expression".format(
            templateName,
        )


class Interpreter:
    def __init__(self, outputFn):
        self._outputFn = outputFn
        self._solutions = list()

        # self._parseStack = ...
        self._resetParseStack()
        self._parseResult = None

        self._lexer = Lexer()
        self._parser = Parser()
        self._engine = Engine()
        self._bindToParser()

    def _resetParseStack(self):
        self._parseStack = {
            "identifiers": [],
            "numbers": [],
            "values": [],
            
            "operations": [],
            "expressions": [],
            # these stacks hold operation lists, not single values
            "operationslow": [],
            "operationsmid": [],
            "operationshigh": [],
            "operationsmax": [],

            "aliasNames": [],
            "templateArgs": [],
            "aliasRightHands": [],
        }

    # treats the string as user input
    def executeLine(self, string):
        try:
            result = self.evaluateLine(string)
            if result["type"] == "relation":
                leftPiece = result["leftHandExpr"]
                rightPiece = result["rightHandExpr"]
                self._ensureIdentifiersAreExpressable(leftPiece)
                self._ensureIdentifiersAreExpressable(rightPiece)
                self._ensureValidTemplateEvals(leftPiece)
                self._ensureValidTemplateEvals(rightPiece)
                leftExpr = leftPiece.obj
                rightExpr = rightPiece.obj
                subLeftExpr = self._engine.substituteTemplates(leftExpr)
                subRightExpr = self._engine.substituteTemplates(rightExpr)
                relation = Relation(subLeftExpr, subRightExpr)
                self._engine.addRelation(relation)
            
            elif result["type"] == "alias":
                isTemplate = result["templateArgs"].obj is not None
                if isTemplate:
                    idsPiece = result["aliasNames"]
                    # only one identifier allowed by parser for template alias names
                    identifier = idsPiece.obj[0]
                    argsPiece = result["templateArgs"]
                    rightHandPiece = result["rightHand"]

                    self._ensureTemplateUses(argsPiece, rightHandPiece)

                    template = Template(argsPiece.obj, rightHandPiece.obj)
                    self._engine.setAlias(identifier, template)

                else:
                    idsPiece = result["aliasNames"]
                    rightHandPiece = result["rightHand"]
                    rightHand = rightHandPiece.obj

                    self._ensureDefined(rightHandPiece)
                    self._ensureIdentifiersAreExpressable(rightHandPiece)
                    self._ensureValidTemplateEvals(rightHandPiece)

                    rightWithSubs = self._engine.substitute(rightHand)
                    self._engine.setAliases(idsPiece.obj, rightWithSubs)
            
            elif result["type"] == "expression":
                exprPiece = result["expression"]
                expr = exprPiece.obj

                self._ensureIdentifiersAreExpressable(exprPiece)
                self._ensureValidTemplateEvals(exprPiece)

                if isinstance(expr, Identifier) and self._engine.isDefined(expr):
                    identifier = expr
                    sub = self._engine.substitute(identifier)
                    self._print(sub)
                    return

                solSet = self._engine.getSolutionsFor(expr)
                if solSet.isNumeric:
                    strMap = map(lambda numeric: str(numeric), solSet)
                    self._print(' or '.join(strMap))
                    return
                
                if len(solSet) == 0:
                    # this will find any undefined variables (wasn't checked yet)
                    self._ensureDefined(exprPiece)

                    # must be a purely numeric expression!
                    numeric = self._engine.substitute(expr)
                    if not isinstance(numeric, Numeric):
                        # this shouldn't ever happen, but in case it does...
                        raise TypeError("(Somehow) a non-numeric value came from what should be a purely numeric expression")
                    self._print(numeric)
                    return
                
                for sol in solSet:
                    solStr = str(sol)
                    parseResult = self.evaluateLine(solStr)
                    exprPiece = parseResult["expression"]
                    expr = exprPiece.obj
                    subExpr = self._engine.substitute(expr)
                    self._print(subExpr)
            
            elif result["type"] == "command":
                self._throwBranchNotImplemented("command executions")
            
            elif result["type"] == "empty":
                pass # nuthin to do
            
            else:
                raise TypeError("Interpreter encountered an unexpected result type")
        except Exception as e:
            self._handleError(e)
            self._resetParseStack()

    # helper that can turn strings into engine-useable data structures
    def evaluateLine(self, string):
        if '\n' in string:
            raise ValueError("Interpreter expected a single line to evaluate; given multiple lines (in a single string)")

        tokens = self._lexer.process(string)
        self._parser.inspect(tokens, string)
        result = self._parseResult
        self._parseResult = None
        # stack should be empty (otherwise, parser callbacks aren't
        # communicating/working properly)
        self._assertParseStackEmpty()
        return result

    # errors when identifiers are not defined (aka aliased)
    def _ensureDefined(self, stackPiece):
        badTraces = []
        for trace in stackPiece.traces:
            if trace["type"] == TRACE_TYPES["IDENTIFIER"]:
                identifier = trace["obj"]
                if not self._engine.isDefined(identifier):
                    badTraces.append(trace)
        if len(badTraces) > 0:
            raise UndefinedIdentifierError(badTraces)

    # warns when a template's arguments are not used in its definition
    def _ensureTemplateUses(self, argNamesPiece, rightHandPiece):
        badTraces = []
        for argTrace in argNamesPiece.traces:
            if argTrace["type"] == TRACE_TYPES["IDENTIFIER"]:
                inRightHand = False
                for rightHandTrace in rightHandPiece.traces:
                    if rightHandTrace["type"] == TRACE_TYPES["IDENTIFIER"]:
                        if argTrace["obj"] == rightHandTrace["obj"]:
                            inRightHand = True
                            break
                if not inRightHand:
                    badTraces.append(argTrace)
        if len(badTraces) > 0:
            warning = UnusedArgumentsWarning(badTraces)
            warning.warn(self._outputFn)

    # errors when an identifier in an expression doesn't return an expressable
    def _ensureIdentifiersAreExpressable(self, exprPiece, force=False):
        if isinstance(exprPiece.obj, (Expression, NegativeExpression)) or force:
            badTraces = []
            for idTrace in exprPiece.traces:
                if idTrace["type"] == TRACE_TYPES["IDENTIFIER"]:
                    identifier = idTrace["obj"]
                    alias = self._engine.substitute(identifier)
                    if not isinstance(alias, Expressable):
                        badTraces.append(idTrace)
            if len(badTraces) > 0:
                raise InvalidExpressionError(badTraces)

    # errors when a template evaluates to a non-expressable in an expression
    def _ensureValidTemplateEvals(self, exprPiece):
        badTraces = []
        for templateTrace in exprPiece.traces:
            if templateTrace["type"] == TRACE_TYPES["TEMPLATE_CALL"]:
                templateId = templateTrace["obj"].nameId
                template = self._engine.getAlias(templateId)
                shouldBeExpr = template.rightHand
                if not isinstance(shouldBeExpr, Expressable):
                    badTraces.append(templateTrace)
        if len(badTraces) > 0:
            raise BadTemplateEvaluationError(badTraces)

class InterpreterParser:
    def __init__(self):
        self._lexer = Lexer()
        self._parser = Parser()
        self._bindToParser()

        # self._stacks = ...
        self._resetStack()
        self._parseResult = None

    @property
    def lastResult(self):
        return self._parseResult

    def _resetStack(self):
        self._stacks = {
            "identifiers": [],
            "numbers": [],
            "values": [],
            
            "operations": [],
            "expressions": [],
            # these stacks hold operation lists, not single values
            "operationslow": [],
            "operationsmid": [],
            "operationshigh": [],
            "operationsmax": [],

            "aliasNames": [],
            "templateArgs": [],
            "aliasRightHands": [],
        }

    def evaluateLine(self, string):
        try:
            if '\n' in string:
                raise ValueError("Interpreter expected a single line to evaluate; given multiple lines (in a single string)")

            tokens = self._lexer.process(string)
            self._parser.inspect(tokens, string)
            # stack should be empty (otherwise, parser callbacks aren't
            # communicating/working properly)
            self._assertParseStackEmpty()
            return self.lastResult
        finally:
            self._resetStack()

    def _bindToParser(self):
        # this function is only run on initialization; these helper functions
        # aren't too inefficient
        def pushStack(key, stackPiece):
            if not isinstance(stackPiece, StackPieceTracer):
                raise TypeError("Interpreter pushed non-tracer type to stack")
            return self._stacks[key].append(stackPiece)
        def popStack(key):
            return self._stacks[key].pop()
        
        # each callback should generally be popping StackPieces from one stack,
        # potentially modifying their data, and pushing onto another stack
        def onStart(tokens, branch):
            if branch == "re EOL":
                self._parseResult = {
                    "type": "relation",
                    "rightHandExpr": popStack("expressions"),
                    "leftHandExpr": popStack("expressions"),
                }
            elif branch == "al EOL":
                self._parseResult = {
                    "type": "alias",
                    "aliasNames": popStack("aliasNames"),
                    "templateArgs": popStack("templateArgs"),
                    "rightHand": popStack("aliasRightHands"),
                }
            elif branch == "ex EOL":
                self._parseResult = {
                    "type": "expression",
                    "expression": popStack("expressions"),
                }
            elif branch == "co EOL":
                self._parseResult = {
                    "type": "command",
                }
            elif branch == "EOL":
                self._parseResult = {
                    "type": "empty",
                }
        self._parser.onStart(onStart)

        def onNumber(tokens, branch):
            if branch == "NU" or branch == "EN":
                numberStr = str(tokens[0])
                value = Numeric(numberStr)
            piece = StackPieceTracer(value, tokens)
            pushStack("numbers", piece)
        self._parser.onNumber(onNumber)

        def onFullIdentifier(tokens, branch):
            if branch == "id":
                tokenStrs = map(lambda token: str(token), tokens)
                fullId = ''.join(tokenStrs)
                value = Identifier(fullId)
            piece = StackPieceTracer(value, tokens)
            piece.trace(TRACE_TYPES["IDENTIFIER"])
            pushStack("identifiers", piece)
        self._parser.onFullIdentifier(onFullIdentifier)

        def onIdentifiers(tokens, branch):
            if branch == "fu":
                piece = popStack("identifiers")
                identifier = piece.obj
                identifiers = [identifier]
                piece.update(identifiers, tokens, [])
            elif branch == "fu CO ids":
                piece = popStack("identifiers")
                identifiers = piece.obj
                nextPiece = popStack("identifiers")
                nextIdentifier = nextPiece.obj
                identifiers.insert(0, nextIdentifier)
                piece.update(identifiers, tokens, nextPiece.traces)
            pushStack("identifiers", piece)
        self._parser.onIdentifiers(onIdentifiers)

        def onValue(tokens, branch):
            if branch == "fu":
                piece = popStack("identifiers")
            elif branch == "nu":
                piece = popStack("numbers")
            elif branch == "fu PAO tes PAC":
                argsPiece = popStack("expressions")
                namePiece = popStack("identifiers")
                argExprs = argsPiece.obj
                nameId = namePiece.obj
                templateCall = TemplateCall(nameId, argExprs)
                piece = argsPiece.update(templateCall, tokens, namePiece.traces)
                piece.trace(TRACE_TYPES["TEMPLATE_CALL"])
            elif branch == "fu PAO PAC":
                namePiece = popStack("identifiers")
                argExprs = []
                nameId = namePiece.obj
                templateCall = TemplateCall(nameId, argExprs)
                piece = namePiece.update(templateCall, tokens, [])
                piece.trace(TRACE_TYPES["TEMPLATE_CALL"])
            pushStack("values", piece)
        self._parser.onValue(onValue)

        def onOperatorANY(tokens, branch):
            operatorStr = str(tokens[0])
            piece = StackPieceTracer(operatorStr, tokens)
            # can't use piece.trace() here without reviewing onOperation;
            # that callback ignores this piece's traces
            pushStack("operations", piece)
        self._parser.onOperatorLow(onOperatorANY)
        self._parser.onOperatorMid(onOperatorANY)
        self._parser.onOperatorHigh(onOperatorANY)

        def evaluateOperationList(opList):
            if type(opList) is not list:
                raise TypeError("cannot evaluate non-list-type as operation list")
            if not isinstance(opList[0], Expressable):
                raise RuntimeError("tried to evaluate operation list with a non-expressable (left-hand)")
            # collapses operations left-to-right
            while len(opList) > 1:
                try:
                    leftExpr = opList.pop(0)
                    oper = opList.pop(0)
                    rightExpr = opList.pop(0)
                except IndexError as e:
                    if "pop from empty" in str(e):
                        raise IndexError("Interpreter -> evaluateOperationList(opList) given bad opList")
                if not isinstance(rightExpr, Expressable):
                    raise RuntimeError("tried to evaluate operation list with a non-expressable (right-hand)")
                newExpr = Expression(leftExpr, oper, rightExpr)
                opList.insert(0, newExpr)
            resultExpr = opList[0]
            return resultExpr
        def onOperationANY(tokens, branch):
            if branch in ("opm opl opl", "opm"):
                stackName = "operationslow"
                nextStack = "operationsmid"
            elif branch in ("oph opm opm", "oph"):
                stackName = "operationsmid"
                nextStack = "operationshigh"
            elif branch in ("opx oph oph", "opx"):
                stackName = "operationshigh"
                nextStack = "operationsmax"
            elif branch in ("DA opx", "ev"):
                stackName = "operationsmax"
                        
            isOperatorBranch = branch in (
                "opm opl opl",
                "oph opm opm",
                "opx oph oph",
            )
            # operation-max production
            if branch == "DA opx":
                opListPiece = popStack("operationsmax")
                opList = opListPiece.obj
                expr = evaluateOperationList(opList)
                newExpr = NegativeExpression(expr)
                newOpList = [newExpr]
                piece = opListPiece.update(newOpList, tokens, [])
            elif branch == "ev":
                piece = popStack("expressions")
                expr = piece.obj
                exprs = [expr]
                # lower precedence productions expect the stack to contain lists
                piece.update(exprs, tokens, [])
            # all other productions that use an operator
            elif isOperatorBranch:
                opListPiece = popStack(stackName)
                operPiece = popStack("operations")
                nextOpListPiece = popStack(nextStack)
                opList = opListPiece.obj
                oper = operPiece.obj
                nextOpList = nextOpListPiece.obj
                nextExpr = evaluateOperationList(nextOpList)
                # inserted on left to later be evaluated left-to-right
                # (parser productions return right-to-left)
                opList.insert(0, oper)
                opList.insert(0, nextExpr)
                piece = opListPiece.update(opList, tokens, nextOpListPiece.traces)
            # all other productions that just chain precedence
            else:
                nextOpListPiece = popStack(nextStack)
                nextOpList = nextOpListPiece.obj
                expr = evaluateOperationList(nextOpList)
                # productions that use operators will append to this list later
                opList = [expr]
                piece = nextOpListPiece.update(opList, tokens, [])
            pushStack(stackName, piece)
        self._parser.onOperationLow(onOperationANY)
        self._parser.onOperationMid(onOperationANY)
        self._parser.onOperationHigh(onOperationANY)
        self._parser.onOperationMax(onOperationANY)

        def onExpression(tokens, branch):
            opListPiece = popStack("operationslow")
            opList = opListPiece.obj
            expr = evaluateOperationList(opList)
            piece = opListPiece.update(expr, tokens, [])
            pushStack("expressions", piece)
        self._parser.onExpression(onExpression)

        def onEvaluation(tokens, branch):
            exprBranches = (
                "PAO ex PAC",
                "BRO ex BRC",
            )
            if branch == "va":
                piece = popStack("values")
            elif branch in exprBranches:
                # no need to process;
                # the expression is already in the right place
                return
            pushStack("expressions", piece)
        self._parser.onEvaluation(onEvaluation)

        def onLeftAliasANY(tokens, branch):
            isTemplateBranch = branch in ("fu PAO PAC", "fu PAO ids PAC")
            if branch == "fu":
                idPiece = popStack("identifiers")
                identifier = idPiece.obj
                identifiers = (identifier,)
                idsPiece = idPiece.update(identifiers, tokens, [])
                argsPiece = StackPieceTracer(None, [])
            elif branch == "BRO ids BRC":
                idsPiece = popStack("identifiers")
                argsPiece = StackPieceTracer(None, [])
            elif isTemplateBranch:
                if branch == "fu PAO ids PAC":
                    argsPiece = popStack("identifiers")
                    if type(argsPiece.obj) is not list:
                        raise TypeError("Interpreter -> onLeftAliasTemplate() got arguments from stack that were not in list form")
                else:
                    argsPiece = StackPieceTracer([], [])
                idPiece = popStack("identifiers")
                identifier = idPiece.obj
                identifiers = (identifier,)
                idsPiece = idPiece.update(identifiers, tokens, [])
            pushStack("aliasNames", idsPiece)
            pushStack("templateArgs", argsPiece)
        self._parser.onLeftAlias(onLeftAliasANY)
        self._parser.onLeftAliasTemp(onLeftAliasANY)

        def onRightAlias(tokens, branch):
            if branch == "ex":
                rightHandPiece = popStack("expressions")
            else:
                self._throwBranchNotImplemented("alias expressions".format(branch))
            pushStack("aliasRightHands", rightHandPiece)
        self._parser.onRightAlias(onRightAlias)

        def onRightAliasTemp(tokens, branch):
            if branch == "ex":
                piece = popStack("expressions")
            elif branch == "re":
                self._throwBranchNotImplemented("relations on right-side of alias templates")
            elif branch == "co":
                self._throwBranchNotImplemented("commands on right-side of alias templates")
            elif branch == "ob":
                self._throwBranchNotImplemented("objects on right-side of alias templates")
            pushStack("aliasRightHands", piece)
        self._parser.onRightAliasTemp(onRightAliasTemp)

        def onTemplateArguments(tokens, branch):
            if branch == "ex":
                piece = popStack("expressions")
                expr = piece.obj
                exprs = [expr]
                piece.update(exprs, tokens, [])
            elif branch == "ex CO tes":
                piece = popStack("expressions")
                exprs = piece.obj
                nextPiece = popStack("expressions")
                nextExpr = nextPiece.obj
                exprs.insert(0, nextExpr)
                piece.update(exprs, tokens, nextPiece.traces)
            pushStack("expressions", piece)
        self._parser.onTemplateArguments(onTemplateArguments)

        # TODO: finish these features
        def onUnit(tokens, branch):
            self._throwBranchNotImplemented("units")
        self._parser.onUnit(onUnit)

        def onObjectDeclaration(tokens, branch):
            self._throwBranchNotImplemented("objects")
        self._parser.onObjectDeclaration(onObjectDeclaration)

        def onIdentifier(tokens, branch):
            if branch == "ID PE id":
                self._throwBranchNotImplemented("identifiers")
        self._parser.onIdentifier(onIdentifier)

        def onCommand(tokens, branch):
            self._throwBranchNotImplemented("commands")
        self._parser.onCommand(onCommand)

    def _assertParseStackEmpty(self):
        badStackNames = []
        for stackName in self._parseStack:
            stack = self._parseStack[stackName]
            if len(stack) > 0:
                badStackNames.append(stackName)
        if len(badStackNames) > 0:
            raise RuntimeError("Interpreter did not use all items in the parse stack")

    def _throwBranchNotImplemented(self, featureNamePlural):
        raise InterpreterNotImplementedError("SolverPro cannot process {} (yet)".format(featureNamePlural))


TRACE_TYPES = {
    "IDENTIFIER": "IDENTIFIER",
    "TEMPLATE_CALL": "TEMPLATE_CALL",
}
# contains an element of the stack as well as some helpful metadata
class StackPieceTracer:
    __traceId_DO_NOT_MODIFY = 0
    @classmethod
    def _getUniqueTraceId(cls):
        # ...except for here, obviously
        traceId = cls.__traceId_DO_NOT_MODIFY
        cls.__traceId_DO_NOT_MODIFY += 1
        return traceId

    def __init__(self, obj, tokens):
        self._obj = obj
        self._tokens = tokens
        self._traces = []

    @property
    def obj(self):
        return self._obj

    # keyword arguments intentionally avoided to force thinking
    # about all parameters
    def update(self, obj, tokens, withTraces):
        self._obj = obj
        self._tokens = tokens
        for trace in withTraces:
            self._insertTraceSorted(trace)
        return self

    def trace(self, traceType):
        if traceType not in TRACE_TYPES:
            raise ValueError("Interpreter stack can only trace given a valid type")
        traceStart = self._tokens[0].placementStart
        traceEnd = self._tokens[-1].placementEnd
        trace = {
            "id": self._getUniqueTraceId(),
            "type": traceType,
            "obj": self._obj,
            "start": traceStart,
            "end": traceEnd,
        }
        self._traces.append(trace)

    @property
    def traces(self):
        return iter(self._traces)

    def _insertTraceSorted(self, trace):
        lowBound = 0
        upBound = len(self._traces)
        while upBound != lowBound:
            bisectIdx = int((lowBound + upBound) / 2)
            bisectTrace = self._traces[bisectIdx]
            if trace["id"] < bisectTrace["id"]:
                upBound = bisectIdx
            elif trace["id"] > bisectTrace["id"]:
                lowBound = bisectIdx + 1
            else:
                raise ValueError("trace ids were not unique (or a trace was re-added into the list)")
        self._traces[lowBound:upBound] = [trace]


class InterpreterDatabase:
    @property
    def _validDefinitionKeys(self):
        return (
            Identifier,
            Variable,
        )
    @property
    def _validDefinitionVals(self):
        return (
            Numeric,
            Template,
        )

    def __init__(self):
        self._settingDefinition = False
        self._settingInference = False
        
        def definitions(key, value):
            return self._settingDefinition
        def inferences(key, value):
            return self._settingInference
        def templates(key, value):
            return isinstance(value, Template)

        def FDConst():
            return FilteredDict(templates)

        self._dict = FilteredDict(definitions, inferences, dictConstructor=FDConst)

    def define(self, identifier, value):
        self._checkKeysVals([identifier], [value])
        
        self._settingDefinition = True
        self._dict[identifier] = value
        self._settingDefinition = False

    def defineList(self, identifiers, value):
        self._checkKeysVals(identifiers, [value])
        
        self._settingDefinition = True
        self._dict.update(
            (identifier, value)
            for identifier in identifiers
        )
        self._settingDefinition = False

    def getDefinition(self, identifier):
        return self._dict["definitions"].get(identifier)

    def isDefined(self, identifier):
        return identifier in self._dict["definitions"]

    def setInference(self, identifier):
        pass

    def getInference(self, identifier):
        pass

    def isInferred(self, identifier):
        return identifier in self._dict["inferences"]

    def _checkKeysVals(self, keys, values):
        try:
            trying = "keys"
            iter(keys)
            trying = "values"
            iter(values)
        except TypeError as e:
            if "is not iterable" in str(e):
                raise TypeError("database tried to mass-insert {} but was not given an iterable".format(trying))

        for key in keys:
            if not isinstance(key, self._validDefinitionKeys):
                validKeyNames = ', '.join(map(lambda item: repr(item), self._validDefinitionKeys))
                raise TypeError("database tried to insert key that was not one of these: {}".format(validKeyNames))
        for value in values:
            if not isinstance(value, self._validDefinitionVals):
                validValNames = ', '.join(map(lambda item: repr(item), self._validDefinitionVals))
                raise TypeError("database tried to insert value that was not one of these: {}".format(validValNames))

    # substitutes known/inferred values into a given object
    # (defined/inferred are forced-kwargs)
    def substitute(self, subsable, *args, defined=True, inferred=True):
        if len(args) != 0:
            numArgs = len(args) + 1
            raise TypeError("substitute() takes 1 positional argument but {} were given".format(numArgs))
        if not isinstance(subsable, Substitutable):
            raise TypeError("substitute(subsable) -- subsable must be a Substitutable()")
        
        if defined and not inferred:
            substDict = self._definitions
        elif inferred and not defined:
            substDict = self._inferences
        elif defined and inferred:
            substDict = self._defsAndInfs
        else:
            raise ValueError("substitute(defined=False, inferred=False) -- one kwarg must be True!")

        substituted = subsable.substitute(substDict)
        return substituted

    # substitutes known/inferred values only for template calls
    # (defined/inferred are forced-kwargs)
    def substituteTemplates(self, subsable, *args, defined=True, inferred=True):
        if len(args) != 0:
            numArgs = len(args) + 1
            raise TypeError("substituteTemplates() takes 1 positional argument but {} were given".format(numArgs))
        if not isinstance(subsable, Substitutable):
            raise TypeError("substituteTemplates(subsable) -- subsable must be a Substitutable()")
        
        if defined and not inferred:
            substDict = self._definitions_templatesOnly
        elif inferred and not defined:
            substDict = self._inferences_templatesOnly
        elif defined and inferred:
            substDict = self._defsAndInfs_templatesOnly
        else:
            raise ValueError("substituteTemplates(defined=False, inferred=False) -- one kwarg must be True!")
            
        substituted = subsable.substitute(substDict)
        return substituted


# a dictionary in every way, with an added filter() function
class FilteredDict(dict):
    __argCountsAsserted = False

    def __init__(self, *slotFns, dictConstructor=dict):
        self._filtered = dict()
        self._dictConstructor = dictConstructor
        for slotData in slotFns:
            if type(slotData) == tuple:
                if len(slotData) != 2:
                    raise ValueError("FilteredDict must be given 2-tuples (tuple arguments with 2 elements)")
                slotName, slotFn = slotData
                if type(slotName) is not str or slotName == "":
                    raise TypeError("FilteredDict tuple argument's first item must be a non-empty string")
                if not callable(slotFn):
                    raise TypeError("FilteredDict tuple argument's second item was not a function")
            else:
                slotFn = slotData
                if not callable(slotFn):
                    raise TypeError("FilteredDict received a non-callable function argument")
                slotName = slotFn.__name__
                if slotName == "<lambda>":
                    raise TypeError("FilteredDict cannot accept unnamed lambda functions (use 2-tuples (name, lambda) to name them)")
            # helps to know this when creating instead of getting errors
            # in update() or __setitem__()...
            if self._getArgCounts(slotFn) != 2:
                raise TypeError("FilteredDict requires functions to take two arguments (key, value)")
            self._filtered[slotName] = (slotFn, self._newDict())

    # returns a filtered version of self
    def filter(self, slotName):
        filterData = self._filtered.get(slotName)
        if filterData is None:
            raise KeyError("FilteredDict was not created with the slot name {}".format(slotName))
        filterFn, filteredDict = filterData
        return filteredDict

    def _newDict(self):
        newDict = self._dictConstructor()
        if not isinstance(newDict, dict):
            raise TypeError("FilteredDict was given a dictConstructor() that does not return dicts")
        return newDict

    # dict overrides
    def __repr__(self):
        superRepr = super().__repr__()
        slotNames = ','.join(self._filtered.keys())
        return "FilteredDict({}){}".format(slotNames, superRepr)

    def __setitem__(self, key, val):
        # queued now and added later in case there are errors
        # (we don't want partially-updated dicts!)
        filtersShouldAdd = set()
        for filterKey in self._filtered:
            filterFn, filterDict = self._filtered[filterKey]
            shouldBeInFilter = filterFn(key, val)
            if shouldBeInFilter:
                filtersShouldAdd.add(filterKey)
        
        # whew! now we can update
        super().__setitem__(key, val)
        for filterKey in filtersShouldAdd:
            filterFn, filterDict = self._filtered[filterKey]
            filterDict[key] = val

    def __delitem__(self, key):
        super().__delitem__(key)
        for filterKey in self._filtered:
            filterFn, filterDict = self._filtered[filterKey]
            if key in filterDict:
                del filterDict[key]

    def update(self, sequence):
        # this ensures the sequence is a valid structure, and allows both the
        # loop and super().update() to consume the sequence iter
        allItemsToUpdate = dict(sequence)
        # will be queued now and added later once we know no errors pop up
        # (we don't want partially updated dicts!)
        filtersToAdd = dict()
        for key in allItemsToUpdate:
            val = allItemsToUpdate[key]
            for filterKey in self._filtered:
                filterFn, filterDict = self._filtered[filterKey]
                shouldBeInFilter = filterFn(key, val)
                if shouldBeInFilter:
                    if filtersToAdd.get(filterKey) is None:
                        filtersToAdd[filterKey] = dict()
                    filtersToAdd[filterKey][key] = val
        
        # whew! now we can update
        super().update(allItemsToUpdate)
        for filterKey in filtersToAdd:
            filterFn, filterDict = self._filtered[filterKey]
            filterDict.update(filtersToAdd[filterKey])

    def copy(self):
        raise NotImplementedError("FilteredDict can't copy")

    def clear(self):
        raise NotImplementedError("FilteredDict can't clear")

    @classmethod
    def _getArgCounts(cls, fn):
        # through some dir() digging, I found this out...
        #   def fn(arg1, arg2, arg3=3, arg4=4):
        #       someVar = arg1
        #       anotherVar = arg2
        # tried this...
        #   fn.__code__.co_varnames
        #       (arg1, arg2, arg3, arg4, someVar, anotherVar)
        # and this...
        #   fn.__code__.co_varnames[:fn.__code__.co_argcount]
        #       (arg1, arg2, arg3, arg4)
        # listed arguments every time!

        # (this number includes keyword arguments)
        return fn.__code__.co_argcount
        

if __name__ == "__main__":
    interpreter = Interpreter(print)
