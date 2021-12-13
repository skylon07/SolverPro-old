from abc import ABC, abstractmethod
from typing import Type

import sympy

from constants import INDENT
from errors import *

from lexer import Lexer
from parser import Parser
from structures import *


class Interpreter:
    def __init__(self, outputFn):
        self._outputFn = outputFn
        self._solutions = list()

        self._parser = InterpreterParser(self)
        self._database = InterpreterDatabase()

    # treats the string as user input
    def executeLine(self, string):
        try:
            result = self._parser.evaluateLine(string)
            if result["type"] == "relation":
                leftPiece = result["leftHandExpr"]
                rightPiece = result["rightHandExpr"]

                # self._ensureIdentifiersAreExpressable(leftPiece)
                # self._ensureIdentifiersAreExpressable(rightPiece)
                # self._ensureValidTemplateParams(leftPiece)
                # self._ensureValidTemplateParams(rightPiece)
                # self._ensureValidTemplateReturns(leftPiece)
                # self._ensureValidTemplateReturns(rightPiece)

                leftExpr = self._evalRep(leftPiece.obj)
                rightExpr = self._evalRep(rightPiece.obj)
                relation = Relation(leftExpr, rightExpr)
                # TODO: track relation
            
            elif result["type"] == "alias":
                isTemplate = result["templateParams"].obj is not None
                if isTemplate:
                    idsPiece = result["aliasNames"]
                    paramsPiece = result["templateParams"]
                    rightHandPiece = result["rightHand"]

                    # self._ensureTemplateUses(paramsPiece, rightHandPiece)

                    # only one identifier allowed by parser for template alias names
                    identifier = self._evalRep(idsPiece.obj[0])
                    params = tuple(self._evalRep(rep) for rep in paramsPiece.obj)
                    rightHand = self._evalRep(rightHandPiece.obj)
                    template = Template(identifier, params, rightHand)
                    self._database.define(identifier, template)

                else:
                    idsPiece = result["aliasNames"]
                    rightHandPiece = result["rightHand"]

                    # self._ensureExisting(rightHandPiece)
                    # self._ensureIdentifiersAreExpressable(rightHandPiece)
                    # self._ensureValidTemplateReturns(rightHandPiece)

                    idList = tuple(self._evalRep(rep) for rep in idsPiece.obj)
                    rightHand = self._evalRep(rightHandPiece.obj)
                    rightWithSubs = self._database.substitute(rightHand)
                    rightIter = (rightWithSubs for i in range(len(idList)))
                    self._database.updateDefinitions(idList, rightIter)
            
            elif result["type"] == "expression":
                exprPiece = result["expression"]
                
                # self._ensureIdentifiersAreExpressable(exprPiece)
                # self._ensureValidTemplateReturns(exprPiece)

                identifier = self._evalRep(exprPiece)
                sub = self._database.substitute(identifier)
                self._print(sub)

                return ## TODO: REMOVE OLD CODE BELOW ##

                if isinstance(expr, Identifier) and self._database.isDefined(expr):
                    identifier = expr
                    sub = self._database.substitute(identifier)
                    self._print(sub)
                    return

                solSet = self._getSolutionsFor(expr)
                if solSet.isNumeric:
                    strMap = map(lambda numeric: str(numeric), solSet)
                    self._print(' or '.join(strMap))
                    return
                
                if len(solSet) == 0:
                    # this will find any undefined variables (wasn't checked yet)
                    self._ensureDefined(exprPiece)

                    # must be a purely numeric expression!
                    numeric = self._database.substitute(expr)
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
                    subExpr = self._database.substitute(expr)
                    self._print(subExpr)
            
            elif result["type"] == "command":
                self._throwBranchNotImplemented("command executions")
            
            elif result["type"] == "empty":
                pass # nuthin to do
            
            else:
                raise TypeError("Interpreter encountered an unexpected result type")
        except Exception as e:
            self._handleError(e)

    # evaluates Represent() objects
    def _evalRep(self, representation):
        if isinstance(representation, Represent):
            cls = representation.cls
            rawArgs = representation.args
            args = self._evalRepArgs(rawArgs)
            return cls(*args)

        elif isinstance(representation, Represent.TemplateCall):
            nameId = self._evalRep(representation.nameId)
            rawParamVals = representation.params
            paramVals = self._evalRepArgs(rawParamVals)
            template = self._database.getDefinition(nameId)
            paramNames = template.parameters
            substDict = dict(zip(paramNames, paramVals))
            return template.substitute(substDict)

        else:
            raise TypeError("Interpreter tried to evaluate an invalid Represent() type")

    def _evalRepArgs(self, repArgs):
        repTypes = (
            Represent,
            Represent.TemplateCall,
        )
        for arg in repArgs:
            if isinstance(arg, repTypes):
                yield self._evalRep(arg)
            else:
                yield arg

    # errors when identifiers are not defined (aliased or inferred)
    def _ensureExists(self, stackPiece):
        badTraces = []
        for trace in stackPiece.traces:
            if trace["type"] == TRACE_TYPES["IDENTIFIER"]:
                identifierRep = trace["obj"]
                identifier = self._evalRep(identifierRep)
                if not self._database.doesExist(identifier):
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
                        argIdRep = argTrace["obj"]
                        argIdName = argIdRep.args[0]
                        rightIdRep = rightHandTrace["obj"]
                        rightIdName = rightIdRep.args[0]
                        if argIdName == rightIdName:
                            inRightHand = True
                            break
                if not inRightHand:
                    badTraces.append(argTrace)
        if len(badTraces) > 0:
            warning = UnusedArgumentsWarning(badTraces)
            warning.warn(self._outputFn)

    # errors when an identifier in an expression doesn't return an expressable
    # (ex. reference, not call, to a template)
    def _ensureIdentifiersAreExpressable(self, exprPiece, force=False):
        badTraces = []
        for idTrace in exprPiece.traces:
            if idTrace["type"] == TRACE_TYPES["IDENTIFIER"]:
                identifierRep = idTrace["obj"]
                identifier = self._evalRep(identifierRep)
                # substitute() used to return alias = identifier if identifier not found
                alias = self._database.substitute(identifier)
                if not isinstance(alias, Expressable):
                    badTraces.append(idTrace)
        if len(badTraces) > 0:
            raise InvalidExpressionError(badTraces)

    # errors when template calls are made with non-template identifiers
    def _ensureValidTemplateCallNames(self, anyPiece):
        badTraces = []
        for callTrace in anyPiece.traces:
            if callTrace["type"] == TRACE_TYPES["TEMPLATE_CALL"]:
                callRep = callTrace["obj"]
                nameRep = callRep.nameId
                nameId = self._evalRep(nameRep)
                shouldBeTemplate = self._database.getExisting(nameId)
                if not isinstance(shouldBeTemplate, Template):
                    badTraces.append(callTrace)
        if len(badTraces) > 0:
            raise NotATemplateError(badTraces)

    # errors when the number of args in a template call mismatches the template
    def _ensureValidTemplateParams(self, anyPiece):
        badTraces = []
        for callTrace in anyPiece.traces:
            if callTrace["type"] == TRACE_TYPES["TEMPLATE_CALL"]:
                callRep = callTrace["obj"]
                nameRep = callRep.nameId
                nameId = self._evalRep(nameRep)
                template = self._database.getExisting(nameId)
                tempParamLen = len(template.parameters)
                callParamLen = len(callRep.params)
                if tempParamLen != callParamLen:
                    modTrace = callTrace.copy()
                    parenOffset = 1
                    numParamsTooMany = callParamLen - tempParamLen
                    if numParamsTooMany > 0:
                        beforeOffset = -1
                        endOffset = -parenOffset + beforeOffset
                        endToken = modTrace["tokens"][endOffset]
                        startOffset = numParamsTooMany * beforeOffset + endOffset
                        startToken = modTrace["tokens"][startOffset]
                    else:
                        lastParenToken = modTrace["tokens"][-parenOffset]
                        startToken = endToken = lastParenToken
                    modTrace["start"] = startToken.placementStart
                    modTrace["end"] = endToken.placementEnd
                    badTraces.append(modTrace)
        if len(badTraces) > 0:
            raise InvalidExpressionError(badTraces)

    # errors when a template evaluates to a non-expressable in an expression
    def _ensureValidTemplateReturns(self, exprPiece):
        badTraces = []
        for callTrace in exprPiece.traces:
            if callTrace["type"] == TRACE_TYPES["TEMPLATE_CALL"]:
                callRep = callTrace["obj"]
                templateIdRep = callRep.nameId
                templateId = self._evalRep(templateIdRep)
                template = self._database.getDefinition(templateId)
                shouldBeExpr = template.rightHand
                if not isinstance(shouldBeExpr, Expressable):
                    badTraces.append(callTrace)
        if len(badTraces) > 0:
            raise BadTemplateEvaluationError(badTraces)

    # I/O helper functions
    def _print(self, *args):
        self._outputFn(INDENT, *args, sep='')

    def _handleError(self, e):
        parserErrors = (
            Parser.ParseError,
            Parser.EOLError,
        )
        if isinstance(e, parserErrors) or isinstance(e, InterpreterError):
            self._outputFn(e)
            return True
        
        self._print("(Internal error) {}: {}".format(type(e).__name__, e))
        return False

    # returns a StructureSet of ALL possible scenarios where 'expr' is true
    def _getSolutionsFor(self, expr):
        pass


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
            "templateParams": [],
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
                    "templateParams": popStack("templateParams"),
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
                value = Represent(Numeric, numberStr)
            piece = StackPieceTracer(value, tokens)
            pushStack("numbers", piece)
        self._parser.onNumber(onNumber)

        def onFullIdentifier(tokens, branch):
            if branch == "id":
                tokenStrs = map(lambda token: str(token), tokens)
                fullId = ''.join(tokenStrs)
                value = Represent(Identifier, fullId)
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
                paramsPiece = popStack("expressions")
                namePiece = popStack("identifiers")
                exprParams = paramsPiece.obj
                nameId = namePiece.obj
                templateResult = self._evaluateTemplate(nameId, exprParams)
                piece = paramsPiece.update(templateResult, tokens, namePiece.traces)
                piece.trace(TRACE_TYPES["TEMPLATE_CALL"])
            elif branch == "fu PAO PAC":
                namePiece = popStack("identifiers")
                exprParams = []
                nameId = namePiece.obj
                templateResult = self._evaluateTemplate(nameId, exprParams)
                piece = namePiece.update(templateResult, tokens, [])
                piece.trace(TRACE_TYPES["TEMPLATE_CALL"])
            pushStack("values", piece)
        self._parser.onValue(onValue)

        # (specifically binary operators)
        def onOperatorANY(tokens, branch):
            if branch == "PL":
                operatorFn = lambda a, b: a + b
            elif branch == "DA":
                operatorFn = lambda a, b: a - b
            elif branch == "ST":
                operatorFn = lambda a, b: a * b
            elif branch == "SL":
                operatorFn = lambda a, b: a / b
            elif branch == "CA":
                operatorFn = lambda a, b: a ** b
            piece = StackPieceTracer(operatorFn, tokens)
            # can't use piece.trace() here without reviewing onOperation;
            # that callback ignores this piece's traces
            pushStack("operations", piece)
        self._parser.onOperatorLow(onOperatorANY)
        self._parser.onOperatorMid(onOperatorANY)
        self._parser.onOperatorHigh(onOperatorANY)

        # specifically binary operations
        def evaluateOperationList(opList):
            if type(opList) is not list:
                raise TypeError("cannot evaluate non-list-type as operation list")
            if not issubclass(opList[0].cls, Expressable):
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
                if not issubclass(rightExpr.cls, Expressable):
                    raise RuntimeError("tried to evaluate operation list with a non-expressable (right-hand)")
                newExpr = Represent(Expression, oper, leftExpr, rightExpr)
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
                negOper = lambda x: -x
                newExpr = Represent(Expression, negOper, expr)
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
                paramsPiece = StackPieceTracer(None, [])
            elif branch == "BRO ids BRC":
                idsPiece = popStack("identifiers")
                paramsPiece = StackPieceTracer(None, [])
            elif isTemplateBranch:
                if branch == "fu PAO ids PAC":
                    paramsPiece = popStack("identifiers")
                    if type(paramsPiece.obj) is not list:
                        raise TypeError("Interpreter -> onLeftAliasTemplate() got arguments from stack that were not in list form")
                else:
                    paramsPiece = StackPieceTracer([], [])
                idPiece = popStack("identifiers")
                identifier = idPiece.obj
                identifiers = (identifier,)
                idsPiece = idPiece.update(identifiers, tokens, [])
            pushStack("aliasNames", idsPiece)
            pushStack("templateParams", paramsPiece)
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

    def _evaluateTemplate(self, name, params):
        return Represent.TemplateCall(name, params)

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


# used to keep track of data for creating an object, and avoids errors
# until the Interpreter has done the checks it wants
class Represent:
    def __init__(self, repCls, *clsArgs):
        self._cls = repCls
        self._args = clsArgs

    @property
    def cls(self):
        return self._cls

    @property
    def args(self):
        return self._args

    class TemplateCall:
        def __init__(self, nameId, params):
            self._nameId = nameId
            self._params = params
        
        @property
        def nameId(self):
            return self._nameId

        @property
        def params(self):
            return self._params


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
            "tokens": self._tokens,
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
        )
    @property
    def _validDefinitionVals(self):
        return (
            Numeric,
            Template,
        )
    @property
    def _validInferenceKeys(self):
        return (
            Identifier,
            Variable,
        )
    @property
    def _validInferenceVals(self):
        return (
            Expressable,
        )

    def __init__(self):
        self._assertEqDictKeys()
        
        self._settingDefinition = False
        self._settingInference = False
        
        def definitions(key, value):
            return self._settingDefinition
        def inferences(key, value):
            return self._settingInference

        self._dict = FilteredDict(
            definitions,
            inferences,
        )

    # working with definitions...
    def define(self, identifier, value):
        self._checkKeysVals([identifier], [value], "definitions")
        
        self._settingDefinition = True
        self._dict[identifier] = value
        self._settingDefinition = False

    def updateDefinitions(self, identifiers, values):
        self._checkKeysVals(identifiers, values, "definitions")
        
        self._settingDefinition = True
        self._dict.update(
            (identifier, value)
            for identifier, value in zip(identifiers, values)
        )
        self._settingDefinition = False

    def getDefinition(self, identifier):
        return self._dict.filter("definitions").get(identifier)

    def isDefined(self, identifier):
        return identifier in self._dict.filter("definitions")

    # working with inferences...
    def setInference(self, identifier, value):
        self._checkKeysVals([identifier], [value], "inferences")

        self._settingInference = True
        self._dict[identifier] = value
        self._settingInference = False

    def updateInferences(self, identifiers, values):
        self._checkKeysVals(identifiers, values, "inferences")

        self._settingInference = True
        self._dict.update(
            (identifier, value)
            for identifier, value in zip(identifiers, values)
        )
        self._settingInference = False

    def getInference(self, identifier):
        return self._dict.filter("inferences").get(identifier)

    def isInferred(self, identifier):
        return identifier in self._dict.filter("inferences")

    # "either" checks
    def getExisting(self, identifier):
        return self._dict.get(identifier)

    def doesExist(self, identifier):
        return identifier in self._dict

    def _assertEqDictKeys(self):
        validKeyLists = (
            self._validDefinitionKeys,
            self._validInferenceKeys,
        )
        objs = {
            Identifier: ('name',),
            Variable: ('name',),
        }
        for validKeyList in validKeyLists:
            for idx1 in range(len(validKeyList)):
                cls1 = validKeyList[idx1]
                args1 = objs[cls1]
                newObj1 = cls1(args1)
                for idx2 in range(len(validKeyList)):
                    cls2 = validKeyList[idx2]
                    args2 = objs[cls2]
                    newObj2 = cls2(args2)
                    # both __eq__ and __hash__ are required to be dictionary keys
                    assert newObj1 == newObj2, "{} and {} are not equivelant dictionary keys".format(newObj1, newObj2)
                    assert hash(newObj1) == hash(newObj2), "{} and {} are not same-hash dictionary keys".format(newObj1, newObj2)
                    
    def _checkKeysVals(self, keys, values, settingType):
        if settingType not in ("definitions", "inferences"):
            raise TypeError("Database cannot check keys/vals for {}".format(settingType))
        
        try:
            # this tests both if the values are iterable, and it prevents
            # consuming generators for the calling function
            trying = "keys"
            keys = list(keys)
            trying = "values"
            values = list(values)
        except TypeError as e:
            if "is not iterable" in str(e):
                raise TypeError("database tried to mass-insert {} but was not given an iterable".format(trying))

        if settingType == "definitions":
            validKeys = self._validDefinitionKeys
            validVals = self._validDefinitionVals
        elif settingType == "inferences":
            validKeys = self._validInferenceKeys
            validVals = self._validInferenceVals

        for key in keys:
            if not isinstance(key, validKeys):
                validKeyNames = ', '.join(map(lambda item: repr(item), validKeys))
                raise TypeError("database tried to insert key that was not one of these: {}".format(validKeyNames))
        for value in values:
            if not isinstance(value, validVals):
                validValNames = ', '.join(map(lambda item: repr(item), validVals))
                raise TypeError("database tried to insert value that was not one of these: {}".format(validValNames))

    # substitutes known/inferred values into a given object
    # (defined/inferred are forced-kwargs)
    def substitute(self, subsable, *args, defined=True, inferred=True):
        if len(args) != 0:
            numArgs = len(args) + 1
            raise TypeError("substitute() takes 1 positional argument but {} were given".format(numArgs))
        if not isinstance(subsable, Substitutable):
            raise TypeError("substitute(subsable) -- subsable must be a Substitutable()")
        
        if defined and inferred:
            substDict = self._dict
        elif not inferred:
            substDict = self._dict.filter("defined")
        elif not defined:
            substDict = self._dict.filter("inferences")
        else:
            raise ValueError("substitute(defined=False, inferred=False) -- one kwarg must be True!")

        substituted = subsable.substitute(substDict)
        return substituted


# a dictionary in every way, with an added filter() function
class FilteredDict(dict):
    def __init__(self, *slotFns):
        self._filtered = dict()
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
            self._filtered[slotName] = (slotFn, self._ChildDict(self, slotName))

    # returns a filtered version of self
    def filter(self, slotName):
        filterData = self._filtered.get(slotName)
        if filterData is None:
            raise KeyError("FilteredDict was not created with the slot name {}".format(slotName))
        filterFn, childDict = filterData
        return childDict

    class _ChildDict(dict):
        def __init__(self, parent, slotName):
            self._parent = parent
            self._name = slotName
        
        # dict overrides
        def __repr__(self):
            superRepr = super().__repr__()
            return "FilteredDict.filter({}){}".format(self._name, superRepr)

        def __setitem__(self, key, val):
            self._parent.__setitem__(key, val)
        def _self__setitem__(self, key, val):
            super().__setitem__(key, val)

        def __delitem__(self, key):
            self._parent.__delitem__(key)
        def _self__delitem__(self, key):
            super().__delitem__(key)

        def update(self, sequence):
            self._parent.update(sequence)
        def _self_update(self, key, val):
            super().update(key, val)

        def copy(self):
            raise NotImplementedError("FilteredDict.filter() can't copy")

        def clear(self):
            raise NotImplementedError("FilteredDict.filter can't clear")

    # dict overrides
    def __repr__(self):
        superRepr = super().__repr__()
        slotNames = ','.join(self._filtered.keys())
        return "FilteredDict({}){}".format(slotNames, superRepr)

    def __setitem__(self, key, val):
        # queued now and added later in case there are errors
        # (we don't want partially-updated dicts!)
        filtersShouldAdd = set()
        filtersShouldRemove = set()
        for filterKey in self._filtered:
            filterFn, childDict = self._filtered[filterKey]
            shouldBeInFilter = filterFn(key, val)
            if shouldBeInFilter:
                filtersShouldAdd.add(filterKey)
            elif key in childDict:
                filtersShouldRemove.add(filterKey)
        
        # whew! now we can update
        super().__setitem__(key, val)
        for filterKey in filtersShouldAdd:
            filterFn, childDict = self._filtered[filterKey]
            childDict._self__setitem__(key, val)
        for filterKey in filtersShouldRemove:
            filterFn, childDict = self._filtered[filterKey]
            childDict._self__delitem__(key)

    def __delitem__(self, key):
        super().__delitem__(key)
        for filterKey in self._filtered:
            filterFn, childDict = self._filtered[filterKey]
            if key in childDict:
                childDict._self__delitem__(key)

    def update(self, sequence):
        # this ensures the sequence is a valid structure, and allows both the
        # loop and super().update() to consume the sequence iter
        allItemsToUpdate = dict(sequence)
        # will be queued now and added later once we know no errors pop up
        # (we don't want partially updated dicts!)
        filtersToAdd = dict()
        filtersToRemove = dict()
        for key in allItemsToUpdate:
            val = allItemsToUpdate[key]
            for filterKey in self._filtered:
                filterFn, childDict = self._filtered[filterKey]
                shouldBeInFilter = filterFn(key, val)
                if shouldBeInFilter:
                    if filtersToAdd.get(filterKey) is None:
                        filtersToAdd[filterKey] = dict()
                    filtersToAdd[filterKey][key] = val
                elif key in childDict:
                    if filtersToRemove.get(filterKey) is None:
                        filtersToRemove[filterKey] = set()
                    filtersToRemove[filterKey].add(key)
        
        # whew! now we can update
        super().update(allItemsToUpdate)
        for filterKey in filtersToAdd:
            filterFn, childDict = self._filtered[filterKey]
            childDict._self_update(filtersToAdd[filterKey])
        for filterKey in filtersToRemove:
            removeEach = filtersToRemove[filterKey]
            filterFn, childDict = self._filtered[filterKey]
            for key in removeEach:
                childDict._self__delitem__(key)

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
