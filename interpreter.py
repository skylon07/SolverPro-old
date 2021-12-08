from abc import ABC, abstractmethod

from constants import INDENT
from errors import TracebackError

from lexer import Lexer
from parser import Parser
from engine import *


class InterpreterError(TracebackError, ABC):
    def __init__(self, badTraces):
        message = self._generateMessage(badTraces)
        badStarts = map(lambda trace: trace["start"], badTraces)
        badEnds = map(lambda trace: trace["end"], badTraces)
        super().__init__(message, badStarts, badEnds)

    @abstractmethod
    def _generateMessage(self, badTraces):
        return # message string


class InterpreterWarning(InterpreterError):
    def warn(self, outputFn):
        outputFn(self.message)

class UndefinedError(InterpreterError):
    def _generateMessage(self, badTraces):
        if len(badTraces) > 1:
            plural = True
        else:
            plural = False
        
        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "{}{}ndefined identifier{} {} given: {}".format(
            "An " if not plural else "",
            "u" if not plural else "U",
            "s" if plural else "",
            "were" if plural else "was",
            ','.join(badIdentifierStrs),
        )

# meant to be called, not raised
class UnusedArgumentsWarning(InterpreterWarning):
    def _generateMessage(self, badTraces):
        if len(badTraces) > 1:
            plural = True
        else:
            plural = False

        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "Variable{} {} {} not used in {} template definition".format(
            "s" if plural else "",
            ','.join(badIdentifierStrs),
            "were" if plural else "was",
            "their" if plural else "its",
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
                self._throwBranchNotImplemented("relation executions")
            
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
                    self._ensureDefined(rightHandPiece)
                    rightWithSubs = self._engine.substitute(rightHandPiece.obj)
                    self._engine.setAliases(idsPiece.obj, rightWithSubs)
            
            elif result["type"] == "expression":
                exprPiece = result["expression"]
                self._ensureDefined(exprPiece)
                subExpr = self._engine.substitute(exprPiece.obj)
                self._print(str(subExpr))
            
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

    # errors when identifiers are not defined
    def _ensureDefined(self, stackPiece):
        badTraces = []
        for trace in stackPiece.traces:
            if trace["type"] == TRACE_TYPES["UNDEF_ID"]:
                identifier = trace["obj"]
                if not self._engine.isDefined(identifier):
                    badTraces.append(trace)
        if len(badTraces) > 0:
            raise UndefinedError(badTraces)

    # warns when a template's arguments are not used
    def _ensureTemplateUses(self, argNamesPiece, rightHandPiece):
        badTraces = []
        for argTrace in argNamesPiece.traces:
            if argTrace["type"] == TRACE_TYPES["UNDEF_ID"]:
                inRightHand = False
                for rightHandTrace in rightHandPiece.traces:
                    if rightHandTrace["type"] == TRACE_TYPES["UNDEF_ID"]:
                        if argTrace["obj"] == rightHandTrace["obj"]:
                            inRightHand = True
                            break
                if not inRightHand:
                    badTraces.append(argTrace)
        if len(badTraces) > 0:
            warning = UnusedArgumentsWarning(badTraces)
            warning.warn(self._outputFn)

    def _bindToParser(self):
        def pushStack(key, stackPiece):
            if not isinstance(stackPiece, StackPieceTracer):
                raise TypeError("Interpreter pushed non-tracer type to stack")
            return self._parseStack[key].append(stackPiece)
        def popStack(key):
            return self._parseStack[key].pop()
        
        def onStart(tokens, branch):
            if branch == "re EOL":
                self._parseResult = {
                    "type": "relation",
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
            piece.trace(TRACE_TYPES["UNDEF_ID"])
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
                pass # TODO: evaluate the template
            elif branch == "fu PAO PAC":
                pass # TODO: evaluate the template
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
            # collapses operations left-to-right
            while len(opList) > 1:
                try:
                    leftExpr = opList.pop(0)
                    oper = opList.pop(0)
                    rightExpr = opList.pop(0)
                except IndexError as e:
                    if "pop from empty" in str(e):
                        raise IndexError("Interpreter -> evaluateOperationList(opList) given bad opList")
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

        def onRelation(tokens, branch):
            self._throwBranchNotImplemented("relations")
        self._parser.onRelation(onRelation)

        def onCommand(tokens, branch):
            self._throwBranchNotImplemented("commands")
        self._parser.onCommand(onCommand)

    def _print(self, *args):
        self._outputFn(INDENT, *args, sep='')

    def _assertParseStackEmpty(self):
        badStackNames = []
        for stackName in self._parseStack:
            stack = self._parseStack[stackName]
            if len(stack) > 0:
                badStackNames.append(stackName)
        if len(badStackNames) > 0:
            raise RuntimeError("Interpreter did not use all items in the parse stack")

    def _throwBranchNotImplemented(self, featureNamePlural):
        raise NotImplementedError("SolverPro cannot process {} (yet)".format(featureNamePlural))

    def _handleError(self, e):
        parserErrors = (
            Parser.ParseError,
            Parser.EOLError,
        )
        if type(e) in parserErrors or isinstance(e, InterpreterError):
            self._outputFn(e)
            return True

        if type(e) is NotImplementedError:
            self._print(type(e).__name__ + ':', e)
            return True
        
        self._print("(Internal error) {}: {}".format(type(e).__name__, e))
        return False


TRACE_TYPES = {
    "UNDEF_ID": "UNDEF_ID",
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
                raise ValueError("trace ids were not unique")
        self._traces[lowBound:upBound] = [trace]

if __name__ == "__main__":
    piece1 = StackPieceTracer(Identifier("a"), [Lexer.Token("a", "IDENTIFIER", 0)])
    piece2 = StackPieceTracer(Identifier("b"), [Lexer.Token("b", "IDENTIFIER", 0)])
    
    piece2.trace(TRACE_TYPES["UNDEF_ID"])
    piece1.trace(TRACE_TYPES["UNDEF_ID"])
    piece1.trace(TRACE_TYPES["UNDEF_ID"])
    piece2.trace(TRACE_TYPES["UNDEF_ID"])
    piece1.trace(TRACE_TYPES["UNDEF_ID"])
    piece2.trace(TRACE_TYPES["UNDEF_ID"])
    piece1.trace(TRACE_TYPES["UNDEF_ID"])
    piece1.trace(TRACE_TYPES["UNDEF_ID"])
    piece2.trace(TRACE_TYPES["UNDEF_ID"])
    piece2.trace(TRACE_TYPES["UNDEF_ID"])

    piece1.update((piece1.obj, piece2.obj), [], piece2.traces)
    