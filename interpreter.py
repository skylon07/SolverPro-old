import sympy

from constants import INDENT
from errors import *

from lexer import Lexer
from solverparser import Parser
from structures import *


class Interpreter:
    def __init__(self, outputFn):
        self._outputFn = outputFn
        self._solutions = list()

        self._parser = InterpreterParser()
        self._initializeSympy()
        self._initializeBuiltins()

    # this helps cut out the initial lag Sympy seems to have...
    def _initializeSympy(self):
        pass # TODO


    # TODO: error when trying to redefine builtins
    def _initializeBuiltins(self):
        pass

    # treats the string as user input
    # TODO: change paradigm so templates are evaluated before expressions are created
    def executeLine(self, string):
        try:
            result = self._parser.evaluateLine(string)
            if result["type"] == "relation":
                self._throwBranchNotImplemented("command executions")
            
            elif result["type"] == "alias":
                self._throwBranchNotImplemented("command executions")
            
            elif result["type"] == "expression":
                self._throwBranchNotImplemented("command executions")
            
            elif result["type"] == "command":
                self._throwBranchNotImplemented("command executions")
            
            elif result["type"] == "empty":
                pass # nuthin to do
            
            else:
                raise TypeError("Interpreter encountered an unexpected result type")
        except Exception as e:
            self._handleError(e)

    # I/O helper functions
    def _print(self, *args):
        args = [str(arg) for arg in args]
        args[0] = INDENT + args[0]
        self._outputFn(*args)

    def _handleError(self, e):
        parserErrors = (
            Parser.ParseError,
            Parser.EOLError,
        )
        if isinstance(e, parserErrors) or isinstance(e, InterpreterError):
            self._outputFn(e)
            return
        
        self._print("(Internal error) {}: {}".format(type(e).__name__, e))


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

            tokens = self._lexer.processGen(string)
            tokensNoComments = tuple(
                token for token in tokens
                if token.type != Lexer.types["COMMENT"]
            )
            self._parser.inspect(tokensNoComments, string)
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
            # TODO: convert all parse results to actual types/classes
            if branch == "relation EOL":
                self._parseResult = {
                    "type": "relation",
                    "rightHandExpr": popStack("expressions"),
                    "leftHandExpr": popStack("expressions"),
                }
            elif branch == "alias EOL":
                self._parseResult = {
                    "type": "alias",
                    "aliasNames": popStack("aliasNames"),
                    "templateParams": popStack("templateParams"),
                    "rightHand": popStack("aliasRightHands"),
                }
            elif branch == "expression EOL":
                self._parseResult = {
                    "type": "expression",
                    "expression": popStack("expressions"),
                }
            elif branch == "command EOL":
                self._parseResult = {
                    "type": "command",
                }
            elif branch == "EOL":
                self._parseResult = {
                    "type": "empty",
                }
        self._parser.onStart(onStart)

        def onNumber(tokens, branch):
            if branch == "NUMBER" or branch == "ENUMBER":
                numberStr = str(tokens[0])
                value = NumberRepresentation(numberStr)
            piece = StackPieceTracer(value, tokens)
            pushStack("numbers", piece)
        self._parser.onNumber(onNumber)

        def onFullIdentifier(tokens, branch):
            if branch == "identifier":
                tokenStrs = (str(token) for token in tokens)
                fullId = ''.join(tokenStrs)
                value = IdentifierRepresentation(fullId)
            piece = StackPieceTracer(value, tokens)
            piece.trace(TRACE_TYPES["IDENTIFIER"])
            pushStack("identifiers", piece)
        self._parser.onFullIdentifier(onFullIdentifier)

        def onIdentifiers(tokens, branch):
            if branch == "fullidentifier":
                piece = popStack("identifiers")
                identifier = piece.obj
                identifiers = [identifier]
                piece.update(identifiers, tokens, [])
            elif branch == "fullidentifier COMMA identifiers":
                piece = popStack("identifiers")
                identifiers = piece.obj
                nextPiece = popStack("identifiers")
                nextIdentifier = nextPiece.obj
                identifiers.insert(0, nextIdentifier)
                piece.update(identifiers, tokens, nextPiece.traces)
            pushStack("identifiers", piece)
        self._parser.onIdentifiers(onIdentifiers)

        def onValue(tokens, branch):
            if branch == "fullidentifier":
                piece = popStack("identifiers")
                idRep = piece.obj
                varRep = VariableRepresentation(idRep)
                piece.update(varRep, tokens, [])
            elif branch == "number":
                piece = popStack("numbers")
            elif branch == "fullidentifier PAREN_OPEN expressions PAREN_CLOSE":
                paramsPiece = popStack("expressions")
                namePiece = popStack("identifiers")
                exprParams = paramsPiece.obj
                nameId = namePiece.obj
                templateResult = TemplateCallRepresentation(nameId, exprParams)
                piece = paramsPiece.update(templateResult, tokens, namePiece.traces)
                piece.trace(TRACE_TYPES["TEMPLATE_CALL"])
            elif branch == "fullidentifier PAREN_OPEN PAREN_CLOSE":
                namePiece = popStack("identifiers")
                exprParams = []
                nameId = namePiece.obj
                templateResult = TemplateCallRepresentation(nameId, exprParams)
                piece = namePiece.update(templateResult, tokens, [])
                piece.trace(TRACE_TYPES["TEMPLATE_CALL"])
            piece.trace(TRACE_TYPES["VALUE"])
            pushStack("values", piece)
        self._parser.onValue(onValue)

        # (specifically binary operators)
        def onOperatorANY(tokens, branch):
            if branch == "PLUS":
                operatorFn = lambda a, b: a + b
                operatorStr = '+'
            elif branch == "DASH":
                operatorFn = lambda a, b: a - b
                operatorStr = '-'
            elif branch == "STAR":
                operatorFn = lambda a, b: a * b
                operatorStr = '*'
            elif branch == "SLASH":
                operatorFn = lambda a, b: a / b
                operatorStr = '/'
            elif branch == "CARROT":
                operatorFn = lambda a, b: a ** b
                operatorStr = '^'
            operatorRep = OperatorRepresentation(operatorFn, operatorStr)
            piece = StackPieceTracer(operatorRep, tokens)
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
                    operRep = opList.pop(0)
                    rightExpr = opList.pop(0)
                except IndexError as e:
                    if "pop from empty" in str(e):
                        raise IndexError("Interpreter -> evaluateOperationList(opList) given bad opList")
                if not issubclass(rightExpr.cls, Expressable):
                    raise RuntimeError("tried to evaluate operation list with a non-expressable (right-hand)")
                leftRight = (leftExpr, rightExpr)
                newExpr = ExpressionRepresentation(operRep, leftRight)
                opList.insert(0, newExpr)
            resultExpr = opList[0]
            return resultExpr
        
        def onOperationANY(tokens, branch):
            isOperatorBranch = None
            isChainBranch = None
            if branch in ("operationmid operatorlow operationlow", "operationmid"):
                stackName = "operationslow"
                nextStack = "operationsmid"
                isChainBranch = branch == "operationmid"
                isOperatorBranch = not isChainBranch
            elif branch in ("operationhigh operatormid operationmid", "operationhigh"):
                stackName = "operationsmid"
                nextStack = "operationshigh"
                isChainBranch = branch == "operationhigh"
                isOperatorBranch = not isChainBranch
            elif branch in ("operationmax operatorhigh operationhigh", "operationmax"):
                stackName = "operationshigh"
                nextStack = "operationsmax"
                isChainBranch = branch == "operationmax"
                isOperatorBranch = not isChainBranch
            elif branch in ("DASH operationmax", "evaluation"):
                stackName = "operationsmax"
                isChainBranch = branch == "evaluation"
                isOperatorBranch = not isChainBranch
            
            # operation-max production
            if branch == "DASH operationmax":
                opListPiece = popStack("operationsmax")
                opList = opListPiece.obj
                expr = evaluateOperationList(opList)
                negOperRep = OperatorRepresentation(lambda x: -x, "-")
                newExpr = ExpressionRepresentation(negOperRep, [expr])
                newOpList = [newExpr]
                piece = opListPiece.update(newOpList, tokens, [])
            # operation-max production
            elif branch == "evaluation":
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
                operRep = operPiece.obj
                nextOpList = nextOpListPiece.obj
                nextExpr = evaluateOperationList(nextOpList)
                # inserted on left to later be evaluated left-to-right
                # (parser productions return right-to-left)
                opList.insert(0, operRep)
                opList.insert(0, nextExpr)
                piece = opListPiece.update(opList, tokens, nextOpListPiece.traces)
            # all other productions that just chain precedence
            elif isChainBranch:
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
                "PAREN_OPEN expression PAREN_CLOSE",
                "BRACKET_OPEN expression BRACKET_CLOSE",
            )
            if branch == "value":
                piece = popStack("values")
                val = piece.obj
                noOper = OperatorRepresentation(lambda x: x, "$")
                expr = ExpressionRepresentation(noOper, [val])
                piece.update(expr, tokens, [])
            elif branch in exprBranches:
                # no need to process;
                # the expression is already in the right place
                return
            pushStack("expressions", piece)
        self._parser.onEvaluation(onEvaluation)

        def onLeftAliasANY(tokens, branch):
            isTemplateBranch = branch in ("fullidentifier PAREN_OPEN PAREN_CLOSE", "fullidentifier PAREN_OPEN identifiers PAREN_CLOSE")
            if branch == "fullidentifier":
                idPiece = popStack("identifiers")
                identifier = idPiece.obj
                identifiers = (identifier,)
                idsPiece = idPiece.update(identifiers, tokens, [])
                paramsPiece = StackPieceTracer(None, [])
            elif branch == "BRACKET_OPEN identifiers BRACKET_CLOSE":
                idsPiece = popStack("identifiers")
                paramsPiece = StackPieceTracer(None, [])
            elif isTemplateBranch:
                if branch == "fullidentifier PAREN_OPEN identifierss PAREN_CLOSE":
                    paramsPiece = popStack("identifiers")
                    if type(paramsPiece.obj) is not list:
                        raise TypeError("Interpreter -> onLeftAliasTemplate() got arguments from stack that were not in list form")
                elif branch == "fullidentifier PAREN_OPEN PAREN_CLOSE":
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
            if branch == "expression":
                rightHandPiece = popStack("expressions")
            elif branch == "BRACKET_OPEN expressions BRACKET_CLOSE":
                self._throwBranchNotImplemented("expression lists")
            else:
                self._throwBranchNotImplemented("alias expressions ({})".format(branch))
            pushStack("aliasRightHands", rightHandPiece)
        self._parser.onRightAlias(onRightAlias)

        def onRightAliasTemp(tokens, branch):
            if branch == "expression":
                piece = popStack("expressions")
            elif branch == "relation":
                self._throwBranchNotImplemented("relations on right-side of alias templates")
            elif branch == "command":
                self._throwBranchNotImplemented("commands on right-side of alias templates")
            elif branch == "objectdeclaration":
                self._throwBranchNotImplemented("objects on right-side of alias templates")
            pushStack("aliasRightHands", piece)
        self._parser.onRightAliasTemp(onRightAliasTemp)

        def onExpressions(tokens, branch):
            if branch == "expression":
                piece = popStack("expressions")
                expr = piece.obj
                exprs = [expr]
                piece.update(exprs, tokens, [])
            elif branch == "expression COMMA expressions":
                piece = popStack("expressions")
                exprs = piece.obj
                nextPiece = popStack("expressions")
                nextExpr = nextPiece.obj
                exprs.insert(0, nextExpr)
                piece.update(exprs, tokens, nextPiece.traces)
            pushStack("expressions", piece)
        self._parser.onExpressions(onExpressions)

        # TODO: finish these features
        def onUnit(tokens, branch):
            self._throwBranchNotImplemented("units")
        self._parser.onUnit(onUnit)

        def onObjectDeclaration(tokens, branch):
            self._throwBranchNotImplemented("objects")
        self._parser.onObjectDeclaration(onObjectDeclaration)

        def onIdentifier(tokens, branch):
            if branch == "IDENTIFIER PERIOD identifier":
                self._throwBranchNotImplemented("identifiers")
        self._parser.onIdentifier(onIdentifier)

        def onCommand(tokens, branch):
            self._throwBranchNotImplemented("commands")
        self._parser.onCommand(onCommand)

    def _assertParseStackEmpty(self):
        badStackNames = []
        for stackName in self._stacks:
            stack = self._stacks[stackName]
            if len(stack) > 0:
                badStackNames.append(stackName)
        if len(badStackNames) > 0:
            raise RuntimeError("Interpreter did not use all items in the parse stack")

    def _throwBranchNotImplemented(self, featureNamePlural):
        raise InterpreterNotImplementedError(featureNamePlural)


# TODO: should probably replace with some kind of enum class... (static properties)
TRACE_TYPES = {
    "IDENTIFIER": "IDENTIFIER",
    "TEMPLATE_CALL": "TEMPLATE_CALL",
    "VALUE": "VALUE",
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


if __name__ == "__main__":
    interpreter = Interpreter(print)
