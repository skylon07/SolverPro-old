import sympy

from constants import INDENT
from errors import *

from lexer import Lexer
from solverparser import Parser
from algebramaster import AlgebraMaster, NotANumericException
from structures import *


class Interpreter:
    def __init__(self, outputFn):
        self._outputFn = outputFn
        
        self._parser = InterpreterParser()
        self._master = AlgebraMaster()
        self._fillMasterWithFakeData()
        self._initializeBuiltins()

    # TODO: error when trying to redefine builtins
    def _initializeBuiltins(self):
        pass

    def _fillMasterWithFakeData(self):
        self._master.define([Identifier('a')], SubSet({4}))
        self._master.define([Identifier('b')], SubSet({-5}))
        self._master.define([Identifier('c')], SubSet({sympy.Symbol('a') + sympy.Symbol('b')}))
        self._master.define([Identifier('x')], SubSet({4, -4}))
        self._master.define([Identifier('y')], SubSet({sympy.Symbol('c'), sympy.Symbol('x')}))
        

    # treats the string as user input
    # TODO: change paradigm so templates are evaluated before expressions are created
    def executeLine(self, string):
        try:
            # TODO: evaluate template calls
            result = self._parser.evaluateLine(string)
            
            if result["type"] == "relation":
                raise InterpreterNotImplementedError("relations")
            
            elif result["type"] == "alias":
                newIdsConstructor = Constructor(result['aliasNames'], "list")
                paramsConstructor = Constructor(result['templateParams'], "list")
                rightHandConstructor = Constructor(result['rightHand'], "either", forceList=True)

                isTemplateAlias = result['templateParams'].obj != None
                if not isTemplateAlias:
                    newIds = newIdsConstructor.construct()
                    values = SubSet(rightHandConstructor.construct())
                    try:
                        self._master.define(newIds, values)
                    except NotANumericException as err:
                        rightHandConstructor.failForUndefined()
                        raise err
                else:
                    raise InterpreterNotImplementedError("template aliases")
            
            elif result["type"] == "expression":
                exprConstructor = Constructor(result['expression'], "single")

                # TODO: ensure identifiers exist (as definitions or relations) before constructing
                expression = exprConstructor.construct()
                if isNumeric(expression):
                    self._print(expression)
                else:
                    subExprSet = self._master.substituteKnown(expression)
                    assert type(subExprSet) is SubSet, "substituteKnown() did not return a SubSet()"
                    for item in subExprSet:
                        self._print(item)
            
            elif result["type"] == "command":
                raise InterpreterNotImplementedError("commands")
            
            elif result["type"] == "empty":
                pass # nuthin to do
            
            else:
                raise TypeError("Interpreter encountered an unexpected result type")
        except Exception as e:
            self._handleError(e)

    # I/O helper functions
    def _print(self, *args):
        args = [self._formatForPrint(arg) for arg in args]
        args[0] = INDENT + args[0]
        self._outputFn(*args)

    def _formatForPrint(self, val):
        # floats are rounded on format because there's
        # no way to round them inside the sympy expression
        if type(val) in (float, sympy.Float):
            val = RoundedFloat.roundFloat(float(val), 12)
            if val == val // 1 and "e" not in str(val).lower():
                return str(int(val))
        elif type(val) in (int, sympy.Integer):
            floatValStr = str(float(val))
            if "e" in floatValStr.lower():
                return floatValStr
        return str(val)

    def _handleError(self, e):
        parserErrors = (
            Parser.ParseError,
            Parser.EOLError,
        )
        if isinstance(e, parserErrors) or isinstance(e, InterpreterException):
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
        self._stacks = InterpreterParser._Stacks()
        
    class _Stacks:
        def __init__(self):
            self.identifiers = []
            self.numbers = []
            self.values = []
            
            self.operations = []
            self.expressions = []
            # these stacks hold operation lists, not single values
            self.operationslow = []
            self.operationsmid = []
            self.operationshigh = []
            self.operationsmax = []

            self.aliasNames = []
            self.templateParams = []
            self.aliasRightHands = []
        
        def __iter__(self):
            return iter([
                self.identifiers,
                self.numbers,
                self.values,
                self.operations,
                self.expressions,
                self.operationslow,
                self.operationsmid,
                self.operationshigh,
                self.operationsmax,
                self.aliasNames,
                self.templateParams,
                self.aliasRightHands,
            ])

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
            assert len([badStack for badStack in self._stacks if len(badStack) > 0]) == 0, "Parser stack should be empty"
            return self.lastResult
        finally:
            self._resetStack()

    def _bindToParser(self):
        # this function is only run on initialization; these helper functions
        # aren't too inefficient
        def pushStack(stack, stackPiece):
            assert type(stackPiece) is StackPieceTracer, "Can only push stack piece tracers"
            return stack.append(stackPiece)
        def popStack(stack):
            return stack.pop()
        def throwBranchNotCaught(branch, onFnName):
            raise ValueError("Branch '{}' was not considered for '{}'".format(branch, onFnName))
        
        # each callback should generally be popping StackPieces from one stack,
        # potentially modifying their data, and pushing onto another stack
        def onStart(tokens, branch):
            # TODO: convert all parse results to actual types/classes
            if branch == "relation EOL":
                self._parseResult = {
                    "type": "relation",
                    "rightHandExpr": popStack(self._stacks.expressions),
                    "leftHandExpr": popStack(self._stacks.expressions),
                }
            elif branch == "alias EOL":
                self._parseResult = {
                    "type": "alias",
                    "aliasNames": popStack(self._stacks.aliasNames),
                    "templateParams": popStack(self._stacks.templateParams),
                    "rightHand": popStack(self._stacks.aliasRightHands),
                }
            elif branch == "expression EOL":
                self._parseResult = {
                    "type": "expression",
                    "expression": popStack(self._stacks.expressions),
                }
            elif branch == "command EOL":
                self._parseResult = {
                    "type": "command",
                }
            elif branch == "EOL":
                self._parseResult = {
                    "type": "empty",
                }
            else:
                throwBranchNotCaught(branch, "onStart")
        self._parser.onStart(onStart)

        def onNumber(tokens, branch):
            if branch == "NUMBER" or branch == "ENUMBER":
                numberStr = str(tokens[0])
                value = NumberRepresentation(numberStr)
            else:
                throwBranchNotCaught(branch, "onNumber")
            piece = StackPieceTracer(value, tokens)
            pushStack(self._stacks.numbers, piece)
        self._parser.onNumber(onNumber)

        def onFullIdentifier(tokens, branch):
            if branch == "identifier":
                tokenStrs = (str(token) for token in tokens)
                fullId = ''.join(tokenStrs)
                value = IdentifierRepresentation(fullId)
            else:
                throwBranchNotCaught(branch, "onFullIdentifier")
            piece = StackPieceTracer(value, tokens)
            piece.trace(StackPieceTracer.types.IDENTIFIER)
            pushStack(self._stacks.identifiers, piece)
        self._parser.onFullIdentifier(onFullIdentifier)

        def onIdentifiers(tokens, branch):
            if branch == "fullidentifier":
                piece = popStack(self._stacks.identifiers)
                identifier = piece.obj
                identifiers = [identifier]
                piece.update(identifiers, tokens, [])
            elif branch == "fullidentifier COMMA identifiers":
                piece = popStack(self._stacks.identifiers)
                identifiers = piece.obj
                nextPiece = popStack(self._stacks.identifiers)
                nextIdentifier = nextPiece.obj
                identifiers.insert(0, nextIdentifier)
                piece.update(identifiers, tokens, nextPiece.traces)
            else:
                throwBranchNotCaught(branch, "onIdentifiers")
            pushStack(self._stacks.identifiers, piece)
        self._parser.onIdentifiers(onIdentifiers)

        def onValue(tokens, branch):
            if branch == "fullidentifier":
                piece = popStack(self._stacks.identifiers)
                idRep = piece.obj
                varRep = VariableRepresentation(idRep)
                piece.update(varRep, tokens, [])
            elif branch == "number":
                piece = popStack(self._stacks.numbers)
            # TODO: PARSER NO LONGER SUPPORTS TEMPLATE CALLS
            # elif branch == "fullidentifier PAREN_OPEN expressions PAREN_CLOSE":
            #     paramsPiece = popStack(self._stacks.expressions)
            #     namePiece = popStack(self._stacks.identifiers)
            #     exprParams = paramsPiece.obj
            #     nameId = namePiece.obj
            #     templateResult = TemplateCallRepresentation(nameId, exprParams)
            #     piece = paramsPiece.update(templateResult, tokens, namePiece.traces)
            #     piece.trace(StackPieceTracer.types.TEMPLATE_CALL)
            # elif branch == "fullidentifier PAREN_OPEN PAREN_CLOSE":
            #     namePiece = popStack(self._stacks.identifiers)
            #     exprParams = []
            #     nameId = namePiece.obj
            #     templateResult = TemplateCallRepresentation(nameId, exprParams)
            #     piece = namePiece.update(templateResult, tokens, [])
            #     piece.trace(StackPieceTracer.types.TEMPLATE_CALL)
            else:
                throwBranchNotCaught(branch, "onValue")
            piece.trace(StackPieceTracer.types.VALUE)
            pushStack(self._stacks.values, piece)
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
            else:
                throwBranchNotCaught(branch, "onOperatorANY")
            operatorRep = OperatorRepresentation(operatorFn, operatorStr)
            piece = StackPieceTracer(operatorRep, tokens)
            # can't use piece.trace() here without reviewing onOperation;
            # that callback ignores this piece's traces
            pushStack(self._stacks.operations, piece)
        self._parser.onOperatorLow(onOperatorANY)
        self._parser.onOperatorMid(onOperatorANY)
        self._parser.onOperatorHigh(onOperatorANY)

        # specifically binary operations
        def evaluateOperationList(opList):
            assert type(opList) is list, "Can only evaluate lists of expressions/operators"
            
            # collapses operations left-to-right
            while len(opList) > 1:
                try:
                    leftExpr = opList.pop(0)
                    operRep = opList.pop(0)
                    rightExpr = opList.pop(0)
                    assert type(leftExpr) is ExpressionRepresentation, "'opList' was not in a valid format"
                    assert type(operRep) is OperatorRepresentation, "'opList' was not in a valid format"
                    assert type(rightExpr) is ExpressionRepresentation, "'opList' was not in a valid format"
                except IndexError as e:
                    if "pop from empty" in str(e):
                        raise IndexError("Interpreter -> evaluateOperationList(opList); opList did not follow [expr, oper, expr, ..., oper, expr] pattern")
                leftRight = (leftExpr, rightExpr)
                newExpr = ExpressionRepresentation(operRep, leftRight)
                opList.insert(0, newExpr)
            resultExpr = opList[0]
            return resultExpr
        
        def onOperationANY(tokens, branch):
            isOperatorBranch = None
            isChainBranch = None
            if branch in ("operationmid operatorlow operationlow", "operationmid"):
                stack = self._stacks.operationslow
                nextStack = self._stacks.operationsmid
                isChainBranch = branch == "operationmid"
                isOperatorBranch = not isChainBranch
            elif branch in ("operationhigh operatormid operationmid", "operationhigh"):
                stack = self._stacks.operationsmid
                nextStack = self._stacks.operationshigh
                isChainBranch = branch == "operationhigh"
                isOperatorBranch = not isChainBranch
            elif branch in ("operationmax operatorhigh operationhigh", "operationmax"):
                stack = self._stacks.operationshigh
                nextStack = self._stacks.operationsmax
                isChainBranch = branch == "operationmax"
                isOperatorBranch = not isChainBranch
            elif branch in ("DASH operationmax", "evaluation"):
                stack = self._stacks.operationsmax
                nextStack = self._stacks.expressions
                isChainBranch = branch == "evaluation"
                isOperatorBranch = not isChainBranch
            else:
                throwBranchNotCaught(branch, "onOperationANY")
            
            assert isOperatorBranch is not None, "'isOperatorBranch' must be set"
            assert isChainBranch is not None, "'isChainBranch' must be set"
            
            # operation-max production
            if branch == "DASH operationmax":
                opListPiece = popStack(stack)
                opList = opListPiece.obj
                expr = evaluateOperationList(opList)
                negOperRep = OperatorRepresentation(lambda x: -x, "-")
                newExpr = ExpressionRepresentation(negOperRep, [expr])
                newOpList = [newExpr]
                piece = opListPiece.update(newOpList, tokens, [])
            # operation-max production
            elif branch == "evaluation":
                piece = popStack(nextStack)
                expr = piece.obj
                exprs = [expr]
                # lower precedence productions expect the stack to contain lists
                piece.update(exprs, tokens, [])
            # all other productions that use an operator
            elif isOperatorBranch:
                opListPiece = popStack(stack)
                operPiece = popStack(self._stacks.operations)
                nextOpListPiece = popStack(nextStack)
                # TODO: make separate opList datatype, and allow reversing evaluation
                # for exponents (should be evaluated right-left, but currently evaluated
                # left-right; everything else should be left-right)
                opList = opListPiece.obj
                operRep = operPiece.obj
                nextOpList = nextOpListPiece.obj
                nextExpr = evaluateOperationList(nextOpList)
                # inserted on left to reverse order to match expectations
                # (parser productions return right-to-left)
                # ex. parser "1 + 2 + 3" > 3, 2, 1; reversing gives expected 1, 2, 3
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
            else:
                throwBranchNotCaught(branch, "onOperationANY")
            pushStack(stack, piece)
        self._parser.onOperationLow(onOperationANY)
        self._parser.onOperationMid(onOperationANY)
        self._parser.onOperationHigh(onOperationANY)
        self._parser.onOperationMax(onOperationANY)

        def onExpression(tokens, branch):
            opListPiece = popStack(self._stacks.operationslow)
            opList = opListPiece.obj
            expr = evaluateOperationList(opList)
            piece = opListPiece.update(expr, tokens, [])
            pushStack(self._stacks.expressions, piece)
        self._parser.onExpression(onExpression)

        def onEvaluation(tokens, branch):
            exprBranches = (
                "PAREN_OPEN expression PAREN_CLOSE",
                "BRACKET_OPEN expression BRACKET_CLOSE",
            )
            if branch == "value":
                piece = popStack(self._stacks.values)
                val = piece.obj
                # needed since opLists must contain ExpressionRepresentations
                expr = ExpressionRepresentation.convert(val)
                piece.update(expr, tokens, [])
            elif branch in exprBranches:
                # no need to process;
                # the expression is already in the right place
                assert type(self._stacks.expressions[-1].obj) is ExpressionRepresentation, "Expression stack has non-expression, and tried to pass"
                return
            else:
                throwBranchNotCaught(branch, "onEvaluation")
            pushStack(self._stacks.expressions, piece)
        self._parser.onEvaluation(onEvaluation)

        def onLeftAliasANY(tokens, branch):
            isTemplateBranch = branch in ("fullidentifier PAREN_OPEN PAREN_CLOSE", "fullidentifier PAREN_OPEN identifiers PAREN_CLOSE")
            if branch == "fullidentifier":
                idPiece = popStack(self._stacks.identifiers)
                identifier = idPiece.obj
                identifiers = (identifier,)
                idsPiece = idPiece.update(identifiers, tokens, [])
                paramsPiece = StackPieceTracer(None, [])
            elif branch == "BRACKET_OPEN identifiers BRACKET_CLOSE":
                idsPiece = popStack(self._stacks.identifiers)
                paramsPiece = StackPieceTracer(None, [])
            elif isTemplateBranch:
                if branch == "fullidentifier PAREN_OPEN identifiers PAREN_CLOSE":
                    paramsPiece = popStack(self._stacks.identifiers)
                    assert type(paramsPiece.obj) is list, "Identifiers should come in a list"
                elif branch == "fullidentifier PAREN_OPEN PAREN_CLOSE":
                    paramsPiece = StackPieceTracer([], [])
                else:
                    throwBranchNotCaught(branch, "onLeftAliasANY")
                idPiece = popStack(self._stacks.identifiers)
                identifier = idPiece.obj
                identifiers = (identifier,)
                idsPiece = idPiece.update(identifiers, tokens, [])
            else:
                throwBranchNotCaught(branch, "onLeftAliasANY")
            pushStack(self._stacks.aliasNames, idsPiece)
            pushStack(self._stacks.templateParams, paramsPiece)
        self._parser.onLeftAlias(onLeftAliasANY)
        self._parser.onLeftAliasTemp(onLeftAliasANY)

        def onRightAlias(tokens, branch):
            if branch == "expression":
                rightHandPiece = popStack(self._stacks.expressions)
            elif branch == "BRACKET_OPEN expressions BRACKET_CLOSE":
                self._throwBranchNotImplemented("expression lists")
            else:
                self._throwBranchNotImplemented("alias expressions ({})".format(branch))
                throwBranchNotCaught(branch, "onRightAlias")
            pushStack(self._stacks.aliasRightHands, rightHandPiece)
        self._parser.onRightAlias(onRightAlias)

        def onRightAliasTemp(tokens, branch):
            if branch == "expression":
                piece = popStack(self._stacks.expressions)
            elif branch == "relation":
                self._throwBranchNotImplemented("relations on right-side of alias templates")
            elif branch == "command":
                self._throwBranchNotImplemented("commands on right-side of alias templates")
            elif branch == "objectdeclaration":
                self._throwBranchNotImplemented("objects on right-side of alias templates")
            else:
                throwBranchNotCaught(branch, "onRightAliasTemp")
            pushStack(self._stacks.aliasRightHands, piece)
        self._parser.onRightAliasTemp(onRightAliasTemp)

        def onExpressions(tokens, branch):
            if branch == "expression":
                piece = popStack(self._stacks.expressions)
                expr = piece.obj
                exprs = [expr]
                piece.update(exprs, tokens, [])
            elif branch == "expression COMMA expressions":
                piece = popStack(self._stacks.expressions)
                exprs = piece.obj
                nextPiece = popStack(self._stacks.expressions)
                nextExpr = nextPiece.obj
                exprs.insert(0, nextExpr)
                piece.update(exprs, tokens, nextPiece.traces)
            else:
                throwBranchNotCaught(branch, "onExpressions")
            pushStack(self._stacks.expressions, piece)
        self._parser.onExpressions(onExpressions)

        # TODO: finish these features
        def onUnit(tokens, branch):
            self._throwBranchNotImplemented("units")
            throwBranchNotCaught(branch, "onUnit")
        self._parser.onUnit(onUnit)

        def onObjectDeclaration(tokens, branch):
            self._throwBranchNotImplemented("objects")
            throwBranchNotCaught(branch, "onObjectDeclaration")
        self._parser.onObjectDeclaration(onObjectDeclaration)

        def onIdentifier(tokens, branch):
            # TODO: not sure if this function is needed...
            #       but don't throw out until objects are implemented
            if branch == "IDENTIFIER PERIOD identifier":
                self._throwBranchNotImplemented("identifiers")
            elif branch == "IDENTIFIER":
                pass
            else:
                throwBranchNotCaught(branch, "onIdentifier")
        self._parser.onIdentifier(onIdentifier)

        def onCommand(tokens, branch):
            self._throwBranchNotImplemented("commands")
            throwBranchNotCaught(branch, "onCommand")
        self._parser.onCommand(onCommand)

    def _throwBranchNotImplemented(self, featureNamePlural):
        raise InterpreterNotImplementedError(featureNamePlural)


# contains an element of the stack as well as some helpful metadata
class StackPieceTracer:
    class _Types:
        def __iter__(self):
            return iter({"IDENTIFIER", "TEMPLATE_CALL", "VALUE"})
        
        @property
        def IDENTIFIER(self):
            return "IDENTIFIER"

        @property
        def TEMPLATE_CALL(self):
            return "TEMPLATE_CALL"

        @property
        def VALUE(self):
            return "VALUE"
    
    types = _Types()

    __traceId_DO_NOT_MODIFY = 0
    @classmethod
    def _getUniqueTraceId(cls):
        # ...except for here, obviously
        traceId = cls.__traceId_DO_NOT_MODIFY
        cls.__traceId_DO_NOT_MODIFY += 1
        return traceId

    def __init__(self, obj, tokens):
        assert isinstance(obj, (type(None), Representation, tuple, list)), "Traced object must be a Representation or list/tuple of Representations"
        assert len([nonRep for nonRep in obj if not isinstance(nonRep, Representation)]) == 0 \
            if type(obj) in (tuple, list) else True, \
            "Tuple/list must only contain Representations"
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
        assert traceType in StackPieceTracer.types, "An invalid trace type was given"
        traceStart = self._tokens[0].placementStart
        traceEnd = self._tokens[-1].placementEnd
        trace = self.Trace(
            traceType,
            self._obj,
            self._tokens,
            traceStart,
            traceEnd,
        )
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
            if trace.id < bisectTrace.id:
                upBound = bisectIdx
            elif trace.id > bisectTrace.id:
                lowBound = bisectIdx + 1
            else:
                raise ValueError("trace ids were not unique (or a trace was re-added into the list)")
        self._traces[lowBound:upBound] = [trace]

    class Trace:
        def __init__(self, traceType, obj, tokens, start, end):
            self.id = StackPieceTracer._getUniqueTraceId()
            self.type = traceType
            self.obj = obj
            self.tokens = tokens
            self.start = start
            self.end = end


class Constructor:
    def __init__(self, stackPiece, mode, forceList=False):
        assert mode in ["list", "single", "either"], "Constructor mode must be 'list', 'single', or 'either'"
        self._piece = stackPiece
        self._mode = mode
        self._forceList = forceList

    def convertTemplateCalls(self, convertFn):
        assert isfunction(convertFn), "The convert function must be a function"
        convertedReps = [convertFn(item) for item in self._repList]
        return self._convertList(convertedReps)

    def construct(self):
        constructedVals = [rep.construct() for rep in self._repList]
        return self._convertList(constructedVals)

    # failer functions
    def failForUndefined(self, isDefinedFn):
        badTraces = []
        for idTrace in self._piece.traces:
            if idTrace.type == StackPieceTracer.types.IDENTIFIER:
                identifier = idTrace.obj.construct()
                isDefined = isDefinedFn(identifier)
                assert type(isDefined) is bool, "isDefinedFn() must return a boolean"
                if not isDefined:
                    badTraces.append(idTrace)
        self._checkFailerFailed(UndefinedIdentifierError, badTraces)

    def _checkFailerFailed(self, ErrType, badTraces):
        assert issubclass(ErrType, InterpreterTracebackException), "Failer functions should only work with traceback errors"
        if len(badTraces) > 0:
            raise ErrType(badTraces)

    def _convertList(self, repList):
        if self._mode == "single":
            assert self._isSingleRep, "Constructor mode mismatch: tracer contained a list"
            return repList[0] if not self._forceList else repList
        elif self._mode == "list":
            assert not self._isSingleRep, "Constructor mode mismatch: tracer contained a single Representation"
            return repList
        elif self._mode == "either":
            shouldBeSingle = not self._forceList and self._isSingleRep
            return repList[0] if shouldBeSingle else repList
        else:
            assert "this" == "impossible", "An unconsidered Constructor mode occurred"
    
    @property
    def _repList(self):
        repOrList = self._piece.obj
        if type(repOrList) in (tuple, list):
            return repOrList
        elif isinstance(repOrList, (type(None), Representation)):
            return [repOrList]
        else:
            assert "this" == "impossible", "Stack piece had an unconsidered value"

    @property
    def _isSingleRep(self):
        return isinstance(self._piece.obj, Representation)


if __name__ == "__main__":
    interpreter = Interpreter(print)
