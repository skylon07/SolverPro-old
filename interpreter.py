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
    def __generateMessage(self, badTraces):
        return # message string

class UndefinedError(InterpreterError):
    def _generateMessage(self, badTraces):
        if len(badTraces) > 1:
            plural = True
        else:
            plural = False
        
        badIdentifiers = map(lambda trace: str(trace["obj"]), badTraces)
        return "{}{}ndefined identifier{} {} given: {}".format(
            "An " if not plural else "",
            "u" if not plural else "U",
            "s" if plural else "",
            "were" if plural else "was",
            ','.join(badIdentifiers),
        )


class Interpreter:
    def __init__(self, outputFn):
        self._outputFn = outputFn
        self._solutions = list()

        self._parseStack = {
            "identifiers": [],
            "numbers": [],
            "values": [],
            "operations": [],
            "expressions": [],

            "aliasNames": [],
            "templateArgs": [],
            "aliasRightHands": [],
        }
        self._parseResult = None

        self._lexer = Lexer()
        self._parser = Parser()
        self._engine = Engine()
        self._bindToParser()

    # treats the string as user input
    def executeLine(self, string):
        try:
            result = self.evaluateLine(string)
            if result["type"] == "relation":
                self._throwBranchNotImplemented("relation executions")
            
            elif result["type"] == "alias":
                isTemplate = result["templateArgs"] is not None
                if isTemplate:
                    self._throwBranchNotImplemented("template alias executions")
                else:
                    identifiers = result["aliasNames"]
                    rawValue = result["rightHand"]
                    value = self._engine.substitute(rawValue)
                    self._engine.setAliases(identifiers, value)
            
            elif result["type"] == "expression":
                expr = result["expression"]
                subExpr = self._engine.substitute(expr)
                self._print(str(subExpr))
            
            elif result["type"] == "command":
                self._throwBranchNotImplemented("command executions")
            
            elif result["type"] == "empty":
                pass # nuthin to do
            
            else:
                raise TypeError("Interpreter encountered an unexpected result type")
        except Exception as e:
            self._handleError(e)

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

        def onFullIdentifier(tokens, branch):
            # TODO: check that the value is defined
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
                identifiers.append(nextIdentifier)
                piece.update(identifiers, tokens, nextPiece.traces)
            pushStack("identifiers", piece)
        self._parser.onIdentifiers(onIdentifiers)

        def onNumber(tokens, branch):
            if branch == "NU" or branch == "EN":
                numberStr = str(tokens[0])
                value = Numeric(numberStr)
            piece = StackPieceTracer(value, tokens)
            pushStack("numbers", piece)
        self._parser.onNumber(onNumber)

        def onValue(tokens, branch):
            if branch == "fu":
                piece = popStack("identifiers")
            if branch == "nu":
                piece = popStack("numbers")
            pushStack("values", piece)
        self._parser.onValue(onValue)

        def onOperationANY(tokens, branch):
            binaryOpBranches = [
                "opm opl opl",
                "oph opm opm",
                "opx oph oph",
            ]
            if branch in binaryOpBranches:
                opPiece = popStack("operations")
                rightPiece = popStack("expressions")
                leftPiece = popStack("expressions")
                newExpr = Expression(leftPiece.obj, opPiece.obj, rightPiece.obj)
                piece = leftPiece.update(newExpr, tokens, rightPiece.traces)
            elif branch == "DA opx":
                piece = popStack("expressions")
                expr = piece.obj
                newExpr = NegativeExpression(expr)
                piece.update(newExpr, tokens)
            else:
                # nothing to do for carry-over branches
                return
            pushStack("expressions", piece)
                
        self._parser.onOperationLow(onOperationANY)
        self._parser.onOperationMid(onOperationANY)
        self._parser.onOperationHigh(onOperationANY)
        self._parser.onOperationMax(onOperationANY)

        def onOperatorANY(tokens, branch):
            operatorStr = str(tokens[0])
            piece = StackPieceTracer(operatorStr, tokens)
            # can't use piece.trace() here without reviewing onOperation;
            # that callback ignores this piece's traces
            pushStack("operations", piece)
        self._parser.onOperatorLow(onOperatorANY)
        self._parser.onOperatorMid(onOperatorANY)
        self._parser.onOperatorHigh(onOperatorANY)

        def onEvaluation(tokens, branch):
            if branch == "va":
                piece = popStack("values")
            elif branch == "PAO ex PAC":
                # no need to process;
                # the expression is already in the right place
                return
            pushStack("expressions", piece)
        self._parser.onEvaluation(onEvaluation)

        def onLeftAliasANY(tokens, branch):
            if branch == "fu":
                idPiece = popStack("identifiers")
                identifier = idPiece.obj
                identifiers = (identifier,)
                idsPiece = idPiece.update(identifiers, tokens, [])
                argsPiece = StackPieceTracer(None, [])
            elif branch == "BRO ids BRC":
                idsPiece = popStack("identifiers")
                argsPiece = StackPieceTracer(None, [])
            else:
                self._throwBranchNotImplemented("alias names".format(branch))
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
            if False:
                pass
            else:
                self._throwBranchNotImplemented("alias template expressions".format(branch))
            TODO_somethingNeedsToGoHere = None
            pushStack("aliasRightHands", TODO_somethingNeedsToGoHere)
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
    "IDENTIFIER": "IDENTIFIER",
}

# contains an element of the stack as well as some helpful metadata
class StackPieceTracer:
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
            self._traces.append(trace)
        return self

    def trace(self, traceType):
        if traceType not in TRACE_TYPES:
            raise ValueError("Interpreter stack can only trace given a valid type")
        traceStart = self._tokens[0].placementStart
        traceEnd = self._tokens[-1].placementEnd
        trace = {
            "type": traceType,
            "obj": self._obj,
            "start": traceStart,
            "end": traceEnd,
        }
        self._traces.append(trace)

    @property
    def traces(self):
        return iter(self._traces)