from constants import INDENT

from lexer import Lexer
from parser import Parser
from engine import *


# decorator for all interpreter functions that call parser.inspect()
def parserInvoker(fn):
    def newFnForLine(self, string):
        lastMode = self._parseMode
        try:
            if '\n' in string:
                raise ValueError("(Internal error) Interpreter expected a single line to evaluate; given multiple lines (in a single string)")
            fn(self, string)
            # self._assertParseStackEmpty()
        finally:
            self._parseMode = lastMode
    return newFnForLine


class Interpreter:
    def __init__(self, outputFn):
        self._outputFn = outputFn
        self._solutions = list()

        self._parseStack = {
            "identifiers": [],
            "numbers": [],
            "values": [],
            "operations": [],
        }
        self._parseResult = None

        self._lexer = Lexer()
        self._parser = Parser()
        self._engine = Engine()
        self._bindToParser()

    @parserInvoker
    # treats the string as user input
    def executeLine(self, string):
        try:
            tokens = self._lexer.process(string)
            self._parser.inspect(tokens, string)
        except (Parser.ParseError, Parser.EOLError) as e:
            self._outputFn(e)
        except NotImplementedError as e:
            self._print(type(e).__name__ + ':', e)
        except Exception as e:
            self._outputFn("{}(Interpreter -- Unhandled error) {}: {}".format(INDENT, type(e).__name__, e))

    @parserInvoker
    # helper that can turn strings into engine-useable data structures
    def evaluateLine(self, string):
        tokens = self._lexer.process(string)
        self._parser.inspect(tokens, string)

    def _bindToParser(self):
        def pushStack(key, *args):
            return self._parseStack[key].append(*args)
        def popStack(key, *args):
            return self._parseStack[key].pop(*args)
        
        def onStart(tokens, branch):
            if branch == "re EOL":
                pass
            elif branch == "al EOL":
                pass
            elif branch == "ex EOL":
                pass
            elif branch == "co EOL":
                pass
            elif branch == "EOL":
                pass
        self._parser.onStart(onStart)

        def onFullIdentifier(tokens, branch):
            if branch == "id":
                tokenStrs = map(lambda token: str(token), tokens)
                fullId = ''.join(tokenStrs)
                pushStack("identifiers", fullId)
        self._parser.onFullIdentifier(onFullIdentifier)

        def onNumber(tokens, branch):
            if branch == "NU" or branch == "EN":
                numberStr = str(tokens[0])
                pushStack("numbers", numberStr)
        self._parser.onNumber(onNumber)

        def onValue(tokens, branch):
            if branch == "fu":
                idStr = popStack("identifiers")
                value = IdentifierValue(idStr)
                pushStack("values", value)
            if branch == "nu":
                numberStr = popStack("numbers")
                value = NumericValue(numberStr)
                pushStack("values", value)
        self._parser.onValue(onValue)

        def onExpression(tokens, branch):
            self._throwBranchNotImplemented("expressions")
        self._parser.onExpression(onExpression)

        def onOperationANY(tokens, branch):
            binaryOpBranches = [
                "opm opl opl",
                "oph opm opm",
                "opx oph oph",
            ]
            if branch in binaryOpBranches:
                operatorStr = popStack("operations")
                rightExpr = popStack("expressions")
                leftExpr = popStack("expressions")
                newExpr = Expression(leftExpr, operatorStr, rightExpr)
                pushStack("expressions", newExpr)
            elif branch == "DA opx":
                expr = popStack("expressions")
                newExpr = NegativeExpression(expr)
                pushStack("expressions", newExpr)
        self._parser.onOperationLow(onOperationANY)
        self._parser.onOperationMid(onOperationANY)
        self._parser.onOperationHigh(onOperationANY)
        self._parser.onOperationMax(onOperationANY)

        def onOperatorANY(tokens, branch):
            operatorStr = tokens[0]
            pushStack("operations", operatorStr)
        self._parser.onOperatorLow(onOperatorANY)
        self._parser.onOperatorMid(onOperatorANY)
        self._parser.onOperatorHigh(onOperatorANY)

        # TODO: finish these features
        def onUnit(tokens, branch):
            self._throwBranchNotImplemented("units")
        self._parser.onUnit(onUnit)

        def onAlias(tokens, branch):
            self._throwBranchNotImplemented("aliases")
        self._parser.onAlias(onAlias)

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
        self._outputFn(INDENT, *args)

    def _assertParseStackEmpty(self):
        badStackNames = []
        for stackName in self._parseStack:
            stack = self._parseStack[stackName]
            if len(stack) > 0:
                badStackNames.append(stackName)
        if len(badStackNames) > 0:
            raise RuntimeError("(Internal error) Interpreter did not use all items in the parse stack")

    def _throwBranchNotImplemented(self, featureNamePlural):
        raise NotImplementedError("SolverPro cannot process {} (yet)".format(featureNamePlural))
