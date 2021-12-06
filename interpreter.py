from constants import INDENT

from lexer import Lexer
from parser import Parser
from engine import *
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
                result = self._engine.substitute(expr)
                self._print(str(result))
            
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
        def pushStack(key, *pushArgs):
            return self._parseStack[key].append(*pushArgs)
        def popStack(key, *popArgs):
            return self._parseStack[key].pop(*popArgs)
        
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
                value = IdentifierValue(fullId)
            pushStack("identifiers", value)
        self._parser.onFullIdentifier(onFullIdentifier)

        def onIdentifiers(tokens, branch):
            if branch == "fu":
                identifier = popStack("identifiers")
                identifiers = [identifier]
            elif branch == "fu CO ids":
                identifiers = popStack("identifiers")
                nextIdentifier = popStack("identifiers")
                identifiers.append(nextIdentifier)
            pushStack("identifiers", identifiers)
        self._parser.onIdentifiers(onIdentifiers)

        def onNumber(tokens, branch):
            if branch == "NU" or branch == "EN":
                numberStr = str(tokens[0])
                value = NumericValue(numberStr)
            pushStack("numbers", value)
        self._parser.onNumber(onNumber)

        def onValue(tokens, branch):
            if branch == "fu":
                value = popStack("identifiers")
            if branch == "nu":
                value = popStack("numbers")
            pushStack("values", value)
        self._parser.onValue(onValue)

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
            elif branch == "DA opx":
                expr = popStack("expressions")
                newExpr = NegativeExpression(expr)
            else:
                # nothing to do for carry-over branches
                return
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

        def onEvaluation(tokens, branch):
            if branch == "va":
                expr = popStack("values")
            elif branch == "PAO ex PAC":
                # no need to process;
                # the expression is already in the right place
                return
            pushStack("expressions", expr)
        self._parser.onEvaluation(onEvaluation)

        def onLeftAliasANY(tokens, branch):
            if branch == "fu":
                identifier = popStack("identifiers")
                identifiers = (identifier,)
                args = None
            elif branch == "BRO ids BRC":
                identifiers = popStack("identifiers")
                args = None
            else:
                self._throwBranchNotImplemented("alias names".format(branch))
            pushStack("aliasNames", identifiers)
            pushStack("templateArgs", args)
        self._parser.onLeftAlias(onLeftAliasANY)
        self._parser.onLeftAliasTemp(onLeftAliasANY)

        def onRightAlias(tokens, branch):
            if branch == "ex":
                expr = popStack("expressions")
                rightHand = expr
            else:
                self._throwBranchNotImplemented("alias expressions".format(branch))
            pushStack("aliasRightHands", rightHand)
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
        if type(e) in parserErrors:
            self._outputFn(e)
            return True

        if type(e) is NotImplementedError:
            self._print(type(e).__name__ + ':', e)
            return True
        
        self._print("(Internal error) {}: {}".format(type(e).__name__, e))
        return False
