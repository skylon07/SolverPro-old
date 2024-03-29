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
        self._database = InterpreterDatabase()
        self._initializeSympy()
        self._initializeBuiltins()

    # this helps cut out the initial lag Sympy seems to have...
    def _initializeSympy(self):
        a = sympy.Symbol('a')
        b = sympy.Symbol('b')
        return a + b * a ** b


    # TODO: error when trying to redefine builtins
    def _initializeBuiltins(self):
        self._database.setDefinition(Identifier('sqrt'), Template(
            [Identifier('x')],
            # TODO: shouldn't this really just be an Expression()?
            SqrtExpression(Variable(Identifier('x'))),
        ))

    # treats the string as user input
    def executeLine(self, string):
        try:
            result = self._parser.evaluateLine(string)
            if result["type"] == "relation":
                leftPiece = result["leftHandExpr"]
                rightPiece = result["rightHandExpr"]

                # unused _ensureIdentifiersExist_REP: relations can infer new values
                self._ensureIdentifiersAreExpressable_REP(leftPiece) and self._ensureIdentifiersAreExpressable_REP(rightPiece)
                self._ensureValidTemplateCallNames_REP(leftPiece) and self._ensureValidTemplateCallNames_REP(rightPiece)
                self._ensureValidNumTemplateParams_REP(leftPiece) and self._ensureValidNumTemplateParams_REP(rightPiece)
                # unused _ensureTemplateUses_REP: not defining a template
                
                leftExpr = self._evalRep(leftPiece.obj)
                rightExpr = self._evalRep(rightPiece.obj)
                
                # TODO: TEMP ensures

                leftExpr = self._evalTemplates(leftExpr)
                rightExpr = self._evalTemplates(rightExpr)
                relation = Relation(leftExpr, rightExpr)
                sympySolutions = relation.solveAll()
                solutions = {var: sympySolutions[var] for var in sympySolutions}
                self._database.defineRelation(relation, solutions)
                self._updateInferences()
            
            elif result["type"] == "alias":
                isTemplate = result["templateParams"].obj is not None
                if isTemplate:
                    raise InterpreterNotImplementedError("template definitions")

                    idsPiece = result["aliasNames"]
                    paramsPiece = result["templateParams"]
                    rightHandPiece = result["rightHand"]

                    # unused _ensureIdentifiersExist_REP: templates don't eval rightHand immediately
                    # unused _ensureIdentifiersAreExpressable_REP: templates don't eval rightHand immediately
                    # unused _ensureValidTemplateCallNames_REP: templates don't eval rightHand immediately
                    # unused _ensureValidNumTemplateParams_REP: templates don't eval rightHand immediately
                    self._ensureTemplateUses_REP(paramsPiece, rightHandPiece)
                    
                    # unused <all template assurances>: templates don't eval rightHand immediately

                    # only one identifier allowed by parser for template alias names
                    identifier = self._evalRep(idsPiece.obj[0])
                    params = tuple(self._evalRepIter(paramsPiece.obj))
                    rightHand = self._evalRep(rightHandPiece.obj)
                    template = Template(params, rightHand)
                    self._database.setDefinition(identifier, template)

                else:
                    idsPiece = result["aliasNames"]
                    rightHandPiece = result["rightHand"]

                    self._ensureIdentifiersExist_REP(rightHandPiece)
                    # things like 'alias := template' are valid,
                    # but 'alias := template + 4' are not
                    if not issubclass(rightHandPiece.obj.cls, Variable):
                        self._ensureIdentifiersAreExpressable_REP(rightHandPiece)
                    self._ensureValidTemplateCallNames_REP(rightHandPiece)
                    self._ensureValidNumTemplateParams_REP(rightHandPiece)
                    # unused _ensureTemplateUses_REP: not defining a template

                    idList = self._evalRepIter(idsPiece.obj)
                    rightHand = self._evalRep(rightHandPiece.obj)
                    
                    # TODO: TEMP ensures

                    rightHand = self._evalTemplates(rightHand)
                    rightHandDef = self._database.getDefinition(rightHand)
                    if isinstance(rightHandDef, Template):
                        for identifier in idList:
                            self._database.setDefinition(identifier, rightHandDef)
                    else:
                        numSet = {Numeric(num) for num in self._database.substitute(rightHand)}
                        for identifier in idList:
                            variable = Variable(identifier)
                            self._database.setDefinition(variable, numSet)
            
            elif result["type"] == "expression":
                exprPiece = result["expression"]
                
                self._ensureIdentifiersExist_REP(exprPiece)
                # things like 'template' are valid (just print it),
                # but 'template + 4' are not
                if not issubclass(exprPiece.obj.cls, Variable):
                    self._ensureIdentifiersAreExpressable_REP(exprPiece)
                self._ensureValidTemplateCallNames_REP(exprPiece)
                self._ensureValidNumTemplateParams_REP(exprPiece)
                # unused _ensureTemplateUses_REP: not defining a template
                
                expr = self._evalRep(exprPiece.obj)
                
                # TODO:  TEMP ensures

                expr = self._evalTemplates(expr)
                if isinstance(expr, Variable):
                    varDef = self._database.getDefinition(expr)
                    if isinstance(varDef, Template):
                        self._print(varDef)
                        return
                
                subSet = {self._convertSympy(sym) for sym in self._database.substitute(expr)}
                for firstSub in subSet:
                    setOfNums = isinstance(firstSub, Numeric)
                    break
                subSetAsStrs = (str(item) for item in subSet)
                if setOfNums:
                    self._print(' or '.join(subSetAsStrs))
                else:
                    self._print(*subSetAsStrs)

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
            args = self._evalRepIter(rawArgs)
            return cls(*args)

        elif isinstance(representation, Represent.TemplateCall):
            nameId = self._evalRep(representation.nameId)
            template = self._database.getDefinition(nameId)
            rawParamVals = representation.params
            paramVals = self._evalRepIter(rawParamVals)
            return TemplateCall(nameId, template, paramVals)

        else:
            raise TypeError("Interpreter tried to evaluate an invalid Represent() type")

    def _evalRepIter(self, repsIter):
        repTypes = (
            Represent,
            Represent.TemplateCall,
        )
        for item in repsIter:
            if type(item) is list:
                yield list(self._evalRepIter(item))
            elif type(item) is tuple:
                yield tuple(self._evalRepIter(item))
            elif type(item) is set:
                yield set(self._evalRepIter(item))
            elif type(item) is type(x for x in []):
                yield (x for x in self._evalRepIter(item))
            elif isinstance(item, repTypes):
                yield self._evalRep(item)
            else:
                yield item

    def _evalTemplates(self, containable):
        if isinstance(containable, Containable):
            return containable.evaluateRecipes()
        else:
            return containable

    # errors when identifiers are not defined (aliased or inferred)
    def _ensureIdentifiersExist_REP(self, stackPiece):
        badTraces = []
        for idTrace in stackPiece.traces:
            if idTrace["type"] == TRACE_TYPES["IDENTIFIER"]:
                identifierRep = idTrace["obj"]
                identifier = self._evalRep(identifierRep)
                if not self._database.doesExist(identifier):
                    badTraces.append(idTrace)
        if len(badTraces) > 0:
            raise UndefinedIdentifierError(badTraces)

    # errors when an identifier in an expression doesn't return an expressable
    # (ex. reference, not call, to a template)
    def _ensureIdentifiersAreExpressable_REP(self, exprPiece, force=False):
        badTraces = []
        for valTrace in exprPiece.traces:
            if valTrace["type"] == TRACE_TYPES["VALUE"]:
                valRep = valTrace["obj"]
                value = self._evalRep(valRep)
                possibleNonExpressable = self._database.getDefinition(value)
                if possibleNonExpressable is not None and not isinstance(possibleNonExpressable, set):
                    badTraces.append(valTrace)
        if len(badTraces) > 0:
            raise InvalidExpressionError(badTraces)

    # errors when template calls are made with non-template identifiers
    # (ex notATemplate() would error)
    def _ensureValidTemplateCallNames_REP(self, anyPiece):
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
    def _ensureValidNumTemplateParams_REP(self, anyPiece):
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
            raise TemplateMismatchError(badTraces)

    # warns when a template's arguments are not used in its definition
    def _ensureTemplateUses_REP(self, paramNamesPiece, rightHandPiece):
        badTraces = []
        for paramTrace in paramNamesPiece.traces:
            if paramTrace["type"] == TRACE_TYPES["IDENTIFIER"]:
                paramIdRep = paramTrace["obj"]
                paramIdName = paramIdRep.args[0]
                inRightHand = False
                for rightHandTrace in rightHandPiece.traces:
                    if rightHandTrace["type"] == TRACE_TYPES["IDENTIFIER"]:
                        rightIdRep = rightHandTrace["obj"]
                        rightIdName = rightIdRep.args[0]
                        if paramIdName == rightIdName:
                            inRightHand = True
                            break
                if not inRightHand:
                    badTraces.append(paramTrace)
        if len(badTraces) > 0:
            warning = UnusedArgumentsWarning(badTraces)
            warning.warn(self._outputFn)

    # errors when a template call evaluates to a non-expressable in an expression
    def _ensureTemplateReturnsExpressable_TEMP(self, exprPiece):
        raise NotImplementedError("ensureTemplateReturnsExpressable")

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
            return True
        
        self._print("(Internal error) {}: {}".format(type(e).__name__, e))
        return False

    def _convertSympy(self, sympyExpr):
        exprStr = str(sympyExpr)
        exprStr = exprStr.replace('**', '^')
        parseResult = self._parser.evaluateLine(exprStr)
        exprRep = parseResult["expression"].obj
        expr = self._evalRep(exprRep)
        # needed to process sympy sqrt()
        expr = self._evalTemplates(expr)
        return expr

    def _updateInferences(self):
        combinedSolutions = self._combineSolutions(self._database.iterateRelations())
        # TODO: there should be a more efficient update method instead of
        #       (re)setting all variables (that won't work when a relation is removed)
        for var in combinedSolutions:
            solSet = combinedSolutions[var]
            if not self._database.isDefined(var):
                self._database.setInference(var, solSet)

    def _combineSolutions(self, relations):
        resultDict = dict()
        for relation in relations:
            allSolutionsAsSyms = relation.solveAll()
            for var in allSolutionsAsSyms:
                if var not in resultDict:
                    resultDict[var] = set()
                solutionsAsSyms = allSolutionsAsSyms[var]
                for solutionSym in solutionsAsSyms:
                    solution = self._convertSympy(solutionSym)
                    resultDict[var].add(solution)
        return resultDict


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
                tokenStrs = (str(token) for token in tokens)
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
                idRep = piece.obj
                varRep = Represent(Variable, idRep)
                piece.update(varRep, tokens, [])
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
            piece.trace(TRACE_TYPES["VALUE"])
            pushStack("values", piece)
        self._parser.onValue(onValue)

        # (specifically binary operators)
        def onOperatorANY(tokens, branch):
            if branch == "PL":
                operatorFn = lambda a, b: a + b
                operatorStr = '+'
            elif branch == "DA":
                operatorFn = lambda a, b: a - b
                operatorStr = '-'
            elif branch == "ST":
                operatorFn = lambda a, b: a * b
                operatorStr = '*'
            elif branch == "SL":
                operatorFn = lambda a, b: a / b
                operatorStr = '/'
            elif branch == "CA":
                operatorFn = lambda a, b: a ** b
                operatorStr = '^'
            piece = StackPieceTracer((operatorStr, operatorFn), tokens)
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
                    operStr, operFn = opList.pop(0)
                    rightExpr = opList.pop(0)
                except IndexError as e:
                    if "pop from empty" in str(e):
                        raise IndexError("Interpreter -> evaluateOperationList(opList) given bad opList")
                if not issubclass(rightExpr.cls, Expressable):
                    raise RuntimeError("tried to evaluate operation list with a non-expressable (right-hand)")
                leftRight = (leftExpr, rightExpr)
                newExpr = Represent(Expression, operStr, operFn, leftRight)
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
                newExpr = Represent(Expression, '-', negOper, [expr])
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
                operInfo = operPiece.obj
                nextOpList = nextOpListPiece.obj
                nextExpr = evaluateOperationList(nextOpList)
                # inserted on left to later be evaluated left-to-right
                # (parser productions return right-to-left)
                opList.insert(0, operInfo)
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
        for stackName in self._stacks:
            stack = self._stacks[stackName]
            if len(stack) > 0:
                badStackNames.append(stackName)
        if len(badStackNames) > 0:
            raise RuntimeError("Interpreter did not use all items in the parse stack")

    def _throwBranchNotImplemented(self, featureNamePlural):
        raise InterpreterNotImplementedError(featureNamePlural)


# used to keep track of data for creating an object, and avoids errors
# until the Interpreter has done the checks it wants
class Represent:
    def __init__(self, repCls, *clsArgs):
        self._cls = repCls
        self._args = clsArgs

    def __repr__(self):
        argsNoLambda = (
            str(arg) if not callable(arg)
                else "(callable)"
            for arg in self._args
        )
        argsStr = ','.join(argsNoLambda)
        return "<R {}({})>".format(self._cls.__name__, argsStr)

    @property
    def cls(self):
        return self._cls

    @property
    def args(self):
        return self._args

    # parser can't see database, and TemplateCall requires a template instance
    # for first arg; this special type gives the necessary information
    class TemplateCall:
        def __init__(self, nameId, params):
            self._nameId = nameId
            self._params = params
            self.cls = TemplateCall
            self.args = params

        def __repr__(self):
            return "<Represent.TemplateCall {}{}>".format(self._nameId, self._params)
        
        @property
        def nameId(self):
            return self._nameId

        @property
        def params(self):
            return self._params


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


class InterpreterDatabase:
    def __init__(self):
        self._assertEqDictKeys()
        
        self._settingDefinition = False
        self._settingInference = False
        
        def definitions(key, value):
            return self._settingDefinition
        def inferences(key, value):
            return self._settingInference
        def relations(key, value):
            return isinstance(key, Relation)

        # defs retains definition order; dict retains all data (including inferred)
        self._defs = list()
        self._dict = FilteredDict(
            definitions,
            inferences,
            relations,
        )

    # working with definitions...
    def setDefinition(self, identifier, values):
        if not isinstance(identifier, (Identifier, Variable)):
            raise TypeError("setDefinition() can only set definitions for an Identifier() or Variable()")
        if not isinstance(values, (Template, set)):
            raise TypeError("setDefinition() can only set definitions to a set")
        if isinstance(values, set):
            for value in values:
                if not isinstance(value, Numeric):
                    raise TypeError("setDefinition() can only take sets of Numeric()s")

        if identifier not in self._dict:
            self._addDef(identifier)
        
        self._settingDefinition = True
        self._dict[identifier] = values
        self._settingDefinition = False

    def getDefinition(self, identifier):
        return self._dict.filter("definitions").get(identifier)

    def isDefined(self, identifier):
        return identifier in self._dict.filter("definitions")

    # working with relations...
    def defineRelation(self, relation, solutions):
        if not isinstance(relation, Relation):
            raise TypeError("defineRelation() only takes a Relation() argument")

        if relation not in self._dict:
            self._addDef(relation)
        self._dict[relation] = solutions
    
    def getRelationSolutions(self, relation):
        return self._dict.filter("relations").get(relation)

    def iterateRelations(self):
        return self._dict.filter("relations").keys()

    # working with inferences...
    def setInference(self, identifier, values):
        if not isinstance(identifier, (Identifier, Variable)):
            raise TypeError("setInference() can only set inferences for an Identifier() or Variable()")
        if not isinstance(values, set):
            raise TypeError("setInference() can only set inferences to Python sets")
        for value in values:
            if not isinstance(value, Expressable):
                raise TypeError("setInference() can only take sets of Expressable()s")

        self._settingInference = True
        self._dict[identifier] = values
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

    def iterateVariableSets(self):
        defs = self._dict.filter("definitions")
        for varDef in defs:
            if isinstance(defs[varDef], set):
                yield varDef
        infs = self._dict.filter("inferences")
        for varInf in infs:
            if isinstance(infs[varInf], set):
                yield varInf

    # substitutes known/inferred values into a given object
    # (defined/inferred are forced-kwargs)
    def substitute(self, expr):
        if isinstance(expr, Numeric):
            return {expr.asSymbol()}
        
        exprSym = expr.asSymbol()
        results = self._substituteUntilNumeric(exprSym)
        didAnySubs = {exprSym} != results
        if not didAnySubs:
            results = self._substituteOnce(exprSym)
        return results

    def _substituteUntilNumeric(self, exprSym, usedVars=None):
        if isinstance(exprSym, sympy.Number):
            return {exprSym}
        
        if usedVars is None:
            usedVars = set()
        usedVars = set(usedVars) # dont modify parent

        varsToIterateAsStr = {str(var) for var in exprSym.free_symbols}
        varsToIterate_nonNumSubs = set()
        varsToIterate_numSubs = set()
        for var in self.iterateVariableSets():
            if str(var) in varsToIterateAsStr:
                # if inferring a variable brings us back to itself,
                # it has no numeric value (if b = d*e, then d = b/e,
                # which means b = (b/e)*e; b is repeated here, so we stop)
                if var in usedVars:
                    return None
                for possibleNum in self._dict[var]:
                    varSubsToNumeric = isinstance(possibleNum, Numeric)
                    break
                if varSubsToNumeric:
                    varsToIterate_numSubs.add(var)
                else:
                    varsToIterate_nonNumSubs.add(var)
                    usedVars.add(var)
        
        # variables with numeric answers are subbed first
        # (b = d*e, e = 2; subbing d*e should lead to d*2 and not b/e*e)
        numSubSyms = set()
        for numSubCombo in self._dictSymCombos(varsToIterate_numSubs):
            subExprSym = exprSym.subs(numSubCombo)
            numSubSyms.add(subExprSym)
        noSubsMade = {exprSym} == numSubSyms

        subUntilNumSymSet = set()
        for numSubExprSym in numSubSyms:
            for subCombo in self._dictSymCombos(varsToIterate_nonNumSubs):
                subExprSym = numSubExprSym.subs(subCombo)
                subUntilNumSymSet_forSubExprSym = self._substituteUntilNumeric(subExprSym, usedVars)
                if subUntilNumSymSet_forSubExprSym is not None:
                    for subUntilNumSym in subUntilNumSymSet_forSubExprSym:
                        subUntilNumSymSet.add(subUntilNumSym)
                else:
                    subUntilNumSymSet.add(numSubExprSym)
                
                if {subExprSym} != subUntilNumSymSet_forSubExprSym:
                    noSubsMade = False
        if noSubsMade:
            # return original expr to avoid complication
            # ex. a = b + c, b = d*e;
            # sub(a) should not return {d*e + c} 
            return {exprSym}
        return subUntilNumSymSet

    def _substituteOnce(self, exprSym, varsToIterate=None):
        if varsToIterate is None:
            # TODO: just get rid of Identifier() and Variable() structures and use symbols...
            varsToIterateAsStr = {str(sym) for sym in exprSym.free_symbols}
            varsToIterate = {var for var in self.iterateVariableSets() if str(var) in varsToIterateAsStr}
        
        resultSubs = set()
        for subCombo in self._dictSymCombos(varsToIterate):
            subSym = exprSym.subs(subCombo)
            resultSubs.add(subSym)
        return resultSubs

    def _dictSymCombos(self, varsToInclude, varsStack=None):
        if varsStack is None:
            varsStack = set(varsToInclude)
        
        if len(varsStack) == 0:
            yield dict()
            return
        
        myVar = varsStack.pop()
        for combo in self._dictSymCombos(varsStack):
            for solution in self._dict[myVar]:
                combo[myVar.asSymbol()] = solution.asSymbol()
                yield combo

    def _assertEqDictKeys(self):
        objs = {
            Identifier: ('name',),
            Variable: (Identifier('name'),),
        }
        for cls1 in objs:
            args1 = objs[cls1]
            newObj1 = cls1(*args1)
            for cls2 in objs:
                args2 = objs[cls2]
                newObj2 = cls2(*args2)
                # both __eq__ and __hash__ are required to be dictionary keys
                assert newObj1 == newObj2, "{} and {} are not equivelant dictionary keys".format(newObj1, newObj2)
                assert hash(newObj1) == hash(newObj2), "{} and {} are not same-hash dictionary keys".format(newObj1, newObj2)
                    
    def _checkKeysVals(self, keys, values, settingType):
        if settingType not in ("definitions", "inferences"):
            raise ValueError("Database cannot check keys/vals for {}".format(settingType))
        
        if settingType == "definitions":
            validKeys = self._validDefinitionKeys
            validVals = self._validDefinitionVals
        elif settingType == "inferences":
            validKeys = self._validInferenceKeys
            validVals = self._validInferenceVals

        for key in keys:
            if not isinstance(key, validKeys):
                validKeyNames = ', '.join(repr(key) for key in validKeys)
                raise TypeError("database tried to insert key that was not one of these: {}".format(validKeyNames))
        for value in values:
            if not isinstance(value, validVals):
                validValNames = ', '.join(repr(key) for key in validVals)
                raise TypeError("database tried to insert value that was not one of these: {}".format(validValNames))
        if len(keys) != len(values):
            raise ValueError("database tried to insert keys/values of different lengths")

    # this should only be used after obj is checked (using _checkKeysVals())
    def _addDef(self, obj):
        pair = (len(self._defs), obj)
        self._defs.append(pair)


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
        def _self_update(self, sequence):
            super().update(sequence)

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
