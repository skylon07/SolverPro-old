import sympy

from structures import *


class AlgebraMaster:
    def __init__(self):
        self._definedSubstitutions = dict()
        self._inferredSubstitutions = dict()
        self._relationsEqZero = set()
        self._relationSymbols = set()

    def substitute(self, expr):
        subsDict = dict(self._definedSubstitutions)
        subsDict.update(self._inferredSubstitutions)
        return Substituter(subsDict).substituteToNumerics(expr)

    def define(self, symbols, vals):
        assert type(symbols) in (tuple, list), "define() requires list or tuple of symbols"
        assert all(type(symbol) in (sympy.Symbol, Identifier) for symbol in symbols), "symbols must be list/tuple of sympy Symbols or Identifiers"
        assert type(vals) is SubSet, "define() requires vals to be a SubSet"
        
        symbols = self._identifiersToSymbols(symbols)
        subVals = SubSet.join(self.substitute(val) for val in vals)
        if not subVals.isNumericSet:
            raise NotANumericException()
        self._definedSubstitutions.update({symbol: subVals for symbol in symbols})
        self._updateInferredSubstitutions()

    def getDefinition(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "getDefinition() can only work for sympy Symbols and Identifiers"
        symbol = self._identifiersToSymbols([symbol])[0]
        return self._definedSubstitutions.get(symbol)

    def isDefined(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "isDefined() can only work for sympy Symbols and Identifiers"
        symbol = self._identifiersToSymbols([symbol])[0]
        return symbol in self._definedSubstitutions

    def relate(self, leftExpr, rightExpr):
        assert isinstance(leftExpr, sympy.Expr), "Cannot relate left side; not a sympy Expr"
        assert isinstance(rightExpr, sympy.Expr), "Cannot relate right side; not a sympy Expr"
        exprEqZero = leftExpr - rightExpr

        self._relationsEqZero.add(exprEqZero)
        self._relationSymbols.update(exprEqZero.free_symbols)
        self._updateInferredSubstitutions()

    def getInference(self, expr):
        assert isinstance(expr, sympy.Expr) or type(expr) is Identifier, "getInference() can only work for sympy Exprs and Identifiers"
        expr = self._identifiersToSymbols([expr])[0]
        return self._inferredSubstitutions.get(expr)

    def isInferred(self, expr):
        assert isinstance(expr, sympy.Expr) or type(expr) is Identifier, "getInference() can only work for sympy Exprs and Identifiers"
        expr = self._identifiersToSymbols([expr])[0]
        return expr in self._inferredSubstitutions

    def getKnown(self, expr):
        if type(expr) in (sympy.Symbol, Identifier):
            return self.getDefinition(expr) or self.getInference(expr)
        else:
            return self.getInference(expr)

    def isKnown(self, expr):
        if type(expr) in (sympy.Symbol, Identifier):
            return self.isDefined(expr) or self.isInferred(expr)
        else:
            return self.isInferred(expr)

    def exists(self, symbol):
        return symbol in self._relationSymbols

    def _identifiersToSymbols(self, identifiersOrSymbols):
        # function exists purely for syntactical purposes
        def raiseInvalidType():
            raise NotImplementedError("List must contain Identifiers and sympy Symbols/Exprs; nothing else")
        
        return [
            identifierToSymbol(symbol) if type(symbol) is Identifier
            else symbol if isinstance(symbol, sympy.Expr)
            else raiseInvalidType()
            for symbol in identifiersOrSymbols
        ]

    # relational/solving functions
    def _updateInferredSubstitutions(self):
        allSymbolicSubs = dict(self._definedSubstitutions)
        allSymbolicSubs.update(self._genNumericSolsFromRelations(self._relationsEqZero))
        self._extrapolateSolutions(allSymbolicSubs)
        self._inferredSubstitutions = {
            exprKey: numSubSet
            for (exprKey, numSubSet) in self._filterNonNumericSubSets(allSymbolicSubs)
        }

    def _genNumericSolsFromRelations(self, relations):
        numericSubs = dict()
        for relation in relations:
            for negNumeric in (atom for atom in relation.atoms() if isNumeric(atom)):
                eqToNumeric = -negNumeric
                solutionSet = SubSet(sympy.solveset(relation, eqToNumeric))
                for solution in solutionSet:
                    subSet = numericSubs.get(solution, SubSet())
                    subSet.add(eqToNumeric)
                    numericSubs[solution] = subSet
        return numericSubs

    def _extrapolateSolutions(self, allSymbolicSubs):
        inferredSubSets = allSymbolicSubs.values()
        numInferred = sum(len(subSet) for subSet in inferredSubSets)
        inferredSubsChanged = True
        iters = 0
        while inferredSubsChanged:
            lastNumInferred = numInferred
            
            # dictionary changes; we have to memorize the keys to iterate
            exprKeys = [key for key in allSymbolicSubs.keys()]
            for exprKey in exprKeys:
                numericsEqToExprKey = allSymbolicSubs[exprKey]
                if not numericsEqToExprKey.isNumericSet:
                    continue

                # TODO: is a SubSet() really necessary here?
                #       what about other places?
                # TODO: maybe substitute known values before making exprsToSub
                exprsToSub = SubSet({exprKey})
                usedSymbols = set()
                collapsedExprSet = self._collapseExprToNumerics(exprsToSub, numericsEqToExprKey, allSymbolicSubs, usedSymbols)
                if __debug__:
                    if len(collapsedExprSet) > 0 and collapsedExprSet.isNumericSet:
                        actualSubSet = allSymbolicSubs[exprKey]
                        assert collapsedExprSet == actualSubSet, "collapsing expression yielded the wrong answer"
                    else:
                        assert collapsedExprSet.isExpressionSet, "collapsing expression did not extract solutions correctly"

            inferredSubSets = allSymbolicSubs.values()
            numInferred = sum(len(subSet) for subSet in inferredSubSets)
            inferredSubsChanged = numInferred != lastNumInferred

            iters += 1
            assert iters < 999999, "_extrapolateSolutions() stuck in an infinite loop"

    def _collapseExprToNumerics(self, exprsToSub, exprsEqSubSet, allSymbolicSubs, usedSymbols):
        assert exprsEqSubSet.isNumericSet, "Cannot collapse expr to numerics if not given the numerics it equals"

        resultSubSet = SubSet()
        for exprToSub in exprsToSub:
            if len(exprToSub.free_symbols) == 1:
                for theOnlySymbol in exprToSub.free_symbols:
                    symbol = theOnlySymbol
                for numericForExprToSub in exprsEqSubSet:
                    relationForExprToSub = exprToSub - numericForExprToSub
                    solutions = sympy.solveset(relationForExprToSub, symbol)
                    # TODO: this indicates some solve function needs to be refactored...
                    if type(solutions) is sympy.Complement:
                        if solutions.args[0] == sympy.Complexes:
                            solutions = solutions.args[1]
                        else:
                            # this happens when an expression was originally solved into the denominator;
                            # just ignore the undefined point
                            solutions = solutions.args[0]
                    symbolIsStillFreeVariable = solutions is sympy.Complexes or len(solutions) == 0
                    if symbolIsStillFreeVariable:
                        # no way to collapse to numerics then!
                        continue # TODO: maybe we can just return...?
                    symbolNumSubSet = SubSet(solutions)
                    assert symbolNumSubSet.isNumericSet, "SubSet after solving for single variable was not a numeric set"
                    
                    if symbol not in allSymbolicSubs:
                        allSymbolicSubs[symbol] = symbolNumSubSet
                    else:
                        possibleNumerics = allSymbolicSubs[symbol]
                        invalidNumerics = [numeric for numeric in symbolNumSubSet if numeric not in possibleNumerics]
                        if len(invalidNumerics) > 0:
                            raise ContradictionException(symbol, possibleNumerics, invalidNumerics)
                    resultSubSet.addFrom(
                        exprToSub.subs(symbol, numeric)
                        for numeric in symbolNumSubSet
                    )
            else:
                for symbolToSub in exprToSub.free_symbols:
                    if symbolToSub in usedSymbols:
                        continue
                    usedSymbols.add(symbolToSub)

                    for subExprsToSub in self._subExprUsingAllRoutesForSymbol(exprToSub, exprsEqSubSet, symbolToSub, allSymbolicSubs):
                        assert type(subExprsToSub) is SubSet, "Tried to substitute for a symbol, but a SubSet was not returned"
                        exprSubSet = self._collapseExprToNumerics(subExprsToSub, exprsEqSubSet, allSymbolicSubs, usedSymbols)
                        allSymbolsSolved = len(exprSubSet) > 0 and exprSubSet.isNumericSet
                        if allSymbolsSolved:
                            resultSubSet.addFrom(exprSubSet)
                            break # to avoid redundantly substituting the same variable again

                    usedSymbols.remove(symbolToSub)
                    if symbolToSub in allSymbolicSubs and allSymbolicSubs[symbolToSub].isExpressionSet:
                        allSymbolicSubs.pop(symbolToSub)
        return resultSubSet

    def _subExprUsingAllRoutesForSymbol(self, exprToSub, exprEqSubSet, symbolToSub, allSymbolicSubs):
        allSymbolSubs = {
            symbol: allSymbolicSubs[symbol]
            for symbol in allSymbolicSubs
            if type(symbol) is sympy.Symbol
        }
        relationsToSub = {
            exprToSub - numericEqToExprToSub
            for numericEqToExprToSub in exprEqSubSet
        }
        
        symbolSubsKnown = symbolToSub in allSymbolicSubs
        if symbolSubsKnown:
            usedExprKeys = set()
            subExprSet = self._recursiveSubstitute(exprToSub, allSymbolSubs, usedExprKeys)
            assert not any(symbolToSub in expr.free_symbols for expr in subExprSet), "Substituting didn't eliminate variable"
            yield subExprSet
        else:
             # dictionary changes; we have to memorize the keys to iterate
            exprKeys = [key for key in allSymbolicSubs.keys()]
            for exprKey in exprKeys:
                # no point in substituting the symbol if it isn't even in the expression
                if symbolToSub not in exprKey.free_symbols:
                    continue
                exprKeyEqSubSet = allSymbolicSubs[exprKey]
                
                # you know how you need 3 unique equations to solve a syatem of 3 variables?
                # this is what this is trying to determine...
                if self._exprProvidesUniqueSolution(exprKey, exprKeyEqSubSet, relationsToSub, symbolToSub, allSymbolSubs):
                    symbolSubs = SubSet.join(
                        self._solveRelationForSymbol(subExprKey - numericEqToExprKey, symbolToSub)
                        for subExprKey in self._recursiveSubstitute(exprKey, allSymbolSubs, set())
                        for numericEqToExprKey in exprKeyEqSubSet
                    )
                    allSymbolSubs[symbolToSub] = allSymbolicSubs[symbolToSub] = symbolSubs
                    usedExprKeys = set()
                    subExprSet = self._recursiveSubstitute(exprToSub, allSymbolSubs, usedExprKeys)
                    assert not any(symbolToSub in expr.free_symbols for expr in subExprSet), "Substituting didn't eliminate variable"
                    yield subExprSet

    def _exprProvidesUniqueSolution(self, expr, exprEqSubSet, baseRelations, symbol, allSymbolSubs):
        for subExpr in self._recursiveSubstitute(expr, allSymbolSubs, set()):
            for numericEqToExprKey in exprEqSubSet:
                solutions = sympy.solveset(subExpr - numericEqToExprKey, symbol)
                # TODO: this indicates some solve function needs to be refactored...
                if type(solutions) is sympy.Complement:
                    # this happens when an expression was originally solved into the denominator;
                    # just ignore the undefined point
                    solutions = solutions.args[0]
                if solutions is sympy.Complexes or len(solutions) == 0:
                    return False
                symbolSubs = SubSet(solutions)
                for symbolSub in symbolSubs:
                    for relation in baseRelations:
                        solutionIsRedundant = relation.subs(symbol, symbolSub) == 0
                        if solutionIsRedundant:
                            return False
        return True

    def _solveRelationForSymbol(self, relation, symbol):
        symbolSubs = sympy.solveset(relation, symbol)
        if type(symbolSubs) is sympy.Complement:
            # this happens when an expression was originally solved into the denominator;
            # just ignore the undefined point
            symbolSubs = symbolSubs.args[0]
        assert symbolSubs is not sympy.Complexes, "Tried to solve relation for single symbol which was a free variable"
        return SubSet(symbolSubs)

    def _filterNonNumericSubSets(self, subDict):
        for exprKey in subDict:
            assert isinstance(exprKey, sympy.Expr), "subDict did not have sympy Expr keys"
            subSet = subDict[exprKey]
            assert type(subSet) is SubSet, "subDict did not contain SubSet values"
            if subSet.isNumericSet:
                yield (exprKey, subSet)


class Substituter:
    def __init__(self, subsDict):
        assert all(isinstance(exprKey, sympy.Expr) for exprKey in subsDict.keys()), "Substitution dictionary must only contain sympy Expr keys"
        assert all(type(subSet) is SubSet for subSet in subsDict.values()), "Substitution dictionary must only map to SubSet values"
        self._substitutions = subsDict
        self._usedKeys = set()

    def substituteToNumerics(self, expr):
        assert all(subSet.isNumericSet for subSet in self._substitutions.values()), "Substituting to numeric requires dictionary to only contain numeric substitutions"
        
        if isNumeric(expr):
            return SubSet({expr})
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        
        resultSet = self._substituteAllCombos(SubSet({expr}))
        assert len(self._usedKeys) == 0, "failed to pop all used keys"
        return resultSet

    def substituteByElimination(self, expr):
        resultSet = self._substituteAllCombos(SubSet({expr}))
        assert len(self._usedKeys) == 0, "failed to pop all used keys"
        assert not any(symbol in result.free_symbols for result in resultSet for exprKey in self._substitutions for symbol in exprKey.free_symbols), "Elimination-substitution requires a dict with expression keys with unidirectional dependencies (can't have {a: b + c, b: a * c}, but CAN have {a: b + c, b: 2 * c})"
        return resultSet

    def backSubstitute(self):
        assert all(type(symbolKey) is sympy.Symbol for symbolKey in self._substitutions.keys()), "Backwards-substitution only works on symbol substitution dictionaries"
        symbolKeys = sorted(self._substitutions.keys(), key=self._getNumBackSubDeps)
        for (symbolKey, numSymbolsProcessed) in zip(symbolKeys, range(len(symbolKeys))):
            symbolSubs = self._substitutions[symbolKey]

            if symbolSubs.hasNumerics:
                symbolSubs = SubSet(numeric for numeric in symbolSubs if isNumeric(numeric))
            else:
                substituter = Substituter(self._substitutions)
                symbolSubs = substituter._substituteInOrder(symbolSubs, symbolKeys[:numSymbolsProcessed])
            
            self._substitutions[symbolKey] = symbolSubs
        return self._substitutions

    def substituteInOrder(self, expr, keyOrder):
        return self._substituteInOrder(SubSet({expr}), keyOrder)

    # below are different kinds of substitution algoritms;
    # some are used, some might not be, but they're still useful to have around
    def _substituteAllPaths(self, primaryExprs):
        # TODO: the keys should be sorted so largest expressions are substituted first
        for exprKey in self._unusedKeys():
            if primaryExprs.isNumericSet:
                return primaryExprs
            
            self._usedKeys.add(exprKey)

            primaryExprs = SubSet.join(
                self._substituteAllPaths(SubSet({primaryExpr.subs(exprKey, exprKeySub)}))
                for primaryExpr in primaryExprs
                for exprKeySub in self._substitutions[exprKey]
            )

            self._usedKeys.remove(exprKey)
        return primaryExprs

    def _substituteAllCombos(self, primaryExprs):
        return SubSet(
            self._subDictUntilFixed(primaryExpr, subCombo)
            for subCombo in self._makeSubCombos()
            for primaryExpr in primaryExprs
        )

    def _makeSubCombos(self, comboDict=None):
        if comboDict is None:
            comboDict = dict()
        
        currKey = first(iterDifference(self._substitutions, comboDict), None)
        if currKey is None:
            comboDictFinished = comboDict
            yield comboDictFinished
        else:
            subSet = self._substitutions[currKey]
            for exprSub in subSet:
                comboDict[currKey] = exprSub
                for comboDictFinished in self._makeSubCombos(comboDict):
                    yield comboDictFinished
            # an empty subSet breaks popping from comboDict below, and it just isn't a good sign...
            assert len(subSet) > 0, "Cannot have empty numeric set"
            comboDict.pop(currKey)

    def _subDictUntilFixed(self, expr, subDict):
        assert expr is not None, "cannot sub a nonexistant expression"
        lastExpr = None
        while lastExpr is not expr:
            lastExpr = expr
            expr = expr.subs(subDict)
        return expr

    def _substituteInOrder(self, primaryExprs, keyOrder):
        for exprKey in keyOrder:
            assert exprKey in self._substitutions, "keyOrder contained exprKey not included in the original subs dict"
            primaryExprs = SubSet(
                primaryExpr.subs(exprKey, exprKeySub)
                for primaryExpr in primaryExprs
                for exprKeySub in self._substitutions[exprKey]
            )
        return primaryExprs

    def _unusedKeys(self):
        return iterDifference(self._substitutions, self._usedKeys)

    def _getNumBackSubDeps(self, symbolKey):
        symbolSubs = self._substitutions[symbolKey]
        if symbolSubs.hasNumerics:
            return 0
        
        symbolDeps = {
            symbolDep
            for subExpr in symbolSubs
            for symbolDep in subExpr.free_symbols
        }
        assert symbolKey not in symbolDeps, "symbol can't depend on itself"
        return len(symbolDeps)


class Solver:
    def __init__(self, relations, symbolDefinitions=None):
        self._relationsEqZero = set(relations)
        self._subsDict = dict()
        self._symbolSubs = dict(symbolDefinitions) if symbolDefinitions is not None else dict()

    def genNumericSolutions(self):
        self._subsDict = dict()
        self._genSolutionsFromNumericsInRelations()
        self._extrapolateNumericSolutionsForSymbols()
        self._subsDict.update({
            symbol: numericSubSet
            for symbol in self._symbolSubs
            for numericSubSet in [self._symbolSubs[symbol]]
            if numericSubSet.isNumericSet
        })
        return self._subsDict

    def _genSolutionsFromNumericsInRelations(self):
        for relation in self._relationsEqZero:
            # although technically the numeric given is the negative/opposite
            # (ie x + 4 (= 0); x is actually -4; we negate the original 4 to get
            # the "solution value"), this doesn't work well for exponents. So, we just
            # let the solver do its thing without negating (which still works, just
            # returns 4 = -x instead)
            for eqNumeric in (atom for atom in relation.atoms() if isNumeric(atom)):
                solutionSet = self._extractRelationalSolutionSet(self._solveSet(relation, eqNumeric))
                for exprKey in solutionSet:
                    self._updateNumericSubs(exprKey, SubSet({eqNumeric}))

    def _extrapolateNumericSolutionsForSymbols(self):
        numSubs = self._getTotalNumSubstitutions()
        substitutionsChanged = True
        iters = 0
        while substitutionsChanged:
            lastNumSubs = numSubs
            
            for exprKey in self._subsDict:
                symbol = self._findSymbolToSolve(exprKey)
                cantInferFromExprKey = symbol is None
                if cantInferFromExprKey:
                    continue

                symbolSubs = SubSet.join(
                    self._inferSymbolFromRelation(symbol, exprKey - numericSub)
                    for numericSub in self._subsDict[exprKey]
                )
                inferredAnyVals = len(symbolSubs) > 0
                if inferredAnyVals:
                    self._updateSymbolSub(symbol, symbolSubs)
            self._symbolSubs = Substituter(self._symbolSubs).backSubstitute()

            numSubs = self._getTotalNumSubstitutions()
            substitutionsChanged = numSubs != lastNumSubs

            iters += 1
            assert iters < 9999, "Substitutions shouldn't get stuck changing..."

    def _getTotalNumSubstitutions(self):
        return (
            sum(len(subSet) for subSet in self._symbolSubs.values() if subSet.isNumericSet),
            sum(len(subSet) for subSet in self._symbolSubs.values()),
        )

    def _findSymbolToSolve(self, expr):
        return first(iterDifference(expr.free_symbols, self._symbolSubs), None)

    def _inferSymbolFromRelation(self, symbol, relation):
        subRelationSet = Substituter(self._symbolSubs).substituteByElimination(relation)
        symbolSubs = SubSet.join(
            newSubSet
            for subRelation in subRelationSet
            for newSubSet in [self._extractInferenceSolutionSet(self._solveSet(subRelation, symbol))]
            if newSubSet is not None
        )
        if symbolSubs.hasNumerics:
            if symbolSubs.isNumericSet:
                return symbolSubs
            else:
                return SubSet(numeric for numeric in symbolSubs if isNumeric(numeric))
        else:
            return symbolSubs

    def _solveSet(self, expr, atom):
        # solving x ** 2 for 2 gives us errors (but if we substitute a symbol it's fine...?)
        # so since solving for numeric atoms doesn't work as expected,
        # hopefully substituting them for symbols will match expectations better
        if isNumeric(atom):
            numericSub = sympy.Symbol(":NUMERIC {}:".format(atom))
            exprWithNumericSubbed = expr.subs(atom, numericSub)
            solution = self._solveSet(exprWithNumericSubbed, numericSub)
            solution.reverseSubstitutionMade({numericSub: atom})
            return solution
        else:
            solutionSet = sympy.solveset(expr, atom)
            return self._Solution(solutionSet)

    def _updateNumericSubs(self, exprKey, subSet):
        assert type(subSet) is SubSet, "_updateNumericSubs() requires a SubSet"
        assert subSet.isNumericSet, "_subsDict can only contain numeric substitutions"
        if exprKey not in self._subsDict:
            self._subsDict[exprKey] = subSet
        else:
            self._subsDict[exprKey].addFrom(subSet)
        assert len(self._subsDict[exprKey]) > 0, "Cannot have empty substitution set for main expression substitutions"

    def _updateSymbolSub(self, symbolKey, subSet):
        assert type(subSet) is SubSet, "_updateSymbolSub() requires a SubSet"
        assert type(symbolKey) is sympy.Symbol, "_updateSymbolSub() requires a sympy Symbol key"
        if symbolKey not in self._symbolSubs:
            self._symbolSubs[symbolKey] = subSet
        else:
            self._symbolSubs[symbolKey].addFrom(subSet)
        assert len(self._symbolSubs[symbolKey]) > 0, "Cannot have empty substitution set for symbol substitutions"

    def _extractRelationalSolutionSet(self, solution):
        types = self._Solution.types
        if solution.type is types.NORMAL:
            solutionSet = SubSet(solution.set)
            return solutionSet
        elif solution.type is types.CONDITIONAL:
            (numericSymbol, eqCondition, baseSet) = solution.set.args
            assert type(numericSymbol) is NumericSymbol, "Tried to extract a realtional solution from a condition set which was not solved for a numeric"
            (leftCondition, rightCondition) = eqCondition.args
            expRelation = leftCondition - rightCondition
            return self._extractRelationalSolutionSet(self._solveExponent(expRelation, toNumber(str(numericSymbol))))
        elif solution.type is types.BRANCHES:
            return SubSet.join(
                self._extractRelationalSolutionSet(branchSolution)
                for branchSolution in solution.set
            )
        else:
            raise NotImplementedError("missing a relational solution type case (general case)")

    def _solveExponent(self, expr, atom):
        assert expr.is_Add, "can't solve an exponential relation if it doesn't follow the format!"
        (exprArg1, exprArg2) = expr.args
        (mul1, exp1, is1Exp) = self._extractMulAndExp(exprArg1)
        (mul2, exp2, is2Exp) = self._extractMulAndExp(exprArg2)
        assert exp1.is_Pow or exp2.is_Pow, "uh... wait, I thought we were solving exponential equations here?"
        if exp1.is_Pow and exp2.is_Pow:
            raise NotImplementedError("Only one term can be an exponential expression")
        elif exp1.is_Pow:
            exp = exp1
            mul = mul1
            eqToExpr = exprArg2
        elif exp2.is_Pow:
            exp = exp2
            mul = mul2
            eqToExpr = exprArg1

        (base, exp) = exp.args
        inBase = atom in base.atoms()
        inExp = atom in exp.atoms()
        assert inBase or inExp, "Can't solve for an exponential expression if the target symbol isn't even in it"
        if inBase and inExp:
            raise NotImplementedError("Solving exponents where atom is in the base and exponent")
        elif inBase:
            baseSubs = sympy.solve(expr, base)
            assert type(baseSubs) is list, "solve() for only one variable should have returned list"
            return self._Solution.makeBranches(
                self._solveSet(base - baseSub, atom)
                for baseSub in baseSubs
            )
        elif inExp:
            expSubs = sympy.solve(expr, exp)
            assert type(expSubs) is list, "solve() for only one variable should have returned list"
            return self._Solution.makeBranches(
                self._solveSet(exp - expSub, atom)
                for expSub in expSubs
            )

    def _extractMulAndExp(self, expr):
        if expr.is_Mul:
            (arg1, arg2) = expr.args
            if arg1.is_Pow:
                return (arg2, arg1, True)
            elif arg2.is_Pow:
                return (arg1, arg2, True)
        elif expr.is_Pow:
            return (1, expr, True)
        return (1, expr, False)

    def _extractInferenceSolutionSet(self, solution):
        types = self._Solution.types
        if solution.type is types.NORMAL:
            solutionSet = SubSet(solution.set)
            return solutionSet
        elif solution.type is types.COMPLEXES:
            # this means a variable can be any value and still hold true in the relation,
            # which ultimately means the relation provided no new information for the symbol;
            # we therefore return saying "hey, this symbol has no new substitutions"
            return None
        elif solution.type is types.EMPTY:
            # similar to the above case, except we ended up with some kind of
            # 4 = 0 case (say, 1 + 3(b + 1)/(b + 1))
            return None
        elif solution.type is types.COMPLEMENT:
            if len(solution.set.args[1]) > 0:
                # this happens when the solver is forced to put a variable in the denominator of a fraction;
                # this generates a solution with (calculus) "holes", which can be ignored
                return SubSet(solution.set.args[0])
            else:
                raise NotImplementedError("missing an inference solution type case (for COMPLEMENT sets)")
        else:
            raise NotImplementedError("missing an inference solution type case (general case)")

    class _Solution:
        @classmethod
        def makeBranches(cls, iterable):
            solution = cls.__new__(cls)
            solution._solutionSet = set(iterable)
            solution._type = cls.types.BRANCHES
            return solution

        class _types:
            @property
            def NORMAL(self):
                return "NORMAL"

            @property
            def EMPTY(self):
                return "EMPTY"

            @property
            def COMPLEXES(self):
                return "COMPLEXES"

            @property
            def COMPLEMENT(self):
                return "COMPLEMENT"

            @property
            def CONDITIONAL(self):
                return "CONDITIONAL"

            @property
            def BRANCHES(self):
                return "BRANCHES"

        types = _types()

        def __init__(self, solutionSet):
            self._solutionSet = solutionSet
            self._type = self._getSetType(solutionSet)

        @property
        def type(self):
            return self._type

        @property
        def set(self):
            return self._solutionSet

        def _getSetType(self, solutionSet):
            types = self.types
            if type(solutionSet) is sympy.FiniteSet:
                if len(solutionSet) == 0:
                    return types.EMPTY
                else:
                    return types.NORMAL
            elif solutionSet is sympy.EmptySet:
                return types.EMPTY
            elif type(solutionSet) is sympy.Complement:
                return types.COMPLEMENT
            elif solutionSet is sympy.Complexes:
                return types.COMPLEXES
            elif type(solutionSet) is sympy.ConditionSet:
                return types.CONDITIONAL
            else:
                raise NotImplementedError("Solution ran into an unconsidered type scenario")

        def reverseSubstitutionMade(self, subsDict):
            self._solutionSet = self._reverseSubsForSet(self._solutionSet, subsDict)

        def _reverseSubsForSet(self, sympySet, subsDict):
            # subsDict is used directly since it is assumed it is already a reverse-dictionary
            # of the substitutions that already occurred (instead of making the caller construct
            # a dictionary accurate to history and reversing it here, which would be less efficient)
            types = self.types
            setType = self._getSetType(sympySet)
            if setType is types.NORMAL:
                # no need to substitute; the symbol being solved for was already extracted
                returnSet = sympySet
            elif setType is types.EMPTY:
                # there is nothing to substitute back
                returnSet = sympySet
            elif setType is types.COMPLEXES:
                # sort of like the above case, except it's everything
                returnSet = sympySet
            elif setType is types.COMPLEMENT:
                (mainSet, complementSet) = sympySet.args
                mainSetSubbed = self._reverseSubsForSet(mainSet, subsDict)
                complementSetSubbed = self._reverseSubsForSet(complementSet, subsDict)
                return sympy.Complement(mainSetSubbed, complementSetSubbed)
            elif setType is types.CONDITIONAL:
                (symbol, eqCondition, baseSet) = sympySet.args
                assert type(eqCondition) is sympy.Eq, "sympy ConditionSet did not have Eq() as expected"
                (eqLeft, eqRight) = eqCondition.args
                symbolSubbed = symbol.subs(subsDict)
                if isNumeric(symbolSubbed):
                    # ConditionSets really don't like numerics as their first arg... this gets around that
                    # (of course, this means the extractor will need to convert it bsck later...)
                    symbolSubbed = NumericSymbol(symbolSubbed)
                eqConditionSubbed = sympy.Eq(eqLeft.subs(subsDict), eqRight.subs(subsDict))
                return sympy.ConditionSet(symbolSubbed, eqConditionSubbed, baseSet)
            else:
                raise NotImplementedError("Tried to reverse a previous substitution for an unconsidered solution type")

            assert not any(symbolKey in expr.atoms() for expr in returnSet for symbolKey in subsDict)
            return returnSet


class NotANumericException(Exception):
    pass # exists just as a type

class ContradictionException(Exception):
    def __init__(self, expr, validVals, invalidVals):
        self._expr = expr
        self._validVals = validVals
        self._invalidVals = invalidVals


if __name__ == "__main__":
    S = sympy.Symbol
    master = AlgebraMaster()
    
    # master._inferredSubstitutions = {
    #     S('a'): SubSet({1}),
    #     S('a') + S('b'): SubSet({2}),
    #     S('c'): SubSet({3}),
    # }
    # print(master.substitute(S('a')))
    # print(master.substitute(S('a') + S('b')))
    # print(master.substitute(S('c') + S('b')))

    # print((S('a') + 4).atoms())
    # print((S('a') - 4).atoms())
    # print((S('a') + -4).atoms())
    # print((S('a') + -4).subs(4, 3))
    # print((S('a') + -4).subs(-4, 3))

    # print(sympy.solveset(S('a') + S('b') + S('c'), S('a')))
    # print(sympy.solveset(S('a') + S('b') + S('c'), S('a') + S('b')))
    # print(sympy.solveset(S('a') + S('b') + S('c') + 3, 3))

    # sols = SubSet()
    # sols.add(4)
    # sols.add(S('a') + 4 - S('a'))
    # print(4)
    # print(S('a') + 4 - S('a'))
    # print(S('a') + 4 - S('a') is 4)
    # print(S('a') + 4 - S('a') == 4)
    # print(hash(S('a') + 4 - S('a')) == hash(4))
    # print(sols)

    # print((S('a') + S('b') - 4).subs(S('a'), 2))

    # print(sympy.solveset(2 / S('a') - 4, 2))
    
    # eq1 = S('a') + 2 * S('b') - 5
    # solve1_1 = [item for item in sympy.solveset(eq1, 2)][0]
    # master._inferredSubstitutions[solve1_1] = SubSet({2})
    # solve1_2 = [item for item in sympy.solveset(eq1, 5)][0]
    # master._inferredSubstitutions[solve1_2] = SubSet({5})
    # eq2 = 2 * S('a') + S('b') - 4
    # solve2_1 = [item for item in sympy.solveset(eq2, 2)][0]
    # master._inferredSubstitutions[solve2_1] = SubSet({2})
    # solve2_2 = [item for item in sympy.solveset(eq2, 4)][0]
    # master._inferredSubstitutions[solve2_2] = SubSet({4})
    # print(eq1.subs(S('a'), [item for item in sympy.solveset(solve1_1 - 2, S('a'))][0]))
    # print(eq1.subs(S('a'), [item for item in sympy.solveset(solve1_2 - 5, S('a'))][0]))
    # # print(eq1.subs(S('a'), [item for item in sympy.solveset(solve2_1 - 2, S('a'))][0]))
    # print(eq1.subs(S('a'), [item for item in sympy.solveset(solve2_2 - 4, S('a'))][0]))
    # b = [item for item in sympy.solveset(eq1.subs(S('a'), [item for item in sympy.solveset(solve2_2 - 4, S('a'))][0]), S('b'))][0]
    # a = [item for item in sympy.solveset(eq1.subs(S('b'), b), S('a'))][0]
    # print(a, b)

    # print((2 * S('a') + 1/3 + sympy.pi).atoms(sympy.Number, sympy.NumberSymbol))

    # master.relate(S('a') - S('b'), sympy.simplify(1))
    # master.relate(S('a') + S('b'), sympy.simplify(9))

    (a, b, c, d) = sympy.symbols("a, b, c, d")
    def dictIncludes(mainDict, shouldContainDict):
        dictDoesNotContainItem = object()
        for shouldContainKey in shouldContainDict:
            shouldContainVal = shouldContainDict[shouldContainKey]
            mainVal = mainDict.get(shouldContainKey, dictDoesNotContainItem)
            if type(shouldContainVal) is SubSet and shouldContainVal.isNumericSet:
                try:
                    shouldFloat = SubSet(
                        float(shouldHaveItem)
                        for shouldHaveItem in shouldContainVal
                    )
                    mainFloat = SubSet(
                        float(mainItem)
                        for mainItem in mainVal
                    )
                    mainDictContainsVal = shouldFloat == mainFloat
                except TypeError:
                    return False
            else:
                mainDictContainsVal = mainVal == shouldContainVal
            if not mainDictContainsVal:
                return False
        return True
    
    solutions = Solver({
        a + 2 * b + c  -  0,
        3 * a - b + 2 * c  -  11,
        -a + b - c  -  -6,
    }).genNumericSolutions()
    assert dictIncludes(solutions, {
        a: SubSet({1}),
        b: SubSet({-2}),
        c: SubSet({3}),
    })

    solutions = Solver({c ** 2 - 4}).genNumericSolutions()
    assert dictIncludes(solutions, {
        c: SubSet({2}),
    })

    solutions = Solver({d ** -2 - 1/4}).genNumericSolutions()
    assert dictIncludes(solutions, {
        d: SubSet({2, -2}),
    })
    
    solutions = Solver({
        (a * c) - b  -  5,
        (c ** 2 - a)/ b  -  -4,
    }).genNumericSolutions()
    assert dictIncludes(solutions, {
        a: SubSet({1}),
        b: SubSet({-2}),
        c: SubSet({3}),
    })

    # a + 2b + c = 0
    # 3a - b + 2c = 11
    # -a + b - c = -6
    # a = -2b - c
    # b = -(11 + c) / 7
    ans = {
        a: sympy.simplify(1),
        b: sympy.simplify(-2),
        c: sympy.simplify(3),
    }
    # master.relate(a + 2 * b + c, (a + 2 * b + c).subs(ans))
    # master.relate(a * c + 2 * b, (a * c + 2 * b).subs(ans))
    # master.relate(c * (b - a), (c * (b - a)).subs(ans))
    master.relate(a + 2 * b + c, (a + 2 * b + c).subs(ans))
    master.relate(3 * a - b + 2 * c, (3 * a - b + 2 * c).subs(ans))
    master.relate(-a + b - c, (-a + b - c).subs(ans))
    master = master
