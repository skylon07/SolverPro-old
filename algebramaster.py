import sympy

from structures import *


class AlgebraMaster:
    def __init__(self):
        self._definedSubstitutions = dict()
        self._inferredSubstitutions = dict()
        self._relationsEqZero = set()
        self._relationSymbols = set()

    def substitute(self, expr):
        if isNumeric(expr):
            return SubSet({expr})
        
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        # TODO: the keys should be sorted so largest expressions are substituted first
        subsDict = dict(self._definedSubstitutions)
        subsDict.update(self._inferredSubstitutions)
        usedSymbols = set()
        assert len([subSet for subSet in subsDict.values() if not subSet.isNumericSet]) == 0, "substitute() assumes that only a numeric substitution dictionary is passed"
        return self._recursiveSubstitute(expr, subsDict, usedSymbols)

    def define(self, symbols, vals):
        assert type(symbols) in (tuple, list), "define() requires list or tuple of symbols"
        assert len([symbol for symbol in symbols if type(symbol) not in (sympy.Symbol, Identifier)]) == 0, "symbols must be list/tuple of sympy Symbols or Identifiers"
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
            assert "this" == "invalid type", "List must contain Identifiers and sympy Symbols/Exprs; nothing else"
        
        return [
            identifierToSymbol(symbol) if type(symbol) is Identifier
            else symbol if isinstance(symbol, sympy.Expr)
            else raiseInvalidType()
            for symbol in identifiersOrSymbols
        ]

    # substitution helper methods
    def _recursiveSubstitute(self, expr, subsDict, usedExprKeys):
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        assert len([exprKey for exprKey in subsDict.keys() if not isinstance(exprKey, sympy.Expr)]) == 0, "subsDict must be a mapping from sympy Exprs"
        assert len([subSet for subSet in subsDict.values() if type(subSet) is not SubSet]) == 0, "subsDict must be a mapping to SubSets"
        
        exprSubSet = SubSet({expr})
        for exprSubKey in subsDict:
            if exprSubKey in usedExprKeys:
                continue
            usedExprKeys.add(exprSubKey)

            exprSubSet = SubSet.join(
                self._recursiveSubstitute(
                    expr.subs(exprSubKey, subForExprSubKey),
                    subsDict,
                    usedExprKeys,
                )
                for expr in exprSubSet
                for subForExprSubKey in subsDict[exprSubKey]
            )

            usedExprKeys.remove(exprSubKey)
        return exprSubSet

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
                    symbolNumSubSet = SubSet(sympy.solveset(relationForExprToSub, symbol))
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
        return resultSubSet

    def _subExprUsingAllRoutesForSymbol(self, exprToSub, exprEqSubSet, symbolToSub, allSymbolicSubs):
        allSymbolSubs = {
            symbol: allSymbolicSubs[symbol]
            for symbol in allSymbolicSubs
            if type(symbol) is sympy.Symbol
        }
        
        symbolSubsKnown = symbolToSub in allSymbolicSubs
        if symbolSubsKnown:
            usedExprKeys = set()
            subExprSet = self._recursiveSubstitute(exprToSub, allSymbolSubs, usedExprKeys)
            assert len([expr for expr in subExprSet if symbolToSub in expr.atoms()]) == 0, "Substituting didn't eliminate variable"
            yield subExprSet
        else:
             # dictionary changes; we have to memorize the keys to iterate
            exprKeys = [key for key in allSymbolicSubs.keys()]
            for exprKey in exprKeys:
                # no point in substituting the symbol if it isn't even in the expression
                if symbolToSub not in exprKey.free_symbols:
                    continue
                exprKeyEqSubSet = allSymbolicSubs[exprKey]
                
                symbolSubs = SubSet.join(
                    self._solveRelationForSymbol(exprKey - numericEqToExprKey, symbolToSub)
                    for numericEqToExprKey in exprKeyEqSubSet
                )
                relationsToSub = {
                    exprToSub - numericEqToExprToSub
                    for numericEqToExprToSub in exprEqSubSet
                }
                # you know how you need 3 unique equations to solve a syatem of 3 variables?
                # this is what this is trying to determine...
                if self._exprProvidesUniqueSolution(symbolSubs, relationsToSub, symbolToSub):
                    allSymbolSubs[symbolToSub] = allSymbolicSubs[symbolToSub] = symbolSubs
                    usedExprKeys = set()
                    subExprSet = self._recursiveSubstitute(exprToSub, allSymbolSubs, usedExprKeys)
                    assert len([expr for expr in subExprSet if symbolToSub in expr.free_symbols]) == 0, "Substituting didn't eliminate variable"
                    yield subExprSet

    def _exprProvidesUniqueSolution(self, symbolSolvesForExprInQuestion, baseRelations, symbolSolvedFor):
        for symbolSub in symbolSolvesForExprInQuestion:
            for relation in baseRelations:
                solutionIsRedundant = relation.subs(symbolSolvedFor, symbolSub) == 0
                if solutionIsRedundant:
                    return False
        return True

    def _solveRelationForSymbol(self, relation, symbol):
        symbolSubs = sympy.solveset(relation, symbol)
        if type(symbolSubs) is sympy.Complement:
            # this happens when an expression was originally solved into the denominator;
            # just ignore the undefined point
            symbolSubs = symbolSubs.args[0]
        return SubSet(symbolSubs)

    def _filterNonNumericSubSets(self, subDict):
        for exprKey in subDict:
            assert isinstance(exprKey, sympy.Expr), "subDict did not have sympy Expr keys"
            subSet = subDict[exprKey]
            assert type(subSet) is SubSet, "subDict did not contain SubSet values"
            if subSet.isNumericSet:
                yield (exprKey, subSet)

    # TODO: remove unneeded functions below (if they really are unneeded):
    def _subUntilFixed(self, expr, subCombo):
        lastExpr = None
        iters = 0
        while lastExpr != expr:
            lastExpr = expr
            expr = expr.subs(subCombo)
            
            iters += 1
            assert iters < 10000, "_subUntilFixed() stuck in infinite loop..."
        return expr

    def _subCombos(self):
        for combo in self._subCombos_nextRec(dict()):
            yield combo

    def _subCombos_nextRec(self, comboDict):
        currSymbol = None
        for someSymbol in self._subCombos_symbolKeys():
            someSymbolUsedAlready = someSymbol in comboDict
            if not someSymbolUsedAlready:
                currSymbol = someSymbol
                break

        if currSymbol is None:
            yield comboDict
        else:
            associatedSet = self.getKnown(currSymbol)
            assert type(associatedSet) is SubSet, "A non-SubSet substitution made its way into the AlgebraMaster..."
            for exprSub in associatedSet:
                comboDict[currSymbol] = exprSub
                for combo in self._subCombos_nextRec(comboDict):
                    yield combo
            assert len(associatedSet) > 0, "Cannot have empty numeric set"
            comboDict.pop(currSymbol)

    def _subCombos_symbolKeys(self):
        for symbolKey in self._definedSubstitutions:
            yield symbolKey
        for symbolKey in self._inferredSubstitutions:
            yield symbolKey

    def _subSymToNumericIfPossible(self, symbolToSub, usedSymbols):
        if self.isKnown(symbolToSub):
            numericSet = self.getKnown(symbolToSub)
            assert type(numericSet) is SubSet, "getKnown() did not return a SubSet"
            assert numericSet.isNumericSet, "getKnown() returned a SubSet that had non-numerics"
            return numericSet
        else:
            numericSet = SubSet()
            symbolSubSet = self._getSolutionsForSymbol(symbolToSub)
            assert symbolSubSet.isExpressionSet, "solutionSet should not contain numerics"
            for symbolSub in symbolSubSet:
                isCircSub = False
                for symbol in symbolSub.free_symbols:
                    if symbol in usedSymbols:
                        isCircSub = True
                        break
                if not isCircSub:
                    possibleNumericSet = self._subToNumericIfPossible(symbolSub, usedSymbols)
                    if possibleNumericSet.isNumericSet:
                        numericSet.addFrom(possibleNumericSet)
            return numericSet

    def _getSolutionsForSymbol(symbol):
        pass

    def _solveSingle(self, leftExpr, rightExpr, forSymbol):
        assert isinstance(leftExpr, sympy.Expr), "_solve() not given correct arguments (leftExpr)"
        assert isinstance(rightExpr, sympy.Expr), "_solve() not given correct arguments (rightExpr)"
        assert isinstance(forSymbol, sympy.Symbol), "_solve() not given correct arguments (forSymbol)"

        eqZeroExpr = leftExpr - rightExpr
        solutions = sympy.solve(eqZeroExpr, forSymbol)
        # some assumptions I made after (a little) testing
        assert type(solutions) is list, "_solve() solutions are given in list format"
        assert len([sol for sol in solutions if not isinstance(sol, sympy.Expr)]) == 0, "_solve() solutions are all sympy.Expr instances"
        return solutions


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

    (a, b, c) = sympy.symbols("a, b, c")
    ans = {
        a: sympy.simplify(4),
        b: sympy.simplify(7),
        c: sympy.simplify(-5),
    }
    master.relate(a + b + c, ans[a] + ans[b] + ans[c])
    master.relate(a * c + 2 * b, ans[a] * ans[c] + 2 * ans[b])
    master.relate(c * (b - a), ans[c] * (ans[b] - ans[a]))
    master = master
