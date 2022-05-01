import sympy

from structures import *


class AlgebraMaster:
    def __init__(self):
        self._definedSubstitutions = dict()
        self._inferredSubstitutions = dict()
        self._relationsEqZero = set()

    def substitute(self, expr):
        if isNumeric(expr):
            return SubSet({expr})
        
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        usedSymbols = set()
        return self._subToNumericIfPossible(expr, usedSymbols)

    def define(self, symbols, vals):
        assert type(symbols) in (tuple, list), "define() requires list or tuple of symbols"
        assert len([symbol for symbol in symbols if type(symbol) not in (sympy.Symbol, Identifier)]) == 0, "symbols must be list/tuple of sympy Symbols or Identifiers"
        assert type(vals) is SubSet, "define() requires vals to be a SubSet"
        
        symbols = self._identifiersToSymbols(symbols)
        subVals = SubSet.join(self.substitute(val) for val in vals)
        if not subVals.isNumericSet:
            raise NotANumericException()
        self._definedSubstitutions.update({symbol: subVals for symbol in symbols})

    def getDefinition(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "getDefinition() can only work for sympy Symbols and Identifiers"
        symbol = self._identifiersToSymbols([symbol])[0]
        return self._definedSubstitutions.get(symbol)

    def isDefined(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "isDefined() can only work for sympy Symbols and Identifiers"
        symbol = self._identifiersToSymbols([symbol])[0]
        return symbol in self._definedSubstitutions

    def _identifiersToSymbols(self, identifiersOrSymbols):
        # function exists purely for syntactical purposes
        def raiseInvalidType():
            assert "this" == "invalid type", "List must contain Identifiers and sympy Symbols; nothing else"
        
        return [
            identifierToSymbol(symbol) if type(symbol) is Identifier
            else symbol if type(symbol) is sympy.Symbol
            else raiseInvalidType()
            for symbol in identifiersOrSymbols
        ]

    def relate(self, leftExpr, rightExpr):
        assert type(leftExpr) is sympy.Expr, "Cannot relate left side; not a sympy Expr"
        assert type(rightExpr) is sympy.Expr, "Cannot relate right side; not a sympy Expr"
        exprEqZero = leftExpr - rightExpr
        self._relationsEqZero.add(exprEqZero)
        # TODO: update self._inferredSubstitutions

    def getInference(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "getInference() can only work for sympy Symbols and Identifiers"
        symbol = self._identifiersToSymbols([symbol])[0]
        return self._inferredSubstitutions.get(symbol)

    def isInferred(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "isInferred() can only work for sympy Symbols and Identifiers"
        symbol = self._identifiersToSymbols([symbol])[0]
        return symbol in self._inferredSubstitutions

    def getKnown(self, symbol):
        return self.getDefinition(symbol) or self.getInference(symbol)

    def isKnown(self, symbol):
        return self.isDefined(symbol) or self.isInferred(symbol)

    def exists(self, symbol):
        pass # TODO

    # substitution helper methods
    def _subToNumericIfPossible(self, expr, usedSymbols):
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        exprSubSet = SubSet({expr})
        for symbolToSub in expr.free_symbols:
            if symbolToSub in usedSymbols:
                continue
            usedSymbols.add(symbolToSub)

            symbolNumericSet = self._subSymToNumericIfPossible(symbolToSub, usedSymbols)
            assert symbolNumericSet.isNumericSet, "symbolNumericSet must be a numeric SubSet"
            if len(symbolNumericSet) > 0:
                exprSubSet = SubSet({
                    currExpr.subs(symbolToSub, symbolSub)
                    for currExpr in exprSubSet
                    for symbolSub in symbolNumericSet
                })
            
            usedSymbols.remove(symbolToSub)
        return exprSubSet

    def _subSymToNumericIfPossible(self, symbolToSub, usedSymbols):
        if self.isKnown(symbolToSub):
            numericSet = self.getKnown(symbolToSub)
            assert type(numericSet) is SubSet, "getKnown() did not return a SubSet"
            assert numericSet.isNumericSet, "getKnown() returned a SubSet that had non-numerics"
            return numericSet
        # DEBUG
        elif symbolToSub == sympy.Symbol('c'):
            return SubSet({-1, 2})
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
        pass # TODO

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


if __name__ == "__main__":
    # testMode = parseInt(input("Enter AlgebraMaster test mode: "))
    testMode = 1
    if testMode == 1:
        a = AlgebraMaster()
        a._substitutions.update({
            sympy.Symbol('a'): SubSet({1, 2}),
            sympy.Symbol('b'): SubSet({3, 4}),
            sympy.Symbol('c'): SubSet({5, 6, 7}),
        })
        for combo in a._subCombos():
            print(combo)
