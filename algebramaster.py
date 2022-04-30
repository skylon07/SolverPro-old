import sympy

from structures import *


class AlgebraMaster:
    def __init__(self):
        self._numericSubstitutions = dict()

    def substituteKnown(self, expr):
        if isNumeric(expr):
            return SubSet({expr})
        
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        return SubSet({
            self._subUntilFixed(expr, subCombo)
            for subCombo in self._subCombos()
        })

    def define(self, symbols, vals):
        assert type(symbols) in (tuple, list), "define() requires list or tuple of symbols"
        assert len([symbol for symbol in symbols if type(symbol) not in (sympy.Symbol, Identifier)]) == 0, "symbols must be list/tuple of sympy Symbols or Identifiers"
        assert type(vals) is SubSet, "define() requires vals to be a SubSet"
        
        symbols = self._identifiersToSymbols(symbols)
        subVals = SubSet.join(self.substituteKnown(val) for val in vals)
        if not subVals.isNumeric:
            raise NotANumericException()
        self._numericSubstitutions.update({symbol: subVals for symbol in symbols})

    def getDefinition(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "getDefinition() can only work for sympy Symbols and Identifiers"
        return self._numericSubstitutions.get(symbol)

    def isDefined(self, symbol):
        assert type(symbol) in (sympy.Symbol, Identifier), "isDefined() can only work for sympy Symbols and Identifiers"
        return symbol in self._numericSubstitutions

    def getUndefinedSymbols(self, expr):
        assert isinstance(expr, sympy.Expr), "Can only get undefined symbols for Sympy expressions"
        for symbol in expr.free_symbols:
            if not self.isDefined(symbol):
                yield symbol

    def _identifiersToSymbols(self, identifiersOrSymbols):
        # function exists purely for syntactical purposes
        def raiseInvalidType():
            assert "this" == "invalid type", "List must contain Identifiers and sympy Symbols; nothing else"
        
        return [
            identifierToSymbol(symbol) if type(symbol) is Identifier
            else symbol if type(symbol) is sympy.Symbol()
            else raiseInvalidType()
            for symbol in identifiersOrSymbols
        ]

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
        for someSymbol in self._numericSubstitutions:
            someSymbolUsedAlready = someSymbol in comboDict
            if not someSymbolUsedAlready:
                currSymbol = someSymbol
                break

        if currSymbol is None:
            yield comboDict
        else:
            associatedSet = self.getDefinition(currSymbol)
            assert type(associatedSet) is SubSet, "A non-SubSet substitution made its way into the AlgebraMaster..."
            for exprSub in associatedSet:
                comboDict[currSymbol] = exprSub
                for combo in self._subCombos_nextRec(comboDict):
                    yield combo
            comboDict.pop(currSymbol)


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
