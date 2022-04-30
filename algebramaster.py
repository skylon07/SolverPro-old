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

    def define(self, ids, vals):
        assert type(ids) in (tuple, list), "define() requires list or tuple of ids"
        assert len([identifier for identifier in ids if type(identifier) is not Identifier]) == 0, "ids must be list/tuple of Identifiers"
        assert type(vals) is SubSet, "define() requires a SubSet of vals"
        
        subVals = SubSet.join(self.substituteKnown(val) for val in vals)
        if not subVals.isNumeric:
            raise NotANumericException()
        self._numericSubstitutions.update({identifier: subVals for identifier in ids})

    def getDefinition(self, identifier):
        assert type(identifier) is Identifier, "getDefinition() can only work for Identifiers"
        return self._numericSubstitutions.get(identifier)

    def isDefined(self, identifier):
        assert type(identifier) is Identifier, "isDefined() can only work for Identifiers"
        return identifier in self._numericSubstitutions

    def getUndefinedSymbols(self, expr, yieldIdentifiers=False):
        assert isinstance(expr, sympy.Expr), "Can only get undefined symbols for Sympy expressions"
        for symbol in expr.free_symbols:
            identifier = symbolToIdentifier(symbol)
            if self.isDefined(identifier):
                if yieldIdentifiers:
                    yield identifier
                else:
                    yield symbol

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
        currKey = None
        currSymbol = None
        for someExprKey in self._numericSubstitutions:
            trySymbol = identifierToSymbol(someExprKey)
            someExprKeyUsedAlready = trySymbol in comboDict
            if not someExprKeyUsedAlready:
                currKey = someExprKey
                currSymbol = trySymbol
                break

        if currSymbol is None:
            yield comboDict
        else:
            associatedSet = self._numericSubstitutions[currKey]
            assert type(associatedSet) is SubSet, "A non-SubSet substitution made its way into the AlgebraMaster..."
            for exprSub in associatedSet:
                comboDict[currSymbol] = exprSub
                for combo in self._subCombos_nextRec(comboDict):
                    yield combo
            comboDict.pop(trySymbol)


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
