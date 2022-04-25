import sympy

from structures import *


class AlgebraMaster:
    def __init__(self):
        self._substitutions = dict()

    @classmethod
    def isNumeric(cls, obj):
        return isinstance(obj, (int, float, RoundedFloat, sympy.Number))

    def substituteKnown(self, expr):
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        return SubSet({
            self._subUntilFixed(expr, subCombo)
            for subCombo in self._subCombos()
        })

    def define(self, ids, vals):
        pass # TODO

    def getDefinition(self, identifier):
        pass # TODO

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
        myExprKey = None
        for someExprKey in self._substitutions:
            someExprKeyUsedAlready = someExprKey in comboDict
            if not someExprKeyUsedAlready:
                myExprKey = someExprKey
                break

        if myExprKey is None:
            yield comboDict
        else:
            associatedSet = self._substitutions[myExprKey]
            assert type(associatedSet) is SubSet, "A non-SubSet substitution made its way into the AlgebraMaster..."
            for exprSub in associatedSet:
                comboDict[myExprKey] = exprSub
                for combo in self._subCombos_nextRec(comboDict):
                    yield combo
            comboDict.pop(myExprKey)


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
