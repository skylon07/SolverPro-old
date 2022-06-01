import sympy

from structures import *


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

    (a, b, c, d) = sympy.symbols("a, b, c, d")
    def dictIncludes(mainDict, shouldContainDict):
        dictDoesNotContainItem = object()
        for shouldContainKey in shouldContainDict:
            shouldContainVal = shouldContainDict[shouldContainKey]
            mainVal = mainDict.get(shouldContainKey, dictDoesNotContainItem)
            if type(shouldContainVal) is SubSet and shouldContainVal.isNumericSet:
                try:
                    shouldFloat = SubSet(
                        float(shouldHaveSub.expr)
                        for shouldHaveSub in shouldContainVal
                    )
                    mainFloat = SubSet(
                        float(mainSub.expr)
                        for mainSub in mainVal
                    )
                    mainDictContainsVal = shouldFloat == mainFloat
                except TypeError:
                    return False
            else:
                mainDictContainsVal = mainVal == shouldContainVal
            if not mainDictContainsVal:
                return False
        return True

    # a + b + c = 6     a = 1
    # 2*a + b - c = 1   b = 2
    # a - b + 2*c = 5   c = 3

    # a * c - b  -  5,  a =  1, -3
    # a ** 2 + c  -  4, b = -2, 10
    # a + b + c  -  2,  c =  3, -5
    s = Substituter({

    }).substituteByElimination(a*c - b - 5, b)
    s = Substituter({
        b: SubSet({a*c - 5}),
    }).substituteByElimination(a**2 + c - 4, a)
    s = Substituter({
        b: SubSet({a*c - 5}),
        a: SubSet({-sympy.sqrt(4 - c), sympy.sqrt(4 - c)})
    }).substituteByElimination(a + b + c - 2, c)

    subber = Substituter({
        b: SubSet({a*c - 5}),
        a: SubSet({-sympy.sqrt(4 - c), sympy.sqrt(4 - c)}),
        c: SubSet({SubSet.Sub(3, {-a*c + b + 5, a - sympy.sqrt(4 - c)}), SubSet.Sub(-5, {-a*c + b + 5, a + sympy.sqrt(4 - c)})})
    })
    s = subber.backSubstitute()

    solutions = Solver() \
        .extractGivenRelationalNumerics({
            a + 2 * b + c  -  0,
            3 * a - b + 2 * c  -  11,
            -a + b - c  -  -6,
        }) \
        .solveSymbolsByBackSubstitution() \
        .substitutions
    assert dictIncludes(solutions, {
        a: SubSet({1}),
        b: SubSet({-2}),
        c: SubSet({3}),
    })

    solutions = Solver({c ** 2 - 4}).genNumericSolutions()
    assert dictIncludes(solutions, {
        c: SubSet({2, -2}),
    })

    solutions = Solver({d ** -2 - 1/4}).genNumericSolutions()
    assert dictIncludes(solutions, {
        d: SubSet({2, -2}),
    })

    solutions = Solver({
        a + b  -  -1,
        a ** 2 + b  -  5,
    }).genNumericSolutions()
    assert dictIncludes(solutions, {
        a: SubSet({3, -2}),
        b: SubSet({-4, 1}),
    })

    solutions = Solver({
        (a * c) - b  -  5,
        a ** 2 + c  -  4,
        a + b + c  -  2,
    }).genNumericSolutions()
    assert dictIncludes(solutions, {
        a: SubSet({1, -3}),
        b: SubSet({-2, 10}),
        c: SubSet({3, -5}),
    })
    
    solutions = Solver({
        (a * c) - b  -  5,
        (c ** 2 - a) / b  -  -4,
        a + b + c  -  2,
    }).genNumericSolutions()
    assert dictIncludes(solutions, {
        a: SubSet({1}),
        b: SubSet({-2}),
        c: SubSet({3}),
    })

    # TODO: add more tests using square roots

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
