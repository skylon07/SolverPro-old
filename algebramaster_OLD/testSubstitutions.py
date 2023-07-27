import sympy

from src.algebramaster_OLD.substitution import *


(a, b, c, d, e) = sympy.symbols("a, b, c, d, e")

def subToSubDictList(sub):
    return SubDictList([SubDict(sub)])


class SubstituteAllKnownsTester:
    # many tests done in this class should also apply for other functions,
    # but have not been moved to the other classes under the assumption that
    # the other substitution functions use substituteAllKnowns under the hood
    def testSubstitutesVariables(self):
        expr1 = a + b
        subs1 = {
            a: 4,
            b: c,
        }
        result1 = subToSubDictList({expr1: c + 4})
        subResult1 = substituteAllKnowns(expr1, subToSubDictList(subs1))
        assert subResult1 == result1

    def testSubstitutesExpressions(self):
        expr1 = a + b + c + d
        subs1 = {
            a + b: e + 2,
            c + d: 2*e + 4,
        }
        result1 = subToSubDictList({expr1: 3*e + 6})
        subResult1 = substituteAllKnowns(expr1, subToSubDictList(subs1))
        assert subResult1 == result1
    
    def testUsesFixedSubstitution(self):
        expr1 = a*b + c - d
        subs1 = {
            a: b + 3,
            b: c - 1,
            d: b + c,
        }
        result1 = subToSubDictList({expr1: -c + (c - 1)*(c + 2) + 1})
        subResult1 = substituteAllKnowns(expr1, subToSubDictList(subs1))
        assert subResult1 == result1

        expr2 = a + b - c + d - e
        subs2 = {
            a: b - d,
            b: e + d,
            c: a + e,
            e: 3*d,
        }
        result2 = subToSubDictList({expr2: -d})
        subResult2 = substituteAllKnowns(expr2, subToSubDictList(subs2))
        assert subResult2 == result2

    def testCanTakeSubDictDirectly(self):
        expr = a + b + c
        subs = SubDict({
            a + b: 4,
        })
        result = SubDict({expr: c + 4})
        subResult = substituteAllKnowns(expr, subs)
        assert subResult == result

    def testThrowsForBadParameters(self):
        errorThrown1 = False
        try:
            substituteAllKnowns(a + b, [{"is a valid SubDictList": False}])
        except (AssertionError, TypeError):
            errorThrown1 = True
        assert errorThrown1

        errorThrown2 = False
        try:
            substituteAllKnowns("a + b", SubDictList([SubDict({a: 4})]))
        except (AssertionError, TypeError):
            errorThrown2 = True
        assert errorThrown2

        errorThrown3 = False
        try:
            substituteAllKnowns(3, SubDictList([]))
        except Exception:
            errorThrown3 = True
        assert not errorThrown3

    def testCopiesConditions(self):
        expr = a + b + c
        conditions = {a: 2}
        subs = SubDict({
            a + b: 4,
        }, conditions)
        subResult = substituteAllKnowns(expr, subs)
        assert subResult.conditions == conditions


class SubstituteToNumericsTester:
    def testNumericsCanBeSubstituted(self):
        expr1 = a*b + c - d
        subs1 = {
            a: 3,
            b: 1,
            d: 2,
        }
        result1 = subToSubDictList({expr1: c + 1})
        subResult1 = substituteToNumerics(expr1, subToSubDictList(subs1))
        assert subResult1 == result1

    def testNonNumericsErrorWhenSubstituted(self):
        errorThrown1 = False
        try:
            expr1 = a*b + c - d
            subs1 = {
                a: b,
                b: 2,
                d: c,
            }
            substituteToNumerics(expr1, subToSubDictList(subs1))
        except AssertionError or ValueError:
            errorThrown1 = True
        assert errorThrown1


class BackSubstituteByInferenceTester:
    def testCanSubstituteForSymbol(self):
        subs = SubDict({
            a: b - c + d,
            b: c + 2,
            c: 1,
        })
        symbol = a
        result = SubDict({a: d + 2})
        subResult = backSubstituteByInference(subs, symbol)
        assert subResult == result

    def testBackSubstitutesConditions(self):
        conditions = {a: b - c + d, b: c + 2}
        subs = SubDict({
            a: b - c + d,
            b: c + 2,
            c: 1,
        }, conditions)
        symbol = a
        subbedConditions = {a: d + 2, b: c + 2}
        subResult = backSubstituteByInference(subs, symbol)

        subbedConditionsForCurrentSymbol = subResult.conditions[a] == subbedConditions[a]
        assert subbedConditionsForCurrentSymbol
        didNotSubConditionsForCurrentSymbol = subResult.conditions[b] == subbedConditions[b]
        assert didNotSubConditionsForCurrentSymbol
