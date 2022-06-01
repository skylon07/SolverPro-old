import sympy

from structures import *


class Substituter:
    def __init__(self, subDictList):
        assert type(subDictList) is SubDictList, "Substituter requires a SubDictList"
        self._subDictList = subDictList

    def substituteToNumerics(self, expr):
        assert isinstance(expr, sympy.Expr), "substituteToNumerics() requires a sympy Expr"
        assert all(isNumeric(val) for subDict in self._subDictList for val in subDict.values()), "Substituter sub dicts' values must be numerics when substituting to numerics"
        if isNumeric(expr):
            return SubDictList([SubDict({expr: expr})])
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        
        resultList = SubDictList([
            SubDict({expr: self._subDictUntilFixed(expr, subDict)}, subDict.conditions)
            for subDict in self._subDictList
        ])
        return resultList

    # other utility functions

    def _subDictUntilFixed(self, expr, subDict):
        assert expr is not None, "cannot sub a nonexistant expression"
        lastExpr = None
        iters = 0
        while lastExpr is not expr:
            lastExpr = expr
            expr = expr.subs(subDict)
            iters += 1
            assert iters < 9999, "Substitution probably should have stopped by now..."
        return expr

class Solver:
    def __init__(self):
        self._baseRelationalSubs = None
        self._subDictList = None

    # STEP 1 fns: solve numerics already present in the relations

    def extractGivenRelationalNumerics(self, relations):
        assert self._baseRelationalSubs is None, "Should not invoke extractGivenRelationalNumerics() more than once"
        self._baseRelationalSubs = SubDict()
        for relation in relations:
            # negative because the numeric in x + 4 (= 0) is a +4,
            # however the actual solution is x = -4 (the opposite of the found atom)
            for negEqNumeric in self._findSolvableNumerics(relation):
                eqNumeric = -negEqNumeric
                solution = self._solveSet(relation, eqNumeric)
                if solution.type is solution.types.NORMAL:
                    solutionSet = solution.set
                else:
                    raise NotImplementedError("extractGivenRelationalNumerics() didn't handle the solution type '{}'".format(solution.type))

                for exprKey in solutionSet:
                    self._baseRelationalSubs[exprKey] = eqNumeric
        return self

    def _findSolvableNumerics(self, expr):
        if isNumeric(expr):
            yield expr
        elif expr.is_Atom:
            pass
        elif expr.is_Add or expr.is_Mul:
            for term in expr.args:
                for numeric in self._findSolvableNumerics(term):
                    yield numeric
        elif expr.is_Pow:
            pass # we ignore solving for numerics in power terms
        else:
            raise NotImplementedError("Unconsidered expression case when finding solvable numerics")

    # STEP 2 fns: back substitute relations to find symbol solutions

    # TODO

    # UTILITY FUNCTIONS

    @property
    def solutions(self):
        if self._subDictList is not None:
            return self._subDictList
        elif self._baseRelationalSubs is not None:
            return SubDictList([self._baseRelationalSubs])
        else:
            return None

    def _solveSet(self, expr, atom, _checkNumeric=True):
        if isNumeric(atom) and _checkNumeric:
            # solving for variables in exponents leads to errors with no clean fixes,
            # so we just ignore them by converting them to symbols first
            numericSub = sympy.Symbol(":NUMERIC {}:".format(atom))
            exprWithNumericSubbed = self._subsNumericInBadTerms(atom, numericSub, expr)
            solution = self._solveSet(exprWithNumericSubbed, atom, _checkNumeric=False)
            solution.reverseSubstitutionMade({numericSub: atom})
            return solution
        else:
            solutionSet = sympy.solveset(expr, atom)
            return self._Solution(solutionSet)

    def _subsNumericInBadTerms(self, numeric, numericSub, expr):
        isBadExpr = expr.is_Pow
        if isBadExpr:
            return expr.subs(numeric, numericSub)
        elif expr.is_Atom:
            return expr
        else:
            return expr.func(*(
                self._subsNumericInBadTerms(numeric, numericSub, term)
                for term in expr.args
            ))

    class _Solution:
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
    # master = AlgebraMaster()

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
