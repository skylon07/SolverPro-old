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

    def substituteByElimination(self, expr, forSymbol):
        assert isinstance(expr, sympy.Expr), "substituteByElimination() requires a sympy Expr"
        assert type(forSymbol) is sympy.Symbol, "substituteByElimination() needs a sympy Symbol for the variable being solved"
        assert forSymbol in expr.free_symbols, "substituteByElimination() cannot help infer when the new symbol is not in the expression"
        assert not any(forSymbol in subDict for subDict in self._subDictList), "substituteByElminination() is meant to help find a symbol not currently in any of the substitution dictionaries"
        
        resultList = SubDictList([
            SubDict({expr: self._subDictUntilFixed(expr, subDict)}, subDict.conditions)
            for subDict in self._subDictList
        ])
        if __debug__:
            allSymbols = {symbol for subDict in self._subDictList for subDictExprKey in subDict.keys() for symbol in subDictExprKey.free_symbols}
            resultExprs = (resultExpr for resultDict in resultList for resultExpr in [resultDict[expr]])
            assert not any(symbol in resultExpr.free_symbols for resultExpr in resultExprs for symbol in allSymbols), "Elimination-substitution requires a dict with expression keys with unidirectional dependencies (can't have {a: b + c, b: a * c}, but CAN have {a: b + c, b: 2 * c})"
        return resultList

    def backSubstitute(self, forSymbol):
        assert type(forSymbol) is sympy.Symbol, "backSubstitute() must be given a symbol to substitute for"
        assert all(forSymbol in subDict for subDict in self._subDictList), "backSubstitute() requires that the symbol has a substitution present in all SubDicts"
        resultList = SubDictList([
            SubDict({forSymbol: self._subDictUntilFixed(symbolSubExpr, subDict)}, subDict.conditions)
            for subDict in self._subDictList
            for symbolSubExpr in [subDict[forSymbol]]
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

    def solveSymbolsByBackSubstitution(self):
        assert self._baseRelationalSubs is not None, "Solving by back substitution requires relational substitutions to have been made already (use extractGivenRelationsNumerics() first)"
        assert self._subDictList is None, "Should not invoke solveSymbolsByBackSubstitution() more than once"
        exprKeyOrder = self._exprKeysSortedByIndependence()
        if __debug__:
            origExprKeyOrder = list(exprKeyOrder)
        self._subDictList = SubDictList(
            subDict
            for subDict in self._recursiveSolveThenBackSubstitute(exprKeyOrder)
            # not an actual filter; this is done just for the side-effect
            if subDict.update(self._baseRelationalSubs) is None
        )
        assert exprKeyOrder == origExprKeyOrder, "back substitution algorithm should end up returning expr order stack to its original state"
        return self

    def _exprKeysSortedByIndependence(self):
        # this ensures that variables needing to be solved don't get "trapped"
        # trying to use an expression they don't exist in
        # (a -- a..2b..2c)
        # (b -- 2a..b..3c)
        # (c -- 2a..4b -- TRAPPED)
        # TODO: will this work with multiple relationships?
        #       ex. a + b = 2; a + b + c = 5;             c + d + e = 12;      d + e = 7
        #                              ^ evaluated here   ^ can't solve here   ^ ...and therefore here
        # DEBUG:
        return sorted(self._baseRelationalSubs.keys(), key=lambda expr: len(expr.free_symbols))
        # return [(2*a + b)/(c + 1), sympy.Mul(-1, (-b + c + 1)/a), a + b*c - b, (a + b*c - 5)/b, a + b + c]
    
    def _recursiveSolveThenBackSubstitute(self, exprKeys, symbolSubs=None):
        if symbolSubs is None:
            symbolSubs = SubDict()

        relationProvidesNewInformation = False
        poppedItemsFromExprKeys = []
        while not relationProvidesNewInformation:
            if len(exprKeys) == 0:
                finalSubDict = symbolSubs
                yield finalSubDict
                for item in reversed(poppedItemsFromExprKeys):
                    exprKeys.insert(0, item)
                return
            currExprKey = exprKeys.pop(0)
            poppedItemsFromExprKeys.append(currExprKey)

            currSymbol = self._findSymbolToSolve(currExprKey, symbolSubs)
            cantInferFromExprKey = currSymbol is None
            # DEBUG
            global DEBUG_solveList
            DEBUG_solveList.append((currSymbol, currExprKey, SubDict(symbolSubs)))
            if cantInferFromExprKey:
                # TODO: this code is copy-pasted; perhaps refactoring it could make it nicer?
                finalSubDict = symbolSubs
                yield finalSubDict
                for item in reversed(poppedItemsFromExprKeys):
                    exprKeys.insert(0, item)
                return
            
            eqNumeric = self._baseRelationalSubs[currExprKey]
            # TODO: use Substituter.substituteByElimination() (it'll need to be repurposed...)
            relation = Substituter(SubDictList())._subDictUntilFixed(currExprKey - eqNumeric, symbolSubs)
            simplifiedRelation = sympy.simplify(relation)
            relationProvidesNewInformation = simplifiedRelation != 0

        solution = self._solveSet(simplifiedRelation, currSymbol)
        # DEBUG
        DEBUG_solveList.append(("solution:", solution))
        if solution.type is solution.types.NORMAL:
            symbolSolutions = solution.set
        elif solution.type is solution.types.COMPLEXES:
            # this means a variable can be any value and still hold true in the relation,
            # which ultimately means the relation provided no new information for the symbol
            symbolSolutions = set()
        elif solution.type is solution.types.EMPTY:
            # similar to the above case, except we ended up with some kind of
            # 4 = 0 case (say, 1 + 3(b + 1)/(b + 1))
            symbolSolutions = set()
        elif solution.type is solution.types.COMPLEMENT:
            if solution.set.args[0] is not sympy.Complexes:
                # this happens when the solver is forced to put a variable in the denominator of a fraction;
                # this generates a solution with (calculus) "holes", which can be ignored
                # (happens in cases like solving a / b - 4 for b)
                symbolSolutions = solution.set.args[0]
            elif solution.set.args[0] is sympy.Complexes:
                # this happens in cases similar to the above, except sort of reversed;
                # here, the "holes" are actually the solutions, since graphically any value is
                # included in the complement set (not sure what an example case is though...)
                symbolSolutions = solution.set.args[1]
        else:
            raise NotImplementedError("solveSymbolsByBackSubstitution() didn't handle the solution type '{}'".format(solution.type))

        if len(symbolSolutions) == 0:
            # because no branching occurs, we can simply reuse the sub dict/conditions set
            nextSymbolSubs = symbolSubs
            for finalSubDict in self._recursiveSolveThenBackSubstitute(exprKeys, nextSymbolSubs):
                yield finalSubDict
        elif len(symbolSolutions) == 1:
            # because no branching occurs, we can simply reuse the sub dict/conditions set
            nextSymbolSubs = symbolSubs
            nextSymbolSubs[currSymbol] = first(symbolSolutions)
            for finalSubDict in self._recursiveSolveThenBackSubstitute(exprKeys, nextSymbolSubs):
                # TODO: use Substituter.backSubstitute() (it'll need to be repurposed...)
                finalSubDict[currSymbol] = finalSubDict[currSymbol].subs(finalSubDict)
                yield finalSubDict
        else:
            for symbolSub in symbolSolutions:
                # a copy of the dict must be made to track each path
                nextSymbolSubs = SubDict(symbolSubs)
                nextSymbolSubs[currSymbol] = symbolSub
                # conditions only matter when a solution "branches"
                nextSymbolSubs.conditions.add(currSymbol - symbolSub)
                for finalSubDict in self._recursiveSolveThenBackSubstitute(exprKeys, nextSymbolSubs):
                    # TODO: use Substituter.backSubstitute() (it'll need to be repurposed...)
                    finalSubDict[currSymbol] = finalSubDict[currSymbol].subs(finalSubDict)
                    yield finalSubDict

        for item in reversed(poppedItemsFromExprKeys):
            exprKeys.insert(0, item)

    def _findSymbolToSolve(self, expr, symbolSubDict):
        # DEBUG
        val = first(iterDifference(expr.free_symbols, symbolSubDict.keys()), None)
        # val = [b, a, a, c, c, None][self._exprKeysSortedByIndependence().index(expr)]
        DEBUG_solveList.append(("find symbol:", val))
        return val

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
    DEBUG_solveList = []
    S = sympy.Symbol
    # master = AlgebraMaster()

    (a, b, c, d) = sympy.symbols("a, b, c, d")

    # a + b + c = 6     a = 1
    # 2*a + b - c = 1   b = 2
    # a - b + 2*c = 5   c = 3

    # a * c - b  -  5,  a =  1, -3
    # a ** 2 + c  -  4, b = -2, 10
    # a + b + c  -  2,  c =  3, -5

    # solutions = Solver() \
    #     .extractGivenRelationalNumerics({}) \
    #     .solutions
    # solutions = Solver() \
    #     .extractGivenRelationalNumerics({
    #         a + b + c  -  6,
    #     }) \
    #     .solutions

    solver = Solver()
    DEBUG_solveList = [solver]
    solutions = solver \
        .extractGivenRelationalNumerics({
            a + b + c  -  6,
            2 * a + b - c  -  1,
            a - b + b * c  -  5,
        }) \
        .solveSymbolsByBackSubstitution() \
        .solutions

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
