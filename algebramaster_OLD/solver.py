import sympy

from src.common.functions import *

from .structures import *
from .substitution import *


class AlgebraSolver:
    def __init__(self):
        self._universes = SubDictList([SubDict()])
        self._unsolvableRelations = []

    @property
    def universes(self):
        return self._universes

    def recordRelation(self, relation):
        foundAnyNumerics = self._extractNumericsFromRelation(relation)
        if foundAnyNumerics:
            self._addBackSubstitutedInferences()
            self._attemptReconcileForUnsolvedRelations()
        else:
            self._unsolvableRelations.append(relation)
    
    def _extractNumericsFromRelation(self, relation):
        foundAnyNumerics = False
        # negative because the numeric in x + 4 (= 0) is a +4,
        # however the actual solution is x = -4 (the opposite of the found atom);
        # in other words, if a - 5 = 0, the atom is -5 even though a = 5
        for negEqNumeric in self._findSolvableNumerics(relation):
            foundAnyNumerics = True
            eqNumeric = -negEqNumeric
            solutions = _SympySolveTools.solveSet(relation, eqNumeric)
            for exprKey in solutions:
                for universe in self._universes:
                    # TODO: check for existing exprKeys; do they contradict?
                    universe[exprKey] = eqNumeric
        return foundAnyNumerics

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
            pass # we ignore solving for numerics in power terms (see note in _SympySolveTools)
        else:
            raise NotImplementedError("Unconsidered expression case when finding solvable numerics")

    def _addBackSubstitutedInferences(self):
        for universeDict in self._universes:
            newSolutionsList = _BackSubstituterSolver(universeDict).getSolutions()
            assert len(newSolutionsList) > 0, "Getting solutions should give back at least one dict (even if it's empty)"
            if len(newSolutionsList) == 1:
                newSolutions = newSolutionsList[0]
                assert newSolutions.conditions == universeDict.conditions, "One solution dict should imply no branching (and therefore no new conditions) occurred"
                newNumericSolutions = self._onlyNumericSubs(newSolutions)
                if len(newNumericSolutions) > 0:
                    emplaceInto(newNumericSolutions, universeDict)
            else:
                self._universes.remove(universeDict)
                for newSolutions in newSolutionsList:
                    newNumericSolutions = self._onlyNumericSubs(newSolutions)
                    if len(newNumericSolutions) > 0:
                        newUniverseDict = SubDict(universeDict)
                        emplaceInto(newNumericSolutions, newUniverseDict)
                        newUniverseDict.conditions = newSolutions.conditions
                        self._universes.append(newUniverseDict)

    def _onlyNumericSubs(self, subDict):
        return SubDict({
            expr: numeric
            for (expr, numeric) in subDict.items()
            if isNumeric(numeric)
        })

    def _attemptReconcileForUnsolvedRelations(self):
        reattemptReconcile = False
        for relationWithoutNumerics in self._unsolvableRelations:
            removeOrigRelation = False
            relationMaybeWithNumerics_dictList = substituteAllKnowns(relationWithoutNumerics, self._universes)
            for relationMaybeWithNumerics_dict in relationMaybeWithNumerics_dictList:
                relationMaybeWithNumerics = relationMaybeWithNumerics_dict[relationWithoutNumerics]
                foundAnyNumerics = self._extractNumericsFromRelation(relationMaybeWithNumerics)
                if foundAnyNumerics:
                    removeOrigRelation = True
                    reattemptReconcile = True
                    self._addBackSubstitutedInferences()
            
            if removeOrigRelation:
                self._unsolvableRelations.remove(relationWithoutNumerics)
        if reattemptReconcile:
            self._attemptReconcileForUnsolvedRelations()


class _BackSubstituterSolver:
    _copyMode = object()

    """
    This class is tasked with extrapolating solutions for individual variables
    given a single "universe" by a "base numerical substitution dictionary" and
    a list of keys present in the dictionary to extrapolate from.

    A "base numerical substitution dictionary" is a *dictionary* containing
    *substitutions* from expressions to *numerics* that they equal. It is
    considered the *base* set of relations to extrapolate solutions from

    This class will provide valid solutions via `getNewSolutions()` only if
        - the substitutions in the base dictionary represent equalities (ie
            {a + b: 2} implies that a + b = 2)
        - the dictionary's values are all numeric (ie {a + b: 2} is valid, but
            {a: 2 - b} is not)
    """

    def __init__(self, baseNumericalSubs):
        inittingForCopy = baseNumericalSubs is _BackSubstituterSolver._copyMode
        if not inittingForCopy:
            self._baseNumericalSubs = baseNumericalSubs
            self._exprKeyOrder = tuple(self._sortExprKeys(baseNumericalSubs.keys()))
            self._unusedExprKeyIdx = 0
            self._symbolSubs = SubDict()
            self._symbolStack = None

    def _copy(self):
        selfCopy = _BackSubstituterSolver(_BackSubstituterSolver._copyMode)
        selfCopy._baseNumericalSubs = self._baseNumericalSubs
        selfCopy._exprKeyOrder = self._exprKeyOrder
        selfCopy._unusedExprKeyIdx = self._unusedExprKeyIdx
        selfCopy._symbolSubs = SubDict(self._symbolSubs)
        selfCopy._symbolStack = list(self._symbolStack) \
            if self._symbolStack is not None else None
        return selfCopy

    def getSolutions(self):
        if __debug__:
            self._resetSymbolStack()
        return SubDictList(subDict for subDict in self._findSolutions())

    def _sortExprKeys(self, exprKeys):
        """
        this ensures that variables needing to be solved don't get "trapped"
        trying to use an expression they don't exist in; for example:
        
        - solve for a -- a..2b..2c
        - solve for b -- 2a..b..3c
        - solve for c -- 2a..4b -- TRAPPED
        
        what should have happened is something like
        
        - solve for a -- 2a..4b -- not trapped!
        - solve for b -- a..2b..2c
        - solve for c -- 2a..b..3c
        """
        return sorted(exprKeys, key = lambda expr: len(expr.free_symbols))
    
    def _findSolutions(self):
        findRelationResult = self._findNextUsefulRelation()
        usedAllExprKeys = findRelationResult is None
        if usedAllExprKeys:
            finalSubDict = self._symbolSubs
            if __debug__:
                self._copySymbolStackTo(finalSubDict)
            yield finalSubDict
        else:
            (nextUsefulRelation, symbolToSolveFor) = findRelationResult
            if __debug__:
                self._addToSymbolStack(symbolToSolveFor)
            solutionsForSymbol = _SympySolveTools.solveSet(nextUsefulRelation, symbolToSolveFor)
            if len(solutionsForSymbol) == 0:
                # TODO: remove this if it actually never happens (and make an assert for it)
                # assert "this" == "never has to happen"
                for finalSubDict in self._findSolutions():
                    yield finalSubDict
            elif len(solutionsForSymbol) == 1:
                # average case; the soltuion was found and needs to be recorded
                solutionForSymbol = first(solutionsForSymbol)
                self._symbolSubs[symbolToSolveFor] = solutionForSymbol
                for finalSubDict in self._findSolutions():
                    yield self._backSubstitute(finalSubDict, symbolToSolveFor)
            else:
                # "branching" case; happens when things like
                # "solve for a: sqrt(a) = 4" are performed (a = 2 and a = -2)
                for solutionForSymbol in solutionsForSymbol:
                    branchingSolver = self._copy()
                    branchingSolver._symbolSubs[symbolToSolveFor] = solutionForSymbol
                    branchingSolver._symbolSubs.conditions[symbolToSolveFor] = solutionForSymbol
                    for finalSubDict in branchingSolver._findSolutions():
                        yield self._backSubstitute(finalSubDict, symbolToSolveFor)

    def _findNextUsefulRelation(self):
        relationProvidesNewInformation = False
        while not relationProvidesNewInformation and not self._outOfExprKeys:
            nextUnusedExprKey = self._exprKeyOrder[self._unusedExprKeyIdx]
            numericEqToExprKey = self._baseNumericalSubs[nextUnusedExprKey]
            baseRelation = nextUnusedExprKey - numericEqToExprKey
            relationWithoutSolvedSymbols = substituteAllKnowns(baseRelation, self._symbolSubs)[baseRelation]

            relationProvidesNewInformation = relationWithoutSolvedSymbols != 0
            if relationProvidesNewInformation:
                nextUsefulRelation = relationWithoutSolvedSymbols
                symbolToSolveFor = self._findSymbolToSolveFor(nextUsefulRelation)
                
                eliminatedRelation = forwardSubstituteByElimination(nextUsefulRelation, self._symbolSubs, symbolToSolveFor)
                eliminatedRelation = eliminatedRelation[nextUsefulRelation]
                # TODO: document why this needs to happen (and make algorithm
                #       "more robust" by using correct function -- see function
                #       docs for simplify())
                # simplifiedEliminatedRelation = sympy.simplify(eliminatedRelation)
                simplifiedEliminatedRelation = eliminatedRelation
                # TODO: is this check needed?
                # relationProvidesNewInformation = simplifiedEliminatedRelation != 0

            self._unusedExprKeyIdx += 1
        if relationProvidesNewInformation:
            return (simplifiedEliminatedRelation, symbolToSolveFor)
        else:
            return None

    def _findSymbolToSolveFor(self, usefulRelation):
        """
        Like _sortExprKeys(), this function will find the right order to solve
        variables in. If a variable only appears a few times, and it can be
        solved for in the currently useful relation, that variable should be
        solved for this relation to avoid deadlock scenarios down the road
        (since other relations are not guaranteed to have the variable at this
        point).
        """
        
        usedSymbols = set(self._symbolSubs.keys())
        possibleSymbols = set(iterDifference(usefulRelation.free_symbols, usedSymbols))
        assert len(possibleSymbols) > 0
        symbolCount = {symbol: 0 for symbol in possibleSymbols}

        unusedExprKeys = self._exprKeyOrder[self._unusedExprKeyIdx + 1:]
        for unusedExprKey in unusedExprKeys:
            # TODO: could we modify each expr in _exprKeyOrder only once as we
            #       solve new symbols? (perhaps new class that also tracks
            #       _unusedExprKeyIdx automatically?)
            exprKeyWithOnlyUnusedSymbols = substituteAllKnowns(unusedExprKey, self._symbolSubs)[unusedExprKey]
            for unusedSymbol in exprKeyWithOnlyUnusedSymbols.free_symbols:
                if unusedSymbol in possibleSymbols:
                    symbolCount[unusedSymbol] += 1
        return min(symbolCount, key = lambda symbol: symbolCount[symbol])

    def _backSubstitute(self, finalSubDict, symbolToSolveFor):
        backSubResult = backSubstituteByInference(finalSubDict, symbolToSolveFor)
        subForSymbolToSolveFor = backSubResult[symbolToSolveFor]
        newConditions = backSubResult.conditions
        finalSubDict[symbolToSolveFor] = subForSymbolToSolveFor
        finalSubDict.conditions = newConditions
        return finalSubDict

    @property
    def _outOfExprKeys(self):
        assert self._unusedExprKeyIdx <= len(self._exprKeyOrder)
        return self._unusedExprKeyIdx == len(self._exprKeyOrder)

    # stack of when symbols are solved (for debugging purposes)
    def _resetSymbolStack(self):
        self._symbolStack = []

    def _addToSymbolStack(self, symbol):
        assert isinstance(symbol, sympy.Symbol)
        self._symbolStack.append(symbol)

    def _copySymbolStackTo(self, subDict):
        subDict._symbolStack = list(self._symbolStack)

    
class _SympySolveTools:
    class _SolutionTypes:
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
    
    _solutionTypes = _SolutionTypes()

    @classmethod
    def solveSet(cls, expr, atom):
        if isNumeric(atom):
            # sometimes solving for numbers gets weird... so we select the
            # specific numbers we want to solve for by converting them to a
            # different symbol first
            atomSub = sympy.Symbol(f":NUMERIC {atom}:")
            exprWithNumericSubbed = cls._substituteNumeric_avoidBadTerms(atom, atomSub, expr)
            solution = cls._solveSetForAtom(exprWithNumericSubbed, atomSub)
            solution.reverseSubstitutionOfNumeric({atomSub: atom})
        else:
            solution = cls._solveSetForAtom(expr, atom)
        return cls._interpretSolution(solution)

    @classmethod
    def _substituteNumeric_avoidBadTerms(cls, numericAtom, numericAtomSub, expr: sympy.Expr):
        # pow expressions are bad, since solving for numerics in them tends to
        # produce crazy and hard-to-predict results;
        # mul expressions are bad in the case of 1 because then it tries to
        # consider the negative expressions in any atoms
        isBadExpr = expr.is_Pow or (
            abs(numericAtom) == 1 and
            expr.is_Mul and
            expr.args[0] == -1
        )
        if isBadExpr:
            return expr
        elif expr.is_Atom:
            exprIsNumericToSub = expr == numericAtom
            exprIsNegNumericToSub = expr == -numericAtom
            if exprIsNumericToSub:
                return numericAtomSub
            elif exprIsNegNumericToSub:
                return -numericAtomSub
            else:
                return expr
        else:
            # this is a neat way to traverse expression trees as given by
            # https://docs.sympy.org/latest/tutorials/intro-tutorial/manipulation.html#recursing-through-an-expression-tree
            expr: sympy.Expr = expr
            return expr.func(*(
                cls._substituteNumeric_avoidBadTerms(numericAtom, numericAtomSub, term)
                for term in expr.args
            ))

    @classmethod
    def _solveSetForAtom(cls, expr, atom):
        solutionSet = sympy.solveset(expr, atom)
        return cls._Solution(solutionSet)
    
    @classmethod
    def _interpretSolution(cls, solution):
        types = cls._solutionTypes
        if solution.type is types.NORMAL:
            return set(solution.set)
        elif solution.type is types.COMPLEXES:
            # this means a variable can be any value and still hold true in the relation,
            # which ultimately means the relation provided no new information for the symbol
            return None
        elif solution.type is types.EMPTY:
            # similar to the above case, except we ended up with some kind of
            # invalid/impossible relation, a "4 = 0 case" (say, 1 + 3*(b + 1)/(b + 1))
            return set()
        elif solution.type is types.COMPLEMENT:
            if solution.set.args[0] is not sympy.Complexes:
                # this happens when the solver is forced to put a variable in the denominator of a fraction;
                # this generates a solution with (calculus) "holes", which can be ignored
                # (happens in cases like solving a/b - 4 for b)
                return set(solution.set.args[0])
            elif solution.set.args[0] is sympy.Complexes:
                # this happens in cases similar to the above, except sort of reversed;
                # here, the solver found that anything is possible *except* for certain
                # "holes", which are actually the solutions (TODO: example case needed)
                return set(solution.set.args[1])
    
    class _Solution:
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
            types = _SympySolveTools._solutionTypes
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

        def reverseSubstitutionOfNumeric(self, subsDict: dict):
            self._solutionSet = self._performSubsOnSet(self._solutionSet, subsDict)

        def _performSubsOnSet(self, sympySet, subsDict):
            types = _SympySolveTools._solutionTypes
            setType = self._getSetType(sympySet)
            if setType is types.NORMAL:
                returnSet = sympy.FiniteSet(*(
                    expr.subs(subsDict)
                    for expr in sympySet
                ))
            elif setType is types.EMPTY:
                # there is nothing to substitute in the empty set {}
                returnSet = sympySet
            elif setType is types.COMPLEXES:
                # sort of like the above case, except it's everything
                returnSet = sympySet
            elif setType is types.COMPLEMENT:
                (mainSet, complementSet) = sympySet.args
                mainSetSubbed = self._performSubsOnSet(mainSet, subsDict)
                complementSetSubbed = self._performSubsOnSet(complementSet, subsDict)
                return sympy.Complement(mainSetSubbed, complementSetSubbed)
            elif setType is types.CONDITIONAL:
                (symbol, eqCondition, baseSet) = sympySet.args
                assert type(eqCondition) is sympy.Eq, "sympy ConditionSet was assumed to always have Eq() as the second argument"
                (eqLeft, eqRight) = eqCondition.args
                symbolSubbed = symbol.subs(subsDict)
                if isNumeric(symbolSubbed):
                    # ConditionSets really don't like numerics as their first arg... this gets around that
                    # (of course, this means the extractor will need to convert it back later...)
                    # TODO: create NumericSymbol class
                    class NumericSymbol:
                        pass
                    symbolSubbed = NumericSymbol(symbolSubbed)
                eqConditionSubbed = sympy.Eq(eqLeft.subs(subsDict), eqRight.subs(subsDict))
                baseSetSubbed = self._performSubsOnSet(baseSet, subsDict)
                return sympy.ConditionSet(symbolSubbed, eqConditionSubbed, baseSetSubbed)
            else:
                raise NotImplementedError("Tried to substitute the set for an unconsidered solution type")
            
            assert not any(symbolKey in expr.atoms() for expr in returnSet for symbolKey in subsDict)
            return returnSet
