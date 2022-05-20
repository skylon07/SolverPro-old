import sympy

from structures import *


class AlgebraMaster:
    def __init__(self):
        self._definedSubstitutions = dict()
        self._inferredSubstitutions = dict()
        self._relationsEqZero = set()
        self._relationSymbols = set()

    def define(self, symbols, vals):
        assert type(symbols) in (tuple, list), "define() requires list or tuple of symbols"
        assert all(type(symbol) in (sympy.Symbol, Identifier) for symbol in symbols), "symbols must be list/tuple of sympy Symbols or Identifiers"
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

    def substitute(self, expr):
        subsDict = dict(self._definedSubstitutions)
        subsDict.update(self._inferredSubstitutions)
        return Substituter(subsDict).substituteToNumerics(expr)

    def _identifiersToSymbols(self, identifiersOrSymbols):
        # function exists purely for syntactical purposes
        def raiseInvalidType():
            raise NotImplementedError("List must contain Identifiers and sympy Symbols/Exprs; nothing else")
        
        return [
            identifierToSymbol(symbol) if type(symbol) is Identifier
            else symbol if isinstance(symbol, sympy.Expr)
            else raiseInvalidType()
            for symbol in identifiersOrSymbols
        ]

    # relational/solving functions
    def _updateInferredSubstitutions(self):
        self._inferredSubstitutions = Solver(self._relationsEqZero, self._definedSubstitutions).genNumericSolutions()


class Substituter:
    def __init__(self, subsDict):
        assert all(isinstance(exprKey, sympy.Expr) for exprKey in subsDict.keys()), "Substitution dictionary must only contain sympy Expr keys"
        assert all(type(subSet) is SubSet for subSet in subsDict.values()), "Substitution dictionary must only map to SubSet values"
        self._substitutions = subsDict
        self._usedKeys = set()

    def substituteToNumerics(self, expr):
        assert isinstance(expr, sympy.Expr), "substituteToNumerics() requires a sympy Expr"
        assert all(subSet.isNumericSet for subSet in self._substitutions.values()), "Substituting to numeric requires dictionary to only contain numeric substitutions"
        
        if isNumeric(expr):
            return SubSet({expr})
        assert isinstance(expr, sympy.Expr), "Can only substitute for Sympy expressions"
        
        # TODO: substitute
        assert len(self._usedKeys) == 0, "failed to pop all used keys"
        return resultSet

    def substituteByElimination(self, expr, forSymbol):
        assert isinstance(expr, sympy.Expr), "substituteByElimination() requires a sympy Expr"
        assert type(forSymbol) is sympy.Symbol, "substituteByElimination() needs a sympy Symbol for the variable being solved"
        # TODO: substitute
        assert len(self._usedKeys) == 0, "failed to pop all used keys"
        assert not any(symbol in resultSub.expr.free_symbols for resultSub in resultSet for exprKey in self._substitutions for symbol in exprKey.free_symbols), "Elimination-substitution requires a dict with expression keys with unidirectional dependencies (can't have {a: b + c, b: a * c}, but CAN have {a: b + c, b: 2 * c})"
        return resultSet

    def backSubstitute(self):
        assert all(type(symbolKey) is sympy.Symbol for symbolKey in self._substitutions.keys()), "Backwards-substitution only works on symbol substitution dictionaries"
        symbolKeys = sorted(self._substitutions.keys(), key=self._getSymbolDepSortKey)
        for (symbolKey, numSymbolsProcessed) in zip(symbolKeys, range(len(symbolKeys))):
            symbolSubs = self._substitutions[symbolKey]

            if symbolSubs.hasNumerics:
                newSubs = SubSet(
                    sub
                    for sub in symbolSubs
                    if isNumeric(sub.expr)
                )
            else:
                pass # TODO
            
            self._substitutions[symbolKey] = newSubs
        return self._substitutions

    # below are the substitution algorithms currently being used

    # TODO: make common subs algorithm

    # unused substitution algorithms

    def _substituteAllPaths(self, primaryExprs):
        # TODO: should sort unused keys by _getSymbolDepSortKey() for accurate results
        # TODO: this does not yet respect SubSet conditions (don't implement until this needs to be used)
        unusedKeys = iterDifference(self._substitutions, self._usedKeys)
        for exprKey in unusedKeys:
            if primaryExprs.isNumericSet:
                return primaryExprs
            
            self._usedKeys.add(exprKey)

            primaryExprs = SubSet.join(
                self._substituteAllPaths(SubSet({primaryExpr.subs(exprKey, exprKeySub)}))
                for primaryExpr in primaryExprs
                for exprKeySub in self._substitutions[exprKey]
            )

            self._usedKeys.remove(exprKey)
        return primaryExprs

    def _substituteInOrder(self, primaryExprs, keyOrder):
        # TODO: this does not yet respect SubSet conditions (don't implement until this needs to be used)
        for exprKey in keyOrder:
            assert exprKey in self._substitutions, "keyOrder contained exprKey not included in the original subs dict"
            primaryExprs = SubSet(
                primaryExpr.subs(exprKey, exprKeySub)
                for primaryExpr in primaryExprs
                for exprKeySub in self._substitutions[exprKey]
            )
        return primaryExprs

    # other utility functions

    def _subDictUntilFixed(self, expr, subDict):
        assert expr is not None, "cannot sub a nonexistant expression"
        
        origExpr = expr
        lastExpr = None
        iters = 0
        while lastExpr is not expr:
            lastExpr = expr
            expr = expr.subs(subDict)
            iters += 1
            assert iters < 9999, "Substitution probably should have stopped by now..."
        
        return self._generateSub(origExpr, expr, subDict)

    def _generateSub(self, exprBeforeSubs, exprAfterSubs, subDict):
        allSymbols = exprBeforeSubs.free_symbols
        nonSubbedSymbols = exprAfterSubs.free_symbols
        subbedSymbols = iterDifference(allSymbols, nonSubbedSymbols)
        conditions = {
            symbol - symbolSubExpr
            for symbol in subbedSymbols
            for symbolSubExpr in [subDict.get(symbol)]
            # variables get eliminated without being subbed; this checks for that
            # (ex. (a + b + c).subs(b, 1 - c) --> a + 1)
            if symbolSubExpr is not None
        }
        return SubSet.Sub(exprAfterSubs, conditions)

    def _getSymbolDepSortKey(self, symbolKey, _symbolsChecked=None):
        symbolsChecked = _symbolsChecked
        if symbolsChecked is None:
            symbolsChecked = set()
        
        symbolSubs = self._substitutions.get(symbolKey)
        if symbolSubs is None:
            # since there are no substitutions (even if that's just "yet"),
            # there are therefore no dependencies
            return 0
        assert symbolKey not in symbolsChecked, "Circular dependencies were processed (these should not exist...)"
        symbolsChecked.add(symbolKey)
        
        # if A's expressions depend on B, C, and D (throughout the whole SubSet for A),
        # then we want to evaluate A after B, C and D, and therefore kA (the sort key
        # for A) > kB and kC and kD; an easy way to guarantee this is just summing them up
        symbolDeps = {
            symbolDep
            for sub in symbolSubs
            for symbolDep in sub.expr.free_symbols
        }
        # TODO: highly inefficient; we should cache values in a dictionary as we find them
        result = sum(self._getSymbolDepSortKey(symbolDep, symbolsChecked) for symbolDep in symbolDeps) + 1
        symbolsChecked.remove(symbolKey)
        return result

class Solver:
    def __init__(self, relations, symbolDefinitions=None):
        self._relationsEqZero = set(relations)
        self._subsDict = dict()
        self._symbolSubs = dict(symbolDefinitions) if symbolDefinitions is not None else dict()

    def genNumericSolutions(self):
        self._subsDict = dict()
        self._genSolutionsFromNumericsInRelations()
        self._extrapolateNumericSolutionsForSymbols()
        self._subsDict.update({
            symbol: numericSubSet
            for symbol in self._symbolSubs
            for numericSubSet in [self._symbolSubs[symbol]]
            if numericSubSet.isNumericSet
        })
        return self._subsDict

    def _genSolutionsFromNumericsInRelations(self):
        for relation in self._relationsEqZero:
            # although technically the eqNumeric given is the negative/opposite
            # (ie x + 4 (= 0); x is actually -4; we negate the original 4 to get
            # the "solution value"), this isn't necessary for the algorithm
            # (it just returns 4 = -x instead)
            for eqNumeric in (atom for atom in relation.atoms() if isNumeric(atom)):
                solutionSet = self._extractRelationalSolutionSet(self._solveSet(relation, eqNumeric))
                for sub in solutionSet:
                    exprKey = sub.expr
                    assert len(sub.conditions) == 0, "What should be done if there are conditions?"
                    self._updateNumericSubs(exprKey, SubSet({eqNumeric}))

    def _extrapolateNumericSolutionsForSymbols(self):
        numSubs = self._getTotalNumSubstitutions()
        substitutionsChanged = True
        iters = 0
        while substitutionsChanged:
            lastNumSubs = numSubs
            
            # moved out for debugging purposes
            exprKeys = self._exprKeysSortedByIndependence()
            for exprKey in exprKeys:
                symbol = self._findSymbolToSolve(exprKey)
                cantInferFromExprKey = symbol is None
                if cantInferFromExprKey:
                    continue

                assert all(len(sub.conditions) == 0 for sub in self._subsDict[exprKey]), "What to do if there are conditions?"
                symbolSubs = SubSet.join(
                    self._inferSymbolFromRelation(symbol, exprKey - numericSub)
                    for sub in self._subsDict[exprKey]
                    for numericSub in [sub.expr]
                )
                inferredAnyVals = len(symbolSubs) > 0
                if inferredAnyVals:
                    self._updateSymbolSubs(symbol, symbolSubs)
            self._symbolSubs = Substituter(self._symbolSubs).backSubstitute()

            numSubs = self._getTotalNumSubstitutions()
            substitutionsChanged = numSubs != lastNumSubs

            iters += 1
            assert iters < 9999, "Substitutions shouldn't get stuck changing..."

    def _getTotalNumSubstitutions(self):
        return (
            sum(len(subSet) for subSet in self._symbolSubs.values() if subSet.isNumericSet),
            sum(len(subSet) for subSet in self._symbolSubs.values()),
        )

    def _exprKeysSortedByIndependence(self):
        # this ensures that variables needing to be solved don't get "trapped"
        # trying to use an expression they don't exist in
        # (a -- a..2b..2c)
        # (b -- 2a..b..3c)
        # (c -- 2a..4b -- TRAPPED)
        return sorted(self._subsDict.keys(), key=lambda expr: len(expr.free_symbols))

    def _findSymbolToSolve(self, expr):
        return first(iterDifference(expr.free_symbols, self._symbolSubs), None)

    def _inferSymbolFromRelation(self, symbol, relation):
        pass # TODO

    def _solveSet(self, expr, atom):
        # solving x ** 2 for 2 gives us errors (but if we substitute a symbol it's fine...?)
        # so since solving for numeric atoms doesn't work as expected,
        # hopefully substituting them for symbols will match expectations better
        if isNumeric(atom):
            numericSub = sympy.Symbol(":NUMERIC {}:".format(atom))
            exprWithNumericSubbed = expr.subs(atom, numericSub)
            solution = self._solveSet(exprWithNumericSubbed, numericSub)
            solution.reverseSubstitutionMade({numericSub: atom})
            return solution
        else:
            solutionSet = sympy.solveset(expr, atom)
            return self._Solution(solutionSet)

    def _updateNumericSubs(self, exprKey, subSet):
        assert isinstance(exprKey, sympy.Expr), "_updateNumericSubs() requires a sympy expression"
        assert type(subSet) is SubSet, "_updateNumericSubs() requires a SubSet"
        assert subSet.isNumericSet, "_subsDict can only contain numeric substitutions"
        if exprKey not in self._subsDict:
            self._subsDict[exprKey] = subSet
        else:
            self._subsDict[exprKey].addFrom(subSet)
        assert len(self._subsDict[exprKey]) > 0, "Cannot have empty substitution set for main expression substitutions"

    def _updateSymbolSubs(self, symbolKey, subSet):
        assert type(symbolKey) is sympy.Symbol, "_updateSymbolSubs() requires a sympy Symbol key"
        assert type(subSet) is SubSet, "_updateSymbolSubs() requires a SubSet"
        if symbolKey not in self._symbolSubs:
            self._symbolSubs[symbolKey] = subSet
        else:
            self._symbolSubs[symbolKey].addFrom(subSet)
        assert len(self._symbolSubs[symbolKey]) > 0, "Cannot have empty substitution set for symbol substitutions"

    def _extractRelationalSolutionSet(self, solution):
        types = self._Solution.types
        if solution.type is types.NORMAL:
            solutionSet = SubSet(solution.set)
            return solutionSet
        elif solution.type is types.CONDITIONAL:
            (numericSymbol, eqCondition, baseSet) = solution.set.args
            numeric = toNumber(str(numericSymbol))
            assert type(numericSymbol) is NumericSymbol, "Tried to extract a realtional solution from a condition set which was not solved for a numeric"
            (leftCondition, rightCondition) = eqCondition.args
            expRelation = leftCondition - rightCondition
            setIsConditionalBecauseOfAnExponent = any(numeric in term.atoms() for term in self._findPowTerms(expRelation))
            if setIsConditionalBecauseOfAnExponent:
                # solving for an exponent gets messy (and in some cases solutions
                # are lost and can't be preserved using the current expr: SubSet() substitution
                # structure), so we just ignore solving for them altogether
                return SubSet()
            else:
                raise NotImplementedError("Extracting a relational ConditionSet (that isn't from an exponent case)")
        elif solution.type is types.BRANCHES:
            return SubSet.join(
                self._extractRelationalSolutionSet(branchSolution)
                for branchSolution in solution.set
            )
        else:
            raise NotImplementedError("missing a relational solution type case (general case)")

    def _solveExponents(self, expr, atom):
        powTermsWithAtom = {term for term in self._findPowTerms(expr) if atom in term.atoms()}
        assert len(powTermsWithAtom) > 0, "uh... then why are you trying to solve for exponents?"
        basesWithAtom = set()
        expsWithAtom = set()
        for powTerm in powTermsWithAtom:
            (base, exp) = powTerm.args
            if atom in base.atoms():
                basesWithAtom.add(base)
            if atom in exp.atoms():
                expsWithAtom.add(exp)

        anyBases = len(basesWithAtom) > 0
        manyBases = len(basesWithAtom) > 1
        anyExps = len(expsWithAtom) > 0
        manyExps = len(expsWithAtom) > 1
        if anyBases and anyExps:
            return self._solveExponents_bruteSolve(expr, atom)
        elif anyBases:
            if manyBases:
                return self._solveExponents_bruteSolve(expr, atom)
            else:
                partIsExp = False
                return self._solveExponents_solveSingle(expr, first(basesWithAtom), atom, partIsExp)
        elif anyExps:
            if manyExps:
                return self._solveExponents_bruteSolve(expr, atom)
            else:
                partIsExp = True
                return self._solveExponents_solveSingle(expr, first(expsWithAtom), atom, partIsExp)

    def _solveExponents_solveSingle(self, expr, partWithAtom, atom, partIsExp):
        subs = sympy.solve(expr, partWithAtom)
        assert type(subs) is list, "solve() for only one variable should have returned list"
        if partIsExp:
            atomIsEvenPower = atom % 2 == 0
            if atomIsEvenPower:
                subs = {
                    signedSub
                    for sub in subs
                    for signedSub in [sub, -sub]
                }
        return self._Solution.makeBranches(
            self._solveSet(partWithAtom - sub, atom)
            for sub in subs
        )
    
    def _solveExponents_bruteSolve(self, expr, atom):
        sols = sympy.solve(expr, atom)
        assert type(sols) is list, "solve() for only one variable should have returned list"
        return self._Solution(set(sols))

    def _findPowTerms(self, expr):
        if expr.is_Pow:
            yield expr
        for term in expr.args:
            for expTerm in self._findPowTerms(term):
                yield expTerm

    def _extractInferenceSolutionSet(self, solution, withConditions):
        pass # TODO
        # types = self._Solution.types
        # if solution.type is types.NORMAL:
        #     solutionSet = SubSet(self._mapToSubsWithConditions(solution.set, withConditions))
        #     return solutionSet
        # elif solution.type is types.COMPLEXES:
        #     # this means a variable can be any value and still hold true in the relation,
        #     # which ultimately means the relation provided no new information for the symbol;
        #     # we therefore return saying "hey, this symbol has no new substitutions"
        #     return None
        # elif solution.type is types.EMPTY:
        #     # similar to the above case, except we ended up with some kind of
        #     # 4 = 0 case (say, 1 + 3(b + 1)/(b + 1))
        #     return None
        # elif solution.type is types.COMPLEMENT:
        #     if solution.set.args[0] is not sympy.Complexes:
        #         # this happens when the solver is forced to put a variable in the denominator of a fraction;
        #         # this generates a solution with (calculus) "holes", which can be ignored
        #         return SubSet(self._mapToSubsWithConditions(solution.set.args[0], withConditions))
        #     elif solution.set.args[0] is sympy.Complexes:
        #         # this happens in cases similar to the above, except sort of reversed;
        #         # here, the "holes" are actually the solutions, since graphically any value is
        #         # included in the complement set (this happens in cases like (c + 1)/(c + 2))
        #         return SubSet(self._mapToSubsWithConditions(solution.set.args[1], withConditions))
        #     else:
        #         raise NotImplementedError("missing an inference solution type case (for COMPLEMENT sets)")
        # else:
        #     raise NotImplementedError("missing an inference solution type case (general case)")
    
    # def _mapToSubsWithConditions(self, values, conditionSet):
    #     return (SubSet.Sub(val, conditionSet) for val in values)

    class _Solution:
        @classmethod
        def makeBranches(cls, iterable):
            solution = cls.__new__(cls)
            solution._solutionSet = set(iterable)
            solution._type = cls.types.BRANCHES
            return solution

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

            # TODO: maybe we can replace this with sympy.Union()?
            @property
            def BRANCHES(self):
                return "BRANCHES"

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
                if len(solutionSet) == 0:
                    return types.EMPTY
                else:
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

    solutions = Solver({
        a + 2 * b + c  -  0,
        3 * a - b + 2 * c  -  11,
        -a + b - c  -  -6,
    }).genNumericSolutions()
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
