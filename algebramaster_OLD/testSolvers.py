import sympy

from src.algebramaster_OLD.solver import AlgebraSolver, _BackSubstituterSolver, _SympySolveTools
from src.algebramaster_OLD.structures import SubDict, SubDictList


(a, b, c, d, e, two) = sympy.symbols("a, b, c, d, e, two")


class SympySolveToolsTester:
    def testReverseSubstituteForBadNumerics(self):
        expr = a + b**two
        equation = 2 - expr
        solution = _SympySolveTools._Solution(sympy.solveset(equation, 2))
        solution.reverseSubstitutionOfNumeric({two: 2})
        assert solution.set == sympy.FiniteSet(a + b**2)

    def testSolveSetPerformsSolving(self):
        expr = a + b*c - 4
        
        # a + b*c - 4 = 0
        # a + b*c = 4
        actualSolution1 = {4}
        toolsSolution1 = _SympySolveTools.solveSet(expr, a + b*c)
        assert toolsSolution1 == actualSolution1

        # a + b*c - 4 = 0
        # a - 4 = -b*c
        # -(a - 4) = b*c
        actualSolution2 = {b*c}
        toolsSolution2 = _SympySolveTools.solveSet(expr, -(a - 4))
        assert toolsSolution2 == actualSolution2

    def testSolveSetSolvesForNumerics(self):
        expr = 2 + 2*a + b
        
        # 2 + 2*a + b = 0
        # 2 + 2*a = -b
        # 2*(1 + a) = -b
        # 2 = -b/(1 + a)
        actualSolution1 = {-b/(1 + a)}
        toolsSolution1 = _SympySolveTools.solveSet(expr, 2)
        assert toolsSolution1 == actualSolution1
    
    def testSolveSetSolvesForNumerics_ignoresExponents(self):
        expr = a + b**2 - 2
        
        # a + b**2 - 2 = 0
        # -2 = -(a + b**2)
        # 2 = a + b**2      <-- should ignore `**2`!
        actualSolution1 = {a + b**2}
        toolsSolution1 = _SympySolveTools.solveSet(expr, 2)
        assert toolsSolution1 == actualSolution1

    def testSolvesSpecialOnesCaseByIgnoringMultiplies(self):
        expr = -a + b - 1

        # -a + b = 1
        # simple, right? NOPE!
        # sympy.solveset(expr, 1) yields b/(a + 1)
        # this is why:
        #   -a + b - 1 = 0
        #   -a     - 1 = -b
        #    a     + 1 =  b
        #         1    =  b/(a + 1)
        # so... technically not wrong, but we wanted it to solve
        # for the 1 already there, not for the 1 that was created
        # during the identity division (might also have to do with
        # it trying to solve for the 1 in -a, aka -1 * a)
        assert sympy.solveset(expr, 1) == {b/(a + 1)}

        wantedSolution = {-a + b}
        toolsSolution = _SympySolveTools.solveSet(expr, 1)
        assert toolsSolution == wantedSolution


class BackSubstituterTester:
    def testGetsCorrectSolutionsFromSingleUniverse(self):
        # a + b = 4
        # a - b = 2
        # (a = 3)
        # (b = 1)
        baseSubs1 = SubDict({
            a + b: 4,
            a - b: 2,
        })
        actualSolutions1 = SubDictList.fromList([{a: 3, b: 1}])
        solver1 = _BackSubstituterSolver(baseSubs1)
        solverSolutions1 = solver1.getSolutions()
        assert solverSolutions1 == actualSolutions1

        # a + 2*b + c = 20
        # 2*a + c = 14
        # b - a = 1
        # (a = 4)
        # (b = 5)
        # (c = 6)
        baseSubs2 = SubDict({
            a + 2*b + c: 20,
            2*a + c: 14,
            b - a: 1,
        })
        actualSolutions2 = SubDictList.fromList([{a: 4, b: 5, c: 6}])
        solver2 = _BackSubstituterSolver(baseSubs2)
        solverSolutions2 = solver2.getSolutions()
        assert solverSolutions2 == actualSolutions2

        # a + b = 6
        # b - c = 7
        # c = d - 8
        # d + e = -7
        # e + a = -10
        # (a = 2)
        # (b = 4)
        # (c = -3)
        # (d = 5)
        # (e = -12)
        baseSubs3 = SubDict({
            a + b: 6,
            b - c: 7,
            c - d: -8,
            d + e: -7,
            e + a: -10,
        })
        actualSolutions3 = SubDictList.fromList([{a: 2, b: 4, c: -3, d: 5, e: -12}])
        solver3 = _BackSubstituterSolver(baseSubs3)
        solverSolutions3 = solver3.getSolutions()
        assert solverSolutions3 == actualSolutions3

    def testGetsCorrectSolutionsFromTwoUniverses(self):
        # a**2 = 4
        # (a = 2, -2)
            # baseSubs1 = SubDict({
            #     a**2: 4,
            # })
            # actualSolutions1 = SubDictList.fromList([
            #     ({a: 2}, {a - 2}),
            #     ({a: -2}, {a + 2}),
            # ])
            # solver1 = _BackSubstituterSolver(baseSubs1)
            # solverSolutions1 = solver1.getSolutions()
            # assert solverSolutions1 == actualSolutions1

        # a + b**2 = 14
        # b = sqrt(14 - a)
        # a = 14 - b**2
        # a + b = 8
        # (a = 5, 10)
        # (b = 3, -2)
        baseSubs2 = SubDict({
            a + b**2: 14,
            a + b: 8,
        })
        actualSolutions2 = SubDictList.fromList([
            ({
                a: 5,
                b: 3,
            }, {b: 3}),
            ({
                a: 10,
                b: -2,
            }, {b: -2}),
        ])
        solver2 = _BackSubstituterSolver(baseSubs2)
        solverSolutions2 = solver2.getSolutions()
        assert solverSolutions2 == actualSolutions2

    def testGetsCorrectSolutionsFromMultipleUniverses(self):
        pass # TODO

    def testReturnsSymbolStack(self):
        assert __debug__ == True

        baseSubs = SubDict({
            a + 2*b + c: 20,
            2*a + c: 14,
            b - a: 1,
        })
        solver = _BackSubstituterSolver(baseSubs)
        solverSolutions = solver.getSolutions()
        assert '_symbolStack' in solverSolutions[0].__dict__


class AlgebraSolverTester:
    # a reminder:
    # 
    # the desired format of SubDict results containing solutions
    # should represent the keys/values such that the colon represents an
    # equals sign (aka {a: -3} means a = -3, represented by a + 3)
    #
    # thanks to the negation trick when extracting numerics from relations,
    # this means numeric solutions should only be interpreted from the right
    # sides of equations, meaning the solution to 2 + a (= 0) would NOT be
    # 2 = -a, but rather a = -2, and consequently the resulting SubDict would
    # NOT be {-a: 2}, but rather {a: -2}
    #
    # for this reason, many of the tests do and should reverse the equation
    # (by subtraction of the values on both sides) if the numeric ends up on
    # the left side of the equality

    def testSolvesBasicEquations(self):
        solver = AlgebraSolver()
        solver.recordRelation(a - 4)
        solver.recordRelation(b - 5)

        assert solver.universes == [
            {
                a: 4,
                b: 5,
            },
        ]

    def testSolvesInSteps(self):
        solver = AlgebraSolver()
        assert solver.universes == [{}]

        # a + 2*b + c = 20
        # 2*a + c = 14
        # b - a = 1
        # (a = 4)
        # (b = 5)
        # (c = 6)

        solver.recordRelation(a + 2*b + c --- 20)
        assert solver.universes == [
            {
                # a + 2*b + c  =   20
                a + 2*b + c: 20,
                # a + 2*b + c  =   20
                #     2*b      =   20 - a - c
                #     2        =  (20 - a - c)/b
                # <--->
                # -(20 - a - c)/b = -2
                -(20 - a - c)/b: -2,
            }
        ]

        solver.recordRelation(2*a + c --- 14)
        assert solver.universes == [
            {
                a + 2*b + c: 20,
                -(20 - a - c)/b: -2,
                # 2*a + c  =   14
                2*a + c: 14,
                # 2*a + c  =   14
                # 2*a      =   14 - c
                # 2        =  (14 - c)/a
                # <--->
                # -(14 - c)/a = -2
                -(14 - c)/a: -2,
            }
        ]

        solver.recordRelation(b - a --- 1)
        assert solver.universes == [
            {
                a + 2*b + c: 20,
                -(20 - a - c)/b: -2,
                2*a + c: 14,
                -(14 - c)/a: -2,
                # b - a = 1
                b - a: 1,
                # (solutions for a, b, c)
                a: 4,
                b: 5,
                c: 6,
            }
        ]

    def testTracksUnsolvableRelations(self):
        solver1 = AlgebraSolver()
        assert solver1.universes == [{}]
        assert solver1._unsolvableRelations == []

        relationWithNoNumerics = a + b + c
        solver1.recordRelation(relationWithNoNumerics)
        assert solver1.universes == [{}]
        assert solver1._unsolvableRelations == [a + b + c]

        solver1.recordRelation(a --- 4)
        assert solver1.universes == [
            {
                a: 4,
                b + c: -4,
            }
        ]
        assert solver1._unsolvableRelations == []

        solver2 = AlgebraSolver()
        assert solver2.universes == [{}]
        assert solver2._unsolvableRelations == []

        # b + c + d intentionally before a + b to test that unsolved relations
        # will be revisited if later unsolved relations end up being solved
        # and providing new information
        solver2.recordRelation(b + c + d)
        solver2.recordRelation(a + b)
        solver2.recordRelation(c + e)
        assert solver2.universes == [{}]
        assert solver2._unsolvableRelations == [b + c + d, a + b, c + e]

        solver2.recordRelation(a --- 3)
        assert solver2.universes == [
            {
                a: 3,
                b: -3,
                c + d: 3,
            }
        ]
        assert solver2._unsolvableRelations == [c + e]

# TODO: add applicable tests from the old tests copied below:
#   # a + b = 4
#   # a - b = 2
#   # (a = 3)
#   # (b = 1)
#   (a, b) = sympy.symbols("a, b")
#   testSolver(
#       [
#           a + b  -  4,
#           a - b  -  2,
#       ],
#       [{
#           a: 3,
#           b: 1,
#       }],
#       "basic two-relation two-variable one-universe system",
#       allSolutionsProvided=True,
#   )
#   Tester.stopIfFailed()
    
#   # a + 2b + c = 20
#   # 2a + c = 14
#   # b - a = 1
#   # (a = 4)
#   # (b = 5)
#   # (c = 6)
#   (a, b, c) = sympy.symbols("a, b, c")
#   testSolver(
#       [
#           a + 2*b + c  -  20,
#           2*a + c  -  14,
#           b - a  -  1,
#       ],
#       [{
#           a: 4,
#           b: 5,
#           c: 6,
#       }],
#       "basic three-relation three-variable one-universe system",
#       allSolutionsProvided=True
#   )
#   Tester.stopIfFailed()
    
#   # a + b = 6
#   # b - c = 7
#   # c = d - 8
#   # d + e = -7
#   # e + a = -10
#   # (a = 2)
#   # (b = 4)
#   # (c = -3)
#   # (d = 5)
#   # (e = -12)
#   (a, b, c, d, e) = sympy.symbols("a, b, c, d, e")
#   testSolver(
#       [
#           a + b  -  6,
#           b - c  -  7,
#           c  -  (d - 8),
#           d + e  -  -7,
#           e + a  -  -10,
#       ],
#       [{
#           a: 2,
#           b: 4,
#           c: -3,
#           d: 5,
#           e: -12,
#       }],
#       "five-variable chained relations system",
#       allSolutionsProvided=True,
#   )
#   Tester.stopIfFailed()
    
#   # ab = 8
#   # b = 2a
#   # (a = -2, 2)
#   # (b = -4, 4)
#   (a, b) = sympy.symbols("a, b")
#   testSolver(
#       [
#           a*b  -  8,
#           b  -  2*a,
#       ],
#       [{
#           a: 2,
#           b: 4,
#       }, {
#           a: -2,
#           b: -4,
#       }],
#       "two-variable two-universe multiplication system",
#       allSolutionsProvided=True
#   )
#   Tester.stopIfFailed()
    
#   # f = m * a
#   # f = 80
#   # m = 20
#   # (a = 4)
#   (f, m, a) = sympy.symbols("f, m, a")
#   testSolver(
#       [
#           f  -  m*a,
#           f - 80,
#           m - 20,
#       ],
#       [{
#           f: 80,
#           m: 20,
#           a: 4,
#       }],
#       "system with one variable-only relation",
#       allSolutionsProvided=True,
#   )
#   Tester.stopIfFailed()
    
#   # k1i = 1/2 * m1 * v1i^2
#   # k2i = 1/2 * m2 * v2i^2
#   # k1f = 1/2 * m1 * v1f^2
#   # k2f = 1/2 * m2 * v2f^2
#   # kt = k1i + k2i
#   # kt = k1f + k2f
#   # m1 = 10
#   # m2 = 16
#   # v1i = 15
#   # v2i = 10
#   # v1f = 5
#   # (v2f = 15)
#   # (k1i = 1125)
#   # (k2i = 800)
#   # (kt = 1925)
#   # (k1f = 125)
#   # (k2f = 1800)
    
#   (m1, m2, v1i, v2i, v1f, v2f, k1i, k2i, k1f, k2f, kt) = sympy.symbols("m1, m2, v1i, v2i, v1f, v2f, k1i, k2i, k1f, k2f, kt")
#   testSolver(
#       [
#           k1i  -  1/2 * m1 * v1i**2,
#           k2i  -  1/2 * m2 * v2i**2,
#           k1f  -  1/2 * m1 * v1f**2,
#           k2f  -  1/2 * m2 * v2f**2,
#           kt  -  (k1i + k2i),
#           kt  -  (k1f + k2f),
#           m1 - 10,
#           m2 - 16,
#           v1i - 15,
#           v2i - 10,
#           v1f - 5,
#       ],
#       [{
#           m1: 10,
#           m2: 16,
#           v1i: 15,
#           v2i: 10,
#           v1f: 5,
#           v2f: 15,
#           k1i: 1125,
#           k2i: 800,
#           k1f: 125,
#           k2f: 1800,
#           kt: 1925,
#       }, {
#           m1: 10,
#           m2: 16,
#           v1i: 15,
#           v2i: 10,
#           v1f: 5,
#           v2f: -15, # the only thing different (because ±√(2*k2f/m2))
#           k1i: 1125,
#           k2i: 800,
#           k1f: 125,
#           k2f: 1800,
#           kt: 1925,
#       }],
#       "system with multiple variable-only relations",
#       allSolutionsProvided=True
#   )
#   Tester.stopIfFailed()
    
#   # a + c = b
#   # d - b = c
#   # a*d = 4*(b + c)
#   # a * 10 = d * 4
#   # (a = 4)
#   # (b = 7)
#   # (c = 3)
#   # (d = 10)
#   (a, b, c, d) = sympy.symbols("a, b, c, d")
#   testSolver(
#       [
#           a + c  -  b,
#           d - b  -  c,
#           a*d  -  4*(b + c),
#           a*10  -  d*4,
#       ],
#       [{
#           a: 4,
#           b: 7,
#           c: 3,
#           d: 10,
#       }],
#       "system with relations missing numerics",
#       allSolutionsProvided=False
#   )
#   Tester.stopIfFailed()
    
#   # a + b = -c
#   # b * b + c = a
#   # b + b - a = -c
#   # (a = 1)
#   # (b = 2)
#   # (c = -3)
#   (a, b, c) = sympy.symbols("a, b, c")
#   testSolver(
#       [
#           a + b  +  c,
#           b * b + c  -  a,
#           b + b - a  +  c,
#       ],
#       [{
#           a: 1,
#           b: 2,
#           c: -3,
#       }],
#       "system with all relations missing numerics (and one positive-only)",
#       allSolutionsProvided=False
#   )
#   Tester.stopIfFailed()