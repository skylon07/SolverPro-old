
import re
import itertools
from sympy import symbols, solve, solveset, Eq, simplify, sqrt, Matrix
a, b, c, d, e, f, g, m, v, t, x, y, z = syms = symbols("a, b, c, d, e, f, g, m, v, t, x, y, z")

# arg types -- <"a = b", ...>, <a, b, ...>
def solveEqs(eqs, syms):
    try:
        eqs = iter(eqs)
    except TypeError:
        raise TypeError("solveEqs() only accepts iterable arguments for equations")
    try:
        syms = iter(syms)
    except TypeError:
        raise TypeError("solveEqs() only accepts iterable arguments for symbols")
    
    # solve for as many symbols as possible
    eqs = list(eqs)
    # DEBUG
    # unsolvedSyms = set(syms)
    class listWithAdd(list):
        def __init__(self, *args):
            super().__init__(*args)
            self.add = self.append
    unsolvedSyms = listWithAdd(syms)
    # / DEBUG
    numSyms = len(unsolvedSyms)
    for firstSym in unsolvedSyms:
        break
    solutions = solveEqsFor(eqs, firstSym, unsolvedSyms)
    noSolutionsFound = solutions is None
    if noSolutionsFound:
        solutions = dict()

    # sometimes not all symbols got a solution
    if len(solutions) != numSyms:
        # substitute an equation with known answers
        subEq = eqs[0]
        for sym in solutions:
            sol = solutions[sym]
            if type(sol) is int:
                subEq = substitute(sym, [sol], [subEq])[0]

        # solve equation for each unsolved variable
        for sym in unsolvedSyms:
            solutions[sym] = solveFor(subEq, sym)
        
    return solutions


# arg types -- ["a = b", ...], a or b, {a, b, ...}
def solveEqsFor(eqs, sym, unsolvedSyms):
    if len(unsolvedSyms) == 0:
        return None
    if len(eqs) == 0:
        return None
    
    # substitute solution for current var into other equations
    tempSol = solveFor(eqs[0], sym)
    if tempSol is None:
        return None
    # SPECIAL CASES:
    # ignore bad solutions (it doesn't give us information on the system)
    # ex. b*c = b*2; of course b = 0 is possible, that's not helpful!
    if tempSol == '0' and len(unsolvedSyms) > 1:
        return None
    # since we have a valid solution;
    # if this is the last equation/symbol, return it!
    if len(eqs) == 1 or len(unsolvedSyms) == 1:
        unsolvedSyms.remove(sym)
        # no substitution needed; no other vars
        finalSol = tempSol
        finalSols = dict()
        tryAddFinalAsInt(finalSols, sym, finalSol)
        return finalSols
    # (resume above instruction)
    otherEqs = substitute(sym, [tempSol], eqs[1:])

    # find solution for next symbol/equation pair
    finalSols = None
    unsolvedSyms.remove(sym) # ignore current symbol for future calls during the loop
    for otherSymToSolveFor in unsolvedSyms:
        finalSols = solveEqsFor(otherEqs, otherSymToSolveFor, unsolvedSyms)
        # if a solution was not found, try the next symbol
        if finalSols is None:
            continue
        break # we found a good solution!
    # no valid solutions were found (or no other symbols existed)
    if finalSols is None:
        # restore what used to be ignored;
        # we didn't actually find a valid solution
        unsolvedSyms.add(sym)
        return None
    
    # generate and add a final solution for current symbol
    finalSol = tempSol
    for otherSym in finalSols:
        otherSol = finalSols[otherSym]
        finalSol = substitute(otherSym, [otherSol], [finalSol])[0]
    finalSol = simplify(finalSol)
    tryAddFinalAsInt(finalSols, sym, finalSol)
    return finalSols


# only adds final solutions that are ints (aka not left with symbols)
def tryAddFinalAsInt(sols, sym, val):
    try:
        sols[sym] = int(val)
    except (TypeError, ValueError):
        sols[sym] = val


# arg types -- a or b, <"b + 4", ...>, <"2 = a*b", ...>
def substitute(sym, symSols, eqs):
    # TODO: remove single-equation limit (blocked by solveFor())
    for symSol in symSols:
        break
    return type(eqs)(
        re.sub(
            str(sym),
            "({})".format(str(symSol)),
            str(eq)
        )
        for eq in eqs
    )

    return {
        re.sub(
            str(sym),
            "({})".format(str(symSol)),
            str(eq)
        )
        for eq in eqs
        for symSol in symSols
    }


# arg types -- "a = b", a or b
def solveFor(eqStr, sym):
    left, right = eqStr.split("=")
    eq = simplify("{} - ({})".format(left, right))
    
    # TODO: make algorithm compatible with all solutions
    # sols = {str(sol) for sol in solveset(eq, sym)}
    sols = {str(sol) for sol in solveFn(eq, sym)}
    for sol in sols:
        return sol



eqs = [
    # "a = b * c",
    # "a = b * 2"

    # "a + b = 2 * c",
    # "2 * b = a + 3",
    # "c = a - 2"

    # "2*c + a = 4 + 2*b",
    # "a + b + c = 7",

    # "a ** 2 = b"
    
    # NO WORKIE!!!
    # "a + b = c",
    # "a = b + c",

    # "a^2 + b^2 = c^2"

    # NO WORKIE!!!
    # "a = b / 5",
    # "b = a",
]
import itertools
allSyms = [b, a, c]
# print(solveEqs(eqs, allSyms), allSyms)
allSyms = list(itertools.permutations(allSyms, len(allSyms)))
for syms in allSyms:
    print(syms)
    solveFn = solveset
    result1 = solveEqs(eqs, syms)
    print("slvst:\ta={}\tb={}\tc={}".format(
        result1[a],
        result1[b],
        result1[c],
    ))
    solveFn = solve
    result2 = solveEqs(eqs, syms)
    print("slv:\ta={}\tb={}\tc={}".format(
        result1[a],
        result1[b],
        result1[c],
    ))