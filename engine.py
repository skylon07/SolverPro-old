from abc import ABC, abstractmethod, abstractproperty
from math import log10

import sympy


# decorator that utilizes memory optimizations
# (should obviously me used only for classes/functions
# whose results can be treated as "immutable")
# TODO: use this class when a performance boost is needed (or to see how much of a boost it gives)
class immutable:
    _immutables = dict()
    _immutableClasses = list()

    # ran when python "calls the decorator"
    def __new__(cls, anyCallable):
        isClass = type(anyCallable) is type
        if isClass:
            class ImmutableMixin(anyCallable):
                # we want "calls" to the class to run the memo function
                def __new__(mixinCls, *args, **kwargs):
                    return cls._getMemo(mixinCls, args, kwargs, create=mixinCls.__createNew)

                def __init__(self, *args, **kwargs):
                    # DONT call parent init after __new__; we want to have control of that
                    return

                # the creation function is separate to avoid recursion errors
                # (and also gives a constant reference for memoizing)
                @classmethod
                def __createNew(mixinCls, *args, **kwargs):
                    newObj = super(ImmutableMixin, mixinCls).__new__(mixinCls)
                    super(mixinCls, newObj).__init__(*args, **kwargs)
                    return newObj
            cls._immutableClasses.append(ImmutableMixin)
            return ImmutableMixin
        else:
            def memoWrapper(*args, **kwargs):
                return cls._getMemo(anyCallable, args, kwargs)
            return memoWrapper

    @classmethod
    def _getMemo(cls, someCallable, argsTuple, kwargsDict, create=None):
        if len(kwargsDict) > 0:
            # currently no way to distinguish if an argument becomes a keyword argument
            # def fn(arg, kwargs="val")...
            # fn(1, 2) <-- args=(1,2) kwargs={}
            raise ValueError("Immutables cannot be created/called with keyword arguments")
        
        # separate creation function is needed for class wrapping
        if create is None:
            create = someCallable
        memoKey = cls._makeCallId(someCallable, argsTuple, kwargsDict)
        if memoKey not in cls._immutables:
            cls._immutables[memoKey] = create(*argsTuple, **kwargsDict)
        return cls._immutables[memoKey]

    @classmethod
    def _makeCallId(cls, someCallable, argsTuple, kwargsDict):
        callHash = hash(someCallable)
        hashableTuple = tuple(cls._makeHashable(item) for item in argsTuple)
        argsHash = hash(hashableTuple)
        # kwargsHash ignored; kwargs not currently allowed
        return hash((callHash, argsHash))

    @classmethod
    def _makeHashable(cls, obj):
        # sorts unordered data types for hashing
        if isinstance(obj, set):
            # TODO: what if items aren't comparable? ex 2 < '3'
            hashed = sorted(cls._makeHashable(item) for item in obj)
            return "set:{}".format(hashed)
        if isinstance(obj, dict):
            # key is already hashable to be in dict
            hashed = sorted((key, cls._makeHashable(obj[key])) for key in obj)
            return "dict:{}".format(hashed)
        
        # treat lists as tuples
        if isinstance(obj, list):
            hashed = [cls._makeHashable(item) for item in obj]
            return "list:{}".format(hashed)
        
        # if object isn't hashable, this will error eventually (just add a case in this function)
        return obj


class Engine:
    @property
    def _validIdKeys(self):
        return (
            # Solutions() relies on Identifier being confirmed here
            Identifier,
        )
    _validIdKeysNames = (
        "Identifier",
    )
    @property
    def _validIdVals(self):
        return (
            Numeric,
            Identifier,
            Template,
        )
    _validIdValsNames = (
        "Numeric",
        "Identifier",
        "Template",
    )

    def __init__(self):
        for keyName, keyType in zip(self._validIdKeysNames, self._validIdKeys):
            # checking this way ensures that an explicit definition of
            # __hash__ and __eq__ are provided
            if not callable(keyType.__hash__) or not callable(keyType.__eq__):
                raise TypeError("{}() cannot be used as dictionary key".format(keyName))
        
        self._identifiers = list()
        self._aliases = dict()
        self._solutions = list()

    def setAlias(self, identifier, value):
        if not isinstance(identifier, self._validIdKeys):
            raise TypeError("Engine.setAlias(identifier, value) -- identifier was not one of {}".format(self._validIdKeysNames))
        if not isinstance(value, self._validIdVals):
            raise TypeError("Engine.setAlias(identifier, value) -- value was not one of {}".format(self._validIdValsNames))

        self._identifiers.append(identifier)
        self._aliases[identifier] = value

    def setAliases(self, identifiers, value):
        if not isinstance(value, self._validIdVals):
            raise TypeError("Engine.setAliases(identifiers, value) -- value was not one of {}".format(self._validIdValsNames))
        for identifier in identifiers:
            if not isinstance(identifier, self._validIdKeys):
                raise TypeError("Engine.setAliases(identifiers, value) -- identifiers was not a list with each item one of {}".format(self._validIdKeysNames))
        
        for identifier in identifiers:
            self.setAlias(identifier, value)

    def getAlias(self, identifier):
        return self._aliases.get(identifier)

    def isDefined(self, identifier):
        # dicts have faster lookup than lists, therefore aliases is used
        return identifier in self._aliases

    # substitutes all known values into a given object
    def substitute(self, subsable):
        if not isinstance(subsable, Substitutable):
            raise TypeError("Engine.substitute(subsable) -- subsable must be a Substitutable()")
        
        substDict = self._aliases
        substituted = subsable.substitute(substDict)
        return substituted

    def addRelation(self, relation):
        if not isinstance(relation, Relation):
            raise TypeError("Engine.addRelation() can only add Relation() objects")

        newSols = Solutions(relation)
        self._solutions.append(newSols)

    # due to the nature of Solutions.solveFor(), this returns a non-standard Engine object
    def getSolutionsFor(self, expr):
        if not isinstance(expr, Expressable):
            raise TypeError("Engine.getSolutionsFor(identifier) not given an Identifier()")
        
        if isinstance(expr, Numeric):
            numeric = expr
            return SolutionSet([numeric])
        
        elif isinstance(expr, Identifier) and self.isDefined(expr):
            identifier = expr
            return SolutionSet([self.substitute(identifier)])
                
        else:
            allSolutions = set()
            for sols in self._solutions:
                solSet = sols.solveFor(expr, self._aliases)
                # using a set ensures each unique solution only appears once
                allSolutions = allSolutions.union(solSet)
            return SolutionSet(allSolutions)


# abstract class that gives a blueprint for repr strings for objects
class Displayable(ABC):
    @abstractmethod
    def __str__(self):
        return # some string representation
    
    def __repr__(self):
        try:
            return "<{}: {}>".format(self._reprName, self)
        except RecursionError as e:
            raise RecursionError("(Inside class Displayable.__repr__(); did you define _reprName as @property?)")

    @abstractproperty
    def _reprName(self):
        return # name that represents type(self)
    

# mixin class that provides a sympy representation of an object
class Symbolable(ABC):
    def asSymbol(self, templatesDict):
        symbol = self._selfAsSymbol(templatesDict)
        if not isinstance(symbol, (sympy.Expr, float, int)):
            raise TypeError("Symbolable._selfAsSymbol() returned non-symbol type")
        return symbol
    
    @abstractmethod
    def _selfAsSymbol(self, templatesDict):
        return # sympy.Symbol()/<something sympy can deal with, like numbers>


# mixin class that provides an evaluation interface
class Substitutable(ABC):
    def substitute(self, substDict):
        subsable = self._substituteSelf(substDict)
        if not isinstance(subsable, Substitutable):
            raise TypeError("Substitutable._substituteSelf() did not return a Substitutable()")
        return subsable

    @abstractmethod
    # substDict[Identifier()] -> Substitutable()
    def _substituteSelf(self, substDict):
        return # Substitutable()


# represents something that "contains" Identifier() objects
class Containable(Substitutable, Displayable):
    def contains(self, identifier, templatesDict):
        if not isinstance(identifier, Identifier):
            raise TypeError("Containable.contains() can only be given an Identifier()")

        contains = self._doesContain(identifier, templatesDict)
        if type(contains) is not bool:
            raise TypeError("Containable._doesContain() did not return a boolean")
        self.__container[identifier] = contains
        return contains
    
    @abstractmethod
    def _doesContain(self, identifier, templatesDict):
        return # boolean


# abstract class that gives operators to expression-like objects
class Expressable(Symbolable, Containable):
    def __hash__(self):
        return hash(str(self))

    def __eq__(self, other):
        if isinstance(other, Expressable):
            return str(self) == str(other)
        return False

    @abstractmethod
    def __str__(self):
        return # a unique string that can be used to check equality
    
    # operations for expressions
    def __add__(self, other):
        return Expression(self, '+', other)

    def __sub__(self, other):
        return Expression(self, '-', other)

    def __mul__(self, other):
        return Expression(self, '*', other)

    def __truediv__(self, other):
        return Expression(self, '/', other)

    def __pow__(self, other):
        return Expression(self, '^', other)

    def __neg__(self):
        return NegativeExpression(self)


class Numeric(Expressable):
    def __init__(self, number):
        validTypes = (float, int, str)
        if type(number) not in validTypes:
            raise TypeError("Numeric constructed with invalid value")
        
        # removes floating point errors
        self._symbol = self._roundFloat(float(number))

    @property
    def _reprName(self):
        return "Numeric"

    # overrides Value.__str__ (allows printing ints)
    def __str__(self):
        forceFormatAboveThis = 1e16
        if self._symbol < forceFormatAboveThis:
            symAsInt = int(self._symbol)
            isInt = symAsInt == self._symbol
            if isInt:
                return str(symAsInt)
        return str(self._symbol)

    def _selfAsSymbol(self, templatesDict):
        return self._symbol

    def _substituteSelf(self, substDict):
        return self

    def _doesContain(self, identifier, templatesDict):
        return False

    @classmethod
    def _roundFloat(cls, rawFloat):
        # special case for 0 (log10 gives domain error)
        if rawFloat == 0:
            return rawFloat
        # number of digits to most signifigant figure
        numDigits = int(log10(abs(rawFloat)))
        if numDigits >= 0:
            # because log10(1) gives 0; we want 1
            numDigits += 1
        # we care about 12 signifigant digits
        # (round() using positive arg will round after decimal;
        # with negative arg will round before decimal; this is 
        # the opposite of numDigits, therefore we use -numDigits)
        roundTo = 12 - numDigits
        roundFloat = round(rawFloat, roundTo)
        return roundFloat

    def __add__(self, other):
        if isinstance(other, Numeric):
            newVal = self._symbol + other._symbol
            return Numeric(newVal)
        return super().__add__(other)
    
    def __sub__(self, other):
        if isinstance(other, Numeric):
            newVal = self._symbol - other._symbol
            return Numeric(newVal)
        return super().__sub__(other)

    def __mul__(self, other):
        if isinstance(other, Numeric):
            newVal = self._symbol * other._symbol
            return Numeric(newVal)
        return super().__mul__(other)

    def __truediv__(self, other):
        if isinstance(other, Numeric):
            newVal = self._symbol / other._symbol
            return Numeric(newVal)
        return super().__truediv__(other)

    def __pow__(self, other):
        if isinstance(other, Numeric):
            newVal = self._symbol ** other._symbol
            return Numeric(newVal)
        return super().__pow__(other)

    def __neg__(self):
        return Numeric(-self._symbol)


# represents a single name within the program
# (used as the key to a dictionary holding what it identifies)
class Identifier(Expressable):
    def __init__(self, idStr):
        self._ensureId(idStr)
        self._symbol = sympy.Symbol(idStr)

    @property
    def _reprName(self):
        return "Identifier"

    def __str__(self):
        return str(self._symbol)

    def _selfAsSymbol(self, templatesDict):
        return self._symbol
    
    def _substituteSelf(self, substDict):
        sub = substDict.get(self)
        if sub:
            if isinstance(sub, Identifier):
                result = sub._substituteSelf(substDict)
            else:
                result = sub
        else:
            result = self
        return result

    def _doesContain(self, identifier, templatesDict):
        # base case for expressions checking if this identifier is the one in question
        return self == identifier

    # these functions are defined to allow equivelant identifiers
    # to modify the same dictionary bucket
    def __eq__(self, otherId):
        if isinstance(otherId, Identifier):
            return self._symbol == otherId._symbol
        return False

    def __hash__(self):
        return super().__hash__()

    # ensures the Identifier() holds a valid name
    @classmethod
    def _ensureId(cls, idStr):
        if type(idStr) is str:
            # empty strs not allowed
            if len(idStr) > 0:
                # first must be alpha char
                if idStr[0].isalpha():
                    # rest must be alphanumeric
                    rest = idStr[1:]
                    if rest.isalnum() or len(rest) == 0:
                        return
        raise ValueError("Identifier() tried to construct with invalid idStr argument")


OPERATORS = {
    "+": "+",
    "PLUS": "+",
    "-": "-",
    "MINUS": "-",
    "*": "*",
    "MULTIPLY": "*",
    "/": "/",
    "DIVIDE": "/",
    "^": "^",
    "EXPONENT": "^",
}
# data structure for performing operations
class Expression(Expressable):
    def __init__(self, expr1, oper, expr2):
        if not isinstance(expr1, Expressable):
            raise TypeError("first argument for Expression must be an expression")
        if type(oper) is not str or oper not in OPERATORS:
            raise TypeError("second argument for Expression must be an operator")
        if not isinstance(expr2, Expressable):
            raise TypeError("third argument for Expression must be an expression")
        
        self._leftExpr = expr1
        self._rightExpr = expr2
        self._oper = oper

    @property
    def _reprName(self):
        return "Expression"

    def __str__(self):
        precedence = {
            '+': 1,
            '-': 1,
            '*': 2,
            '/': 2,
            '^': 3,
        }
        left = str(self._leftExpr)
        right = str(self._rightExpr)
        if isinstance(self._leftExpr, Expression):
            if precedence[self._oper] > precedence[self._leftExpr._oper]:
                # preserve order of operations
                left = '({})'.format(left)
        if isinstance(self._rightExpr, Expression):
            if precedence[self._oper] > precedence[self._rightExpr._oper]:
                # preserve order of operations
                right = '({})'.format(right)
        return "{} {} {}".format(left, self._oper, right)
    
    def _selfAsSymbol(self, templatesDict):
        sym1 = self._leftExpr._selfAsSymbol(templatesDict)
        sym2 = self._rightExpr._selfAsSymbol(templatesDict)
        return self._operate(sym1, sym2)

    def _substituteSelf(self, substDict):
        val1 = self._leftExpr._substituteSelf(substDict)
        val2 = self._rightExpr._substituteSelf(substDict)
        return self._operate(val1, val2)

    def _doesContain(self, identifier, templatesDict):
        left = self._leftExpr
        right = self._rightExpr
        return left._doesContain(identifier, templatesDict) or right._doesContain(identifier, templatesDict)

    def _operate(self, val1, val2):
        if self._oper == '+':
            result = val1 + val2
        elif self._oper == '-':
            result = val1 - val2
        elif self._oper == '*':
            result = val1 * val2
        elif self._oper == '/':
            result = val1 / val2
        elif self._oper == '^':
            result = val1 ** val2
        else:
            # this should never happen, since oper is validated on construction
            raise ValueError("Expression tried to substitute with an invalid operator")
        return result
        

# since expressions can only handle two values around an operator,
# this special expression deals with negation
class NegativeExpression(Expressable):
    def __init__(self, expr):
        if not isinstance(expr, Expressable):
            raise TypeError("first argument for NegativeExpression must be a substitutable")
        
        self._expr = expr

    @property
    def _reprName(self):
        return "NegExpression"

    def __str__(self):
        if isinstance(self._expr, (Numeric, Identifier, TemplateCall, NegativeExpression)):
            return "-{}".format(self._expr)
        return "-({})".format(self._expr)

    def _selfAsSymbol(self, templatesDict):
        sym = self._expr._selfAsSymbol(templatesDict)
        return -sym

    def _substituteSelf(self, substDict):
        result = self._expr._substituteSelf(substDict)
        return -result

    def _doesContain(self, identifier, templatesDict):
        return self._expr._doesContain(identifier, templatesDict)


class Template(Containable):
    def __init__(self, paramNames, rightHand):
        for paramName in paramNames:
            if not isinstance(paramName, Identifier):
                raise TypeError("first argument for Template() must be a list of Identifier()s")
        if not isinstance(rightHand, Substitutable):
            raise TypeError("second argument for Template() must be a Substitutable()")

        self._argNames = paramNames
        self._rightHand = rightHand

    @property
    def rightHand(self):
        return self._rightHand

    @property
    def argNames(self):
        return self._argNames

    def __call__(self, argsDict):
        if len(argsDict) < len(self._argNames):
            raise ValueError("Template.substitute() not given enough substitutions")
        for key in argsDict:
            if key not in self._argNames:
                raise ValueError("Template.substitute() can only substitute arguments that were part of its definition")
        # _substituteSelf() is not used to validate the return value
        return self._rightHand.substitute(argsDict)

    @property
    def _reprName(self):
        return "Template"

    def __str__(self):
        argNamesStr = ', '.join(str(name) for name in self._argNames)
        return "({}) -> {}".format(argNamesStr, self._rightHand)

    def _substituteSelf(self, substDict):
        return self._rightHand._substituteSelf(substDict)

    def _doesContain(self, identifier, templatesDict):
        return self._rightHand._doesContain(identifier, templatesDict)


class TemplateCall(Expressable):
    def __init__(self, name, params):
        if not isinstance(name, Identifier):
            raise TypeError("first argument of TemplateCall must be an Identifier()")
        for param in params:
            if not isinstance(param, Expressable):
                raise TypeError("second argument must only contain Expressable()s")

        self._name = name
        self._params = params

    @property
    def _reprName(self):
        return "TemplateCall"

    def __str__(self):
        paramsStr = ', '.join(str(expr) for expr in self._params)
        return "{}({})".format(self._name, paramsStr)

    @property
    def nameId(self):
        return self._name

    def _selfAsSymbol(self, templatesDict):
        template = templatesDict[self._name]
        if not isinstance(template, Template):
            raise TypeError("TemplateCall substituted and got a non-template")
        subs = self._substituteTemplate(template)
        return subs._selfAsSymbol(templatesDict)

    def _substituteSelf(self, substDict):
        template = substDict[self._name]
        subsable = self._substituteTemplate(template)
        return subsable._substituteSelf(substDict)

    def _substituteTemplate(self, template):
        namesAndValues = zip(template.argNames, self._params)
        substDict = {name: value for name, value in namesAndValues}
        return template(substDict)

    def _doesContain(self, identifier, templatesDict):
        template = templatesDict[self._name]
        return template._doesContain(identifier, templatesDict)


# data structure for two equal expressions
# (this class intentionally not an Expressable)
class Relation(Symbolable, Containable):
    def __init__(self, expr1, expr2):
        if not isinstance(expr1, Expressable):
            raise TypeError("first argument for Relation must be an expression")
        if not isinstance(expr2, Expressable):
            raise TypeError("second argument for Relation must be an expression")
        
        self._leftExpr = expr1
        self._rightExpr = expr2

    @property
    def _reprName(self):
        return "Relation"

    def __str__(self):
        return "{} = {}".format(self._leftExpr, self._rightExpr)

    @property
    def left(self):
        return self._leftExpr

    @property
    def right(self):
        return self._rightExpr
    
    # returns symbol that is assumed equal to zero
    def _selfAsSymbol(self, templatesDict):
        leftSym = self._leftExpr._selfAsSymbol(templatesDict)
        rightSym = self._rightExpr._selfAsSymbol(templatesDict)
        return leftSym - rightSym

    def _substituteSelf(self, substDict):
        leftSub = self._leftExpr._substituteSelf(substDict)
        rightSub = self.rightExpr._substituteSelf(substDict)
        return Relation(leftSub, rightSub)

    def _doesContain(self, identifier, templatesDict):
        left = self._leftExpr
        right = self._rightExpr
        return left._doesContain(identifier, templatesDict) or right._doesContain(identifier, templatesDict)
    

# represents the list of all solutions for variables in a single relation
class Solutions(Containable):
    def __init__(self, relation):
        if not isinstance(relation, Relation):
            raise TypeError("Solutions(relation) can only be created if relation is a Relation()")
        
        self._relation = relation

    @property
    def relation(self):
        return self._relation

    @property
    def _reprName(self):
        return "Solutions"
    
    def __str__(self):
        # this weird syntax produces "{...}*" where '...' is self._relation
        return "*{{{}}}".format(self._relation)

    def _substituteSelf(self, substDict):
        newRelation = self._relation._substituteSelf(substDict)
        return Solutions(newRelation)

    def _doesContain(self, identifier, templatesDict):
        return self._relation._doesContain(identifier, templatesDict)

    # unlike most functions, this one returns a raw sympy object, not an Engine data structure
    def solveFor(self, symbolable, templatesDict):
        if not isinstance(symbolable, Symbolable):
            raise TypeError("Solutions.solveFor() cannot solve for non-Identifier() types")
        
        equation = self._relation.asSymbol(templatesDict)
        solveSym = symbolable.asSymbol(templatesDict)
        solutions = sympy.solveset(equation, solveSym)
        return solutions


# different from solutions; this is a wrapper that signifies what
# kind of answer a Solutions() object solved to
class SolutionSet(Displayable):
    def __init__(self, solutions):
        self._solutions = solutions
        self._isNumeric = self._calcIsNumeric()

    @property
    def isNumeric(self):
        return self._isNumeric

    @property
    def _reprName(self):
        return "SolutionSet"

    def __str__(self):
        return "{}".format(self._solutions)

    def __iter__(self):
        for sol in self._solutions:
            # these errors would raise in cases of relation contradiction
            if self._isNumeric and not self._isNumericLike(sol):
                raise TypeError("SolutionSet received mismatching elements; some Numeric(), some not (expected Numeric)")
            elif not self._isNumeric and self._isNumericLike(sol):
                raise TypeError("SolutionSet received mismatching elements; some Numeric(), some not (expected NOT Numeric)")
            
            yield sol
        
    def __len__(self):
        return len(self._solutions)

    def _calcIsNumeric(self):
        if len(self._solutions) == 0:
            return False
        for firstSol in self._solutions:
            return self._isNumericLike(firstSol)

    @classmethod
    def _isNumericLike(cls, numeric):
        return isinstance(numeric, (Numeric, sympy.Number))


if __name__ == "__main__":
    engine = Engine()
    rel = Relation(
        Expression(Numeric(4), '-', Identifier('b')),
        NegativeExpression(Identifier('a')),
    )
    print(repr(rel))
    sol = Solutions(rel)
    print(repr(sol))
    
    print(sol.solveFor(Identifier('b'), dict()))
    print(type(sol.solveFor(Identifier('b'), dict())))
    
