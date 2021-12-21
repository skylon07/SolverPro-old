from abc import ABC, abstractmethod, abstractproperty
from math import log10
import string

import sympy


# abstract class that gives a blueprint for repr strings for objects
class Displayable(ABC):
    def __repr__(self):
        try:
            return "<{}: {}>".format(self._reprName, self)
        except RecursionError:
            # forgetting @property has been the most common case for recursion errors,
            # since forgetting it causes _reprName() to be a normal function (and repr()
            # of a normal function is <bound ... [self class]>, which calls repr(self)...)
            raise RecursionError("(Inside a Displayable.__repr__(); did you define _reprName as @property?)")
    
    @abstractmethod
    def __str__(self):
        return # some string representation

    @abstractproperty
    def _reprName(self):
        return # name that represents type(self)


# class that makes objects "dict key"-able
class Hashable(ABC):
    @abstractmethod
    def __eq__(self, other):
        return # if self == other

    @abstractmethod
    def __hash__(self):
        return # an equivelantly UNIQUE int (unique unless equal)


# something that can be "substituted" when given a dictionary
class Substitutable(Hashable, ABC):
    def substitute(self, substDict):
        subsable = self._substituteSelf(substDict)
        validExceptions = [Numeric]
        if not isinstance(subsable, (Substitutable, *validExceptions)):
            raise TypeError("_substituteSelf() did not return a Substitutable() (or other valid type)")
        return subsable

    @abstractmethod
    def _substituteSelf(self, substDict):
        return # substituted self based on dict


# something that "contains" other types
class Containable(Substitutable, ABC):
    def iterateType(self, instanceType):
        if not isinstance(instanceType, type):
            raise TypeError("iterateType() can only test against types (class identifiers, builtins, etc)")

        iterable  = self._allOfType(instanceType)
        try:
            # could have been an iterator; always treat as single-use!
            return iter(iterable)
        except TypeError as e:
            if "is not iterable" in str(e):
                raise TypeError("_allOfType() did not return iterable (or iterator)")
    
    def contains(self, instanceType):
        if type(instanceType) is not type:
            raise TypeError("contains() can only test against types (class identifiers, builtins, etc)")

        contains = self._doesContain(instanceType)
        if type(contains) is not bool:
            raise TypeError("_doesContain() did not return a boolean")
        return contains

    @abstractmethod
    def _allOfType(self, instanceType):
        return # iterable or iterator
    
    @abstractmethod
    def _doesContain(self, instanceType):
        return # boolean

    @abstractmethod
    def evaluateRecipes(self):
        return # self with recipes evaluated


# a type of containable that takes a fixed-parameter dictionary
class Factory(Containable, ABC):
    # subclasses must define __init__ to call this one
    @abstractmethod
    def __init__(self, paramNames, product):
        if not isinstance(product, StructureBase):
            raise TypeError("Factory() must be given a structure instance as a product")
        checkedParams = set()
        for param in paramNames:
            if not isinstance(param, Identifier):
                raise TypeError("Factory() tried to create with an invalid parameter")
            if param in checkedParams:
                raise ValueError("Factory() must be given parameters with no repeats")
            checkedParams.add(param)
        
        self.__params = tuple(paramNames)
        self.__productType = type(product)
        self.__ranInit = "initialized"

    def substitute(self, substDict):
        if not hasattr(self, "_Factory__ranInit") or self.__ranInit != "initialized":
            raise RuntimeError("Factory() never had __init__() called")
        # iterate tuple; checking for keys in dict is faster
        for key in self.__params:
            if key not in substDict:
                raise ValueError("Factory() received an invalid substitution dictionary that was missing parameters")
        anyExtra = len(substDict) != len(self.__params)
        if anyExtra:
            raise ValueError("Factory() received a dictionary with extra parameters when substituting")
        return super().substitute(substDict)

    @property
    def parameters(self):
        return self.__params

    @property
    def productType(self):
        return self.__productType

    def evaluateRecipes(self):
        raise RuntimeError("Factory() tried to evaluate recipes before substituting")



class FactoryRecipe(Containable, ABC):
    # subclasses must define __init__ to call this one
    @abstractmethod
    def __init__(self, factoryInstance, params):
        if not isinstance(factoryInstance, Factory):
            raise TypeError("FactoryRecipe() must be given a Factory() as first argument")
        params = tuple(params)
        if len(params) != len(factoryInstance.parameters):
            raise ValueError("FactoryRecipe() received the wrong number of parameters")
        
        self.__factory = factoryInstance
        self.__params = params
        self.__ranInit = "initialized"

    @property
    def factory(self):
        return self.__factory

    @property
    def params(self):
        return self.__params

    # note that this will not evaluate child recipes; it strictly
    # runs its parent factory under self.params
    def evaluate(self):
        if not hasattr(self, "_FactoryRecipe__ranInit") or self.__ranInit != "initialized":
            raise RuntimeError("FactoryRecipe() never had __init__() called")
        paramNames = self.__factory.parameters
        substDict = dict(zip(paramNames, self.__params))
        return self.__factory.substitute(substDict)

    def _doesContain(self, instanceType):
        for param in self.params:
            if isinstance(param, Containable):
                if param.contains(instanceType):
                    return True
            elif isinstance(param, instanceType):
                return True
        return False

    def _allOfType(self, instanceType):
        for param in self.params:
            if isinstance(param, Containable):
                for instance in param.iterateType(instanceType):
                    yield instance
            elif isinstance(param, instanceType):
                yield instance


# represents anything that can be converted to a sympy object (or list of such)
class Symbolable(ABC):
    def asSymbol(self):
        symbol = self._selfAsSymbol()
        if not isinstance(symbol, (sympy.Expr, float, int)):
            raise TypeError("_selfAsSymbol() returned non-symbol type")
        return symbol
    
    @abstractmethod
    def _selfAsSymbol(self):
        return # sympy symbol, expression, or list of such


# represents anything that can be operated on (added, multiplied, etc)
class Expressable(Symbolable, ABC):
    # functions for all operations
    def __unaryOperate(self, operFn, operRep):
        if isinstance(self, Numeric):
            return Numeric(operFn(self.value))
        return Expression(operRep, operFn, [self])
    
    def __binaryOperate(self, other, operFn, operRep):
        if not isinstance(other, Expressable):
            raise TypeError("Expressable() can only operate on other Expressable()s")
        
        if isinstance(self, Numeric) and isinstance(other, Numeric):
            newVal = operFn(self.value, other.value)
            return Numeric(newVal)
        
        else:
            return Expression(operRep, operFn, [self, other])

    # python overloading of operation functions
    def __neg__(self):
        unaryFn = lambda a: -a
        return self.__unaryOperate(unaryFn, '-')

    def __add__(self, other):
        binaryFn = lambda a, b: a + b
        return self.__binaryOperate(other, binaryFn, '+')

    def __sub__(self, other):
        binaryFn = lambda a, b: a - b
        return self.__binaryOperate(other, binaryFn, '-')

    def __mul__(self, other):
        binaryFn = lambda a, b: a * b
        return self.__binaryOperate(other, binaryFn, '*')

    def __truediv__(self, other):
        binaryFn = lambda a, b: a / b
        return self.__binaryOperate(other, binaryFn, '/')

    def __pow__(self, other):
        binaryFn = lambda a, b: a ** b
        return self.__binaryOperate(other, binaryFn, '^')


# abstract base class for all structures
class StructureBase(Displayable, Hashable, ABC):
    # these default implementations can (and probably should) be overridden
    def __hash__(self):
        return hash(str(self))

    def __eq__(self, other):
        return str(self) == str(other)

    @abstractmethod
    def __str__(self):
        return # an equivelantly UNIQUE str (unique unless equal)


class Identifier(Substitutable, StructureBase):
    def __init__(self, idStr):
        self._ensureId(idStr)
        self._id = idStr

    # ensures the Identifier() holds a valid name
    @classmethod
    def _ensureId(cls, idStr):
        if type(idStr) is str:
            # empty strs not allowed
            if len(idStr) > 0:
                validFirsts = set(string.ascii_letters + '_')
                if idStr[0] in validFirsts:
                    validRest = set(string.ascii_letters + string.digits + '_')
                    allValid = True
                    for char in idStr[1:]:
                        if char not in validRest:
                            allValid = False
                            break
                    if allValid:
                        return
        raise ValueError("Identifier() tried to construct with invalid idStr argument")

    def __str__(self):
        return self._id

    @property
    def _reprName(self):
        return "Identifier"

    def _substituteSelf(self, substDict):
        if self in substDict:
            return substDict[self]
        return self


class Numeric(Expressable, StructureBase):
    def __init__(self, number):
        validTypes = (sympy.Number, float, int, str)
        if not isinstance(number, validTypes):
            raise TypeError("Numeric() constructed with invalid value")
        
        # removes floating point errors
        self._value = self._roundFloat(float(number))

    @property
    # the pythonic number representation of this object
    def value(self):
        return self._value

    @property
    def _reprName(self):
        return "Numeric"

    # strigifies value, formatting without the decimal if possible
    def __str__(self):
        forceFormatAboveThis = 1e16
        if self.value < forceFormatAboveThis:
            asInt = int(self.value)
            isInt = asInt == self.value
            if isInt:
                return str(asInt)
        return str(self.value)

    def _selfAsSymbol(self):
        return self._value

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
        # we care about 16 signifigant digits (python uses 16-bit floats);
        # round() using positive arg will round after decimal;
        # with negative arg will round before decimal; this is 
        # the opposite of numDigits, therefore we use -numDigits
        roundTo = 16 - numDigits
        roundFloat = round(rawFloat, roundTo)
        return roundFloat


class Variable(Expressable, Substitutable, StructureBase):
    def __init__(self, identifier):
        if not isinstance(identifier, Identifier):
            raise TypeError("Variable()s can only be created with an Identifier()")
        
        self._id = identifier
        self._symbol = sympy.Symbol(str(self._id))

    @property
    def _reprName(self):
        return "Variable"

    def __str__(self):
        return str(self._id)

    def _selfAsSymbol(self):
        return self._symbol
    
    def _substituteSelf(self, substDict):
        if self in substDict:
            return substDict[self]
        return self


class TemplateCall(Expressable, FactoryRecipe, StructureBase):
    def __init__(self, callId, templateInstance, params):
        if not isinstance(callId, Identifier):
            raise TypeError("TemplateCall() can only use Identifier()s as an identifier")
        if not isinstance(templateInstance, Template):
            raise TypeError("TemplateCall() can only be used with Template() instances")
        
        super().__init__(templateInstance, params)
        self._callId = callId

    @property
    def template(self):
        return self.factory

    @property
    def _reprName(self):
        return "TemplateCall"

    def __str__(self):
        paramStrs = (str(param) for param in self.params)
        return "{}({})".format(self._callId, ','.join(paramStrs))

    def _selfAsSymbol(self):
        raise RuntimeError("Interpreter should have evaluated this out before symbol-ing an expression")

    def _substituteSelf(self, substDict):
        subParams = [
            param.substitute(substDict)
                if isinstance(param, Substitutable)
                else param
            for param in self.params
        ]
        return TemplateCall(self.template, subParams)

    def evaluateRecipes(self):
        evalParams = [
            param.evaluateRecipes()
                if isinstance(param, Containable)
                else param
            for param in self.params
        ]
        # evaluate self for parent container
        return TemplateCall(self.template, evalParams).evaluate()


# TODO: make a distinction between expression with/without templates
class Expression(Expressable, Containable, StructureBase):
    def __init__(self, operRep, operFn, expressables):
        if type(operRep) is not str:
            raise TypeError("Expression() must be given a string representation of its operFn")
        if not callable(operFn):
            raise TypeError("Expression() requires a callable operation function")
        for expressable in expressables:
            if not isinstance(expressable, Expressable):
                raise TypeError("Expression() can only be created with Expressables()")
        validLens = range(1, 3)
        operFnArgCount = self._getArgCounts(operFn)
        if operFnArgCount not in validLens:
            raise ValueError("Expression()s can only operate on one or two arguments")
        if operFnArgCount != len(expressables):
            raise ValueError("Expression() received the wrong number of arguments for its given operation function")
        
        self._operStr = operRep
        self._operFn = operFn
        self._exprs = expressables # will always be tuple (tested)

    @property
    def _reprName(self):
        return "Expression"

    def __str__(self):
        if len(self._exprs) == 1:
            return self._unaryStr()
        elif len(self._exprs) == 2:
            return self._binaryStr()

    def _unaryStr(self):
        expressable = self._exprs[0]
        return "{}{}".format(self._operStr, expressable)

    def _binaryStr(self):
        opInfo = {
            '+': (1, True),
            '-': (1, False),
            '*': (2, True),
            '/': (2, False),
            '^': (3, False),
        }
        precedenceSelf, isCommutative = opInfo[self._operStr]

        leftExpr, rightExpr = self._exprs
        leftStr = str(leftExpr)
        rightStr = str(rightExpr)
        if isinstance(leftExpr, Expression):
            precedenceLeft = opInfo[leftExpr._operStr][0]
            if precedenceSelf > precedenceLeft:
                # preserve order of operations
                leftStr = '({})'.format(leftStr)
        if isinstance(rightExpr, Expression):
            precedenceRight = opInfo[rightExpr._operStr][0]
            if precedenceSelf > precedenceRight:
                # preserve order of operations
                rightStr = '({})'.format(rightStr)
        
        if precedenceSelf == 1:
            # a + b   a - b
            formatStr = "{} {} {}"
        else:
            # a*b  a/b  a^b
            formatStr = "{}{}{}"
        if isCommutative:
            # sorting allows expr(a + b) == expr(b + a)
            leftStr, rightStr = sorted([leftStr, rightStr])
        return formatStr.format(leftStr, self._operStr, rightStr)
    
    def _selfAsSymbol(self):
        syms = (expr.asSymbol() for expr in self._exprs)
        return self._operate(syms)

    def _substituteSelf(self, substDict):
        subExprs = [
            expr.substitute(substDict)
                if isinstance(expr, Substitutable)
                else expr
            for expr in self._exprs
        ]
        # operate simplifies self; this is done becuase working on
        # Numeric(5) is faster than working on Expression(+, 2, 3)
        return self._operate(subExprs)

    def _doesContain(self, instanceType):
        for expr in self._exprs:
            if isinstance(expr, Containable) and expr.contains(instanceType):
                return True
        return False

    def _allOfType(self, instanceType):
        for expr in self._exprs:
            if isinstance(expr, Containable):
                for instance in expr.iterateType(instanceType):
                    yield instance
            elif isinstance(expr, instanceType):
                yield expr

    def evaluateRecipes(self):
        evalExprs = [
            expr.evaluateRecipes()
                if isinstance(expr, Containable)
                else expr
            for expr in self._exprs
        ]
        # operate simplifies self; this is done becuase working on
        # Numeric(5) is faster than working on Expression(+, 2, 3)
        return self._operate(evalExprs)

    def _operate(self, opArgs):
        return self._operFn(*opArgs)

    @classmethod
    def _getArgCounts(cls, fn):
        # (stolen from FilteredDict)
        # through some dir() digging, I found this out...
        #   def fn(arg1, arg2, arg3=3, arg4=4):
        #       someVar = arg1
        #       anotherVar = arg2
        # tried this...
        #   fn.__code__.co_varnames
        #       (arg1, arg2, arg3, arg4, someVar, anotherVar)
        # and this...
        #   fn.__code__.co_varnames[:fn.__code__.co_argcount]
        #       (arg1, arg2, arg3, arg4)
        # listed arguments every time!

        # (this number includes keyword arguments)
        return fn.__code__.co_argcount


class Relation(Symbolable, Containable, StructureBase):
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

    def solve(self, varOrId):
        if not isinstance(varOrId, (Identifier, Variable)):
            raise TypeError("Relation.solve() take a variable or id argument")

        leftSym = self._leftExpr.asSymbol()
        rightSym = self._rightExpr.asSymbol()
        eqSym = leftSym - rightSym
        varSym = varOrId.asSymbol()
        solutions = sympy.solve(eqSym, varSym)
        # some assumptions I made after (a little) testing
        assert type(solutions) is list, "solutions are given in list format"
        for solution in solutions:
            assert isinstance(solution, sympy.Expr), "solutions are all sympy.Expr instances"
        return solutions

    def solveAll(self):
        solutions = dict()
        for var in self.iterateType(Variable):
            if var not in solutions:
                solutions[var] = self.solve(var)
        return solutions
    
    # returns symbol that is assumed equal to zero
    def _selfAsSymbol(self):
        leftSym = self._leftExpr._selfAsSymbol()
        rightSym = self._rightExpr._selfAsSymbol()
        return leftSym - rightSym

    def _substituteSelf(self, substDict):
        if isinstance(self._leftExpr, Substitutable):
            leftSub = self._leftExpr.substitute(substDict)
        else:
            leftSub = self._leftExpr
        if isinstance(self._leftExpr, Substitutable):
            rightSub = self._rightExpr.substitute(substDict)
        else:
            rightSub = self._rightExpr
        return Relation(leftSub, rightSub)

    def _doesContain(self, instanceType):
        if isinstance(self._leftExpr, Containable):
            if self._leftExpr.contains(instanceType):
                return True
        elif isinstance(self._leftExpr, instanceType):
            return True
        elif isinstance(self._rightExpr, Containable):
            if self._rightExpr.contains(instanceType):
                return True
        elif isinstance(self._rightExpr, instanceType):
            return True
        return False

    def _allOfType(self, instanceType):
        if isinstance(self._leftExpr, Containable):
            for instance in self._leftExpr.iterateType(instanceType):
                yield instance
        elif isinstance(self._leftExpr, instanceType):
            yield self._leftExpr

        if isinstance(self._rightExpr, Containable):
            for instance in self._rightExpr.iterateType(instanceType):
                yield instance
        elif isinstance(self._rightExpr, instanceType):
            yield self._rightExpr

    def evaluateRecipes(self):
        if isinstance(self._leftExpr, Containable):
            left = self._leftExpr.evaluateRecipes()
        else:
            left = self._leftExpr

        if isinstance(self._rightExpr, Containable):
            right = self._rightExpr.evaluateRecipes()
        else:
            right = self._rightExpr

        return Relation(left, right)


class Template(Factory, StructureBase):
    def __init__(self, paramNames, rightHand):
        super().__init__(paramNames, rightHand)
        self._rightHand = rightHand

    @property
    def _reprName(self):
        return "Template"

    def __str__(self):
        paramNamesStr = ', '.join(map(str, self.parameters))
        return "({}) -> {}".format(paramNamesStr, self._rightHand)

    def _substituteSelf(self, substDict):
        if isinstance(self._rightHand, Substitutable):
            return self._rightHand.substitute(substDict)
        return self._rightHand

    def _doesContain(self, instanceType):
        if isinstance(self._rightHand, Containable):
            return self._rightHand.contains(instanceType)
        elif isinstance(self._rightHand, instanceType):
            return True

    def _allOfType(self, instanceType):
        if isinstance(self._rightHand, Containable):
            for instance in self._rightHand.iterateType(instanceType):
                yield instance
        elif isinstance(self._rightHand, instanceType):
            yield self._rightHand


if __name__ == "__main__":
    # used to test for missing abstract methods
    print(repr(
        Identifier('myId')
    ))
    print(repr(
        Numeric(4) + Numeric(1)
    ))
    print(repr(
        Variable(Identifier('myVar'))
    ))
    print(repr(
        TemplateCall(Identifier("id"), Template([], Numeric(3.141592)), [])
    ))
    print(repr(
        Expression(
            '-',
            lambda x: -x,
            [Variable(Identifier('a'))],
        )
    ))
    print(repr(
        Expression(
            '*',
            lambda x, y: x * y,
            [
                Variable(Identifier('a')),
                Variable(Identifier('b')),
            ],
        )
    ))
    print(repr(
        Expression(
            '-',
            lambda x, y: x - y,
            [
                Variable(Identifier('a')),
                Variable(Identifier('b')),
            ],
        )
    ))
    print(repr(
        Relation(
            Variable(Identifier('left')),
            Variable(Identifier('right')),
        )
    ))
    print(repr(
        Template([], Numeric(3.141592))
    ))
    print(repr(
        Template([], Relation(
            Variable(Identifier('left')),
            Variable(Identifier('right')),
        ))
    ))
