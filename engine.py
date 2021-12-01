from abc import abstractmethod
from typing import Type

import sympy


class Engine:
    pass


# mixin class that provides a sympy representation of an object
class Symbolable:
    @abstractmethod
    def asSymbol(self):
        return # sympy.Symbol()/<something sympy can deal with, like numbers>


# mixin class that provides an evaluation interface
class Evaluable:
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

    @abstractmethod
    # substDict[symbol] -> numeric value
    def evaluate(self, substDict):
        return # numberic value


# decorator (for classes) that utilizes memory optimizations
# (should obviously me used only for classes
# whose results can be treated as "immutable")
class immutable:
    __immutables = dict()

    def __init__(self, cls):
        self.__cls = cls

    def __call__(self, *args, **kwargs):
        return self.__getMemo(self.__cls, args, kwargs)

    # a decorator that memoizes values from a function/class method
    @classmethod
    def memoize(cls, fn):
        def newFn(*args, **kwargs):
            return cls.__getMemo(fn, args, kwargs)
        return newFn

    @classmethod
    def __getMemo(cls, obj, args, kwargs):
        memoKey = cls.__makeCallId(obj, args, kwargs)
        if memoKey not in cls.__immutables:
            cls.__immutables[memoKey] = obj(*args, **kwargs)
        return cls.__immutables[memoKey]

    @classmethod
    def __makeCallId(cls, obj, args, kwargs):
        return str(obj) + str(args) + str(kwargs)

    def __getattr__(self, name):
        # allows access to original class properties
        return getattr(self.__cls, name)
    

# represents the list of all solutions for variables in a single relation
@immutable
class Solution():
    def __init__(self, relation):
        pass
    
    @immutable.memoize
    def solveVar(symbol):
        pass


# data structure for two equal expressions
@immutable
class Relation(Symbolable):
    def __init__(self, expr1, expr2):
        self._leftExpr = expr1
        self._rightExpr = expr2

    @property
    def left(self):
        return self._leftExpr

    @property
    def right(self):
        return self._rightExpr
    
    # returns symbol that is assumed equal to zero
    @immutable.memoize
    def asSymbol(self):
        return self._leftExpr - self._rightExpr


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
@immutable
class Expression(Symbolable, Evaluable):
    def __init__(self, evalable1, oper, evalable2):
        if not isinstance(evalable1, Evaluable):
            raise TypeError("first argument for Expression must be an evaluable")
        if oper not in OPERATORS:
            raise TypeError("second argument for Expression must be an operator")
        if not isinstance(evalable2, Evaluable):
            raise TypeError("third argument for Expression must be an evaluable")
        self._eval1 = evalable1
        self._eval2 = evalable2
        self._oper = oper

    @immutable.memoize
    def asSymbol(self):
        sym1 = self._eval1.asSymbol()
        sym2 = self._eval2.asSymbol()
        return self._operate(sym1, sym2)

    @immutable.memoize
    def evaluate(self, substDict):
        val1 = self._eval1.evaluate(substDict)
        val2 = self._eval2.evaluate(substDict)
        return self._operate(val1, val2)

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
            raise ValueError("Expression tried to evaluate with an invalid operator")
        # resolves floating-point errors
        return round(result, 12)
        

@immutable
class NegativeExpression(Symbolable, Evaluable):
    def __init__(self, evalable):
        if not isinstance(evalable, Evaluable):
            raise TypeError("first argument for NegativeExpression must be an evaluable")
        self._eval = evalable
        self._evalSym = None
        self._evalEval = None

    @immutable.memoize
    def asSymbol(self):
        if self._evalSym is not None:
            return self._evalSym

        sym = self._eval.asSymbol()
        self._evalSym = -sym
        return self._evalSym

    @immutable.memoize
    def evaluate(self, substDict):
        if self._evalEval is not None:
            return self._evalEval

        val = self._eval.evaluate(substDict)
        self._evalEval = -val
        return self._evalEval


# data structure for defining (e)value(able)s (identifiers or numbers)
@immutable
class Value(Symbolable, Evaluable):
    @abstractmethod
    def __init__(self):
        self._symbol = None # set to sympy-processable value

    def asSymbol(self):
        return self._symbol

    def evaluate(self, substDict):
        return substDict[self._symbol]

class NumericValue(Value):
    def __init__(self, number):
        validTypes = (float, int, str)
        if type(number) not in validTypes:
            raise TypeError("(Internal error) NumericValue constructed with invalid value")
        self._symbol = float(number)

class IdentifierValue(Value):
    def __init__(self, identifier):
        if type(identifier) is not str:
            raise TypeError("(Internal error) IdentifierValue constructed with invalid value")
        self._symbol = sympy.Symbol(identifier)
