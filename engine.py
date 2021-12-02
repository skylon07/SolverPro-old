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
class Substitutable:
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
    # substDict[symbol] -> Substitutable() or numeric
    def substitute(self, substDict):
        return # Substitutable() or numeric


# decorator that utilizes memory optimizations
# (should obviously me used only for classes/functions
# whose results can be treated as "immutable")
class immutable:
    _immutables = dict()

    # ran when python "calls the decorator"
    def __new__(cls, anyCallable):
        isClass = type(anyCallable) is type
        if isClass:
            class ImmutableMixin(anyCallable):
                def __new__(mixinCls, *args, **kwargs):
                    return cls._getMemo(anyCallable, args, kwargs)
            return ImmutableMixin
        else:
            def memoWrapper(*args, **kwargs):
                return cls._getMemo(anyCallable, args, kwargs)
            return memoWrapper

    @classmethod
    def _getMemo(cls, someCallable, argsTuple, kwargsDict):
        memoKey = cls._makeCallId(someCallable, argsTuple, kwargsDict)
        if memoKey not in cls._immutables:
            cls._immutables[memoKey] = someCallable(*argsTuple, **kwargsDict)
        return cls._immutables[memoKey]

    @classmethod
    def _makeCallId(cls, someCallable, argsTuple, kwargsDict):
        return str(id(someCallable)) + str(argsTuple) + str(kwargsDict)
    
    

# represents the list of all solutions for variables in a single relation
@immutable
class Solution():
    def __init__(self, relation):
        pass
    
    @immutable
    def solveVar(symbol):
        pass


# data structure for two equal expressions
@immutable
class Relation(Symbolable):
    def __init__(self, subsable1, subsable2):
        if not isinstance(subsable1, Substitutable):
            raise TypeError("first argument for Relation must be a substitutable")
        if not isinstance(subsable2, Substitutable):
            raise TypeError("second argument for Relation must be a substitutable")
        
        self._leftExpr = subsable1
        self._rightExpr = subsable2

    @property
    def left(self):
        return self._leftExpr

    @property
    def right(self):
        return self._rightExpr
    
    # returns symbol that is assumed equal to zero
    @immutable
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
class Expression(Symbolable, Substitutable):
    def __init__(self, subsable1, oper, subsable2):
        if not isinstance(subsable1, Substitutable):
            raise TypeError("first argument for Expression must be a substitutable")
        if oper not in OPERATORS:
            raise TypeError("second argument for Expression must be an operator")
        if not isinstance(subsable2, Substitutable):
            raise TypeError("third argument for Expression must be a substitutable")
        
        self._subsable1 = subsable1
        self._subsable2 = subsable2
        self._oper = oper

    @immutable
    def asSymbol(self):
        sym1 = self._subsable1.asSymbol()
        sym2 = self._subsable2.asSymbol()
        return self._operate(sym1, sym2)

    @immutable
    def substitute(self, substDict):
        val1 = self._subsable1.substitute(substDict)
        val2 = self._subsable2.substitute(substDict)
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
            raise ValueError("Expression tried to substitute with an invalid operator")
        # resolves floating-point errors
        return round(result, 12)
        

@immutable
class NegativeExpression(Symbolable, Substitutable):
    def __init__(self, subsable):
        if not isinstance(subsable, Substitutable):
            raise TypeError("first argument for NegativeExpression must be a substitutable")
        
        self._subsable = subsable

    @immutable
    def asSymbol(self):
        sym = self._subsable.asSymbol()
        return -sym

    @immutable
    def substitute(self, substDict):
        val = self._subsable.substitute(substDict)
        return -val


# data structure for defining (e)value(able)s (identifiers or numbers)
class Value(Symbolable, Substitutable):
    @abstractmethod
    def __init__(self):
        self._symbol = None # set to sympy-processable value

    @immutable
    def asSymbol(self):
        return self._symbol

    @immutable
    def substitute(self, substDict):
        return substDict[self._symbol]

@immutable
class NumericValue(Value):
    def __init__(self, number):
        validTypes = (float, int, str)
        if type(number) not in validTypes:
            raise TypeError("NumericValue constructed with invalid value")
            
        self._symbol = float(number)

@immutable
class IdentifierValue(Value):
    def __init__(self, identifier):
        if type(identifier) is not str:
            raise TypeError("IdentifierValue constructed with invalid value")

        self._symbol = sympy.Symbol(identifier)
