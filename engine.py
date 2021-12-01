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
    def __init__(self, exp1, exp2):
        pass
    
    # returns symbol that is assumed equal to zero
    @immutable.memoize
    def asSymbol(self):
        pass


# data structure for performing operations
@immutable
class Expression(Symbolable, Evaluable):
    def __init__(self, evalable1, oper, evalable2):
        pass

    @immutable.memoize
    def asSymbol(self):
        pass

    @immutable.memoize
    def evaluate(self, substDict):
        pass

@immutable
class NegativeExpression(Symbolable, Evaluable):
    def __init__(self, evalable):
        pass

    @immutable.memoize
    def asSymbol(self):
        pass

    @immutable.memoize
    def evaluate(self, substDict):
        pass


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
