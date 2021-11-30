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


# represents the list of all solutions for variables in a single relation
class Solution(Symbolable):
    def __init__(self, solutionDict):
        pass

    def asSymbol(self):
        pass


# data structure for two equal expressions
class Relation(Symbolable):
    def __init__(self, exp1, exp2):
        pass
    
    # returns symbol that is assumed equal to zero
    def asSymbol(self):
        pass


# data structure for performing operations
class Expression(Symbolable, Evaluable):
    def __init__(self, evalable1, oper, evalable2):
        pass

    def asSymbol(self):
        pass

    def evaluate(self, substDict):
        pass

class NegativeExpression(Symbolable, Evaluable):
    def __init__(self, evalable):
        pass

    def asSymbol(self):
        pass

    def evaluate(self, substDict):
        pass


# data structure for defining (e)value(able)s (identifiers or numbers)
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