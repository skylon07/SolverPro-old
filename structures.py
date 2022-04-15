from abc import ABC as AbstractClass, abstractmethod, abstractproperty
from inspect import isfunction
from math import log10
import re

import sympy

from requiredmethods import Requireable, requiredCall, ensuredCall


class Displayable(AbstractClass):
    @abstractmethod
    def __repr__(self):
        return  # a string representation of this object


# an interface for the parser that stores data on how to create some object,
# delegating actual creation to the interpreter (allowing it to detect errors
# and print better error messages)
class Representation(Displayable, Requireable, AbstractClass):
    @abstractmethod
    def __init__(self, *args, **kwargs):
        return  # nothing; its just __init__

    @abstractmethod
    def construct(self, *args, **kwargs):
        return  # a new object from the representation


# TODO: RepresentationByString mixin
# TODO: consider how to evaluate Represent_Expression
#       (doesn't it need to be given Represents?)


class IdentifierRepresentation(Representation):
    def __init__(self, idStr):
        assert type(idStr) is str
        self._idStr = idStr

    def __repr__(self):
        return "<IdentifierRep '{}'>".format(self._idStr)

    def construct(self):
        return self._idStr


class NumberRepresentation(Representation):
    def __init__(self, numStr):
        assert type(numStr) is str
        self._numStr = numStr

    def __repr__(self):
        return "<NumberRep '{}'>".format(self._numStr)

    def construct(self):
        return self._roundFloat(float(self._numStr))

    def _roundFloat(self, rawFloat):
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


class VariableRepresentation(Representation):
    def __init__(self, idRep):
        assert type(idRep) is IdentifierRepresentation
        self._idRep = idRep

    def __repr__(self):
        return "<VariableRep '{}'>".format(self._idRep._idStr)

    def construct(self):
        nameStr = self._idRep.construct()
        return sympy.Symbol(nameStr)


class OperatorRepresentation(Representation):
    def __init__(self, operFn, operStr):
        assert isfunction(operFn)
        assert type(operStr) is str
        self._operFn = operFn
        self._operStr = operStr
    
    def __repr__(self):
        return "<OperatorRep '{}'>".format(self._operStr)
    
    @property
    def operStr(self):
        return self._operStr

    def construct(self):
        return self._operFn


class ExpressionRepresentation(Representation):
    def __init__(self, operRep, operArgs):
        assert type(operRep) is OperatorRepresentation
        assert iter(operArgs) is not None
        self._operRep = operRep
        self._operArgs = operArgs

    def __repr__(self):
        return "<ExpressionRep '{}': ({})>".format(self._operRep.operStr, ",".join(repr(arg) for arg in self._operArgs))

    def construct(self):
        constructedArgs = [arg.construct() for arg in self._operArgs]
        operFn = self._operRep.construct()
        return operFn(*constructedArgs)


class TemplateCallRepresentation(Representation):
    def __init__(self, templateIdStr, parameters):
        assert type(templateIdStr) is str
        assert iter(parameters) is not None
        self._templateIdStr = templateIdStr
        self._parameters = parameters

    def __repr__(self):
        return "<TemplateCallRep '{}': ({})>".format(self._templateIdStr, ",".join(repr(arg) for arg in self._operArgs))

    def construct(self):
        return TemplateCall(self._templateIdStr, self._parameters)


if __name__ == "__main__":
    # used to test for missing abstract methods
    pass
