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
class Representation(Displayable, AbstractClass):
    @abstractmethod
    def __init__(self, *args, **kwargs):
        return  # nothing; its just __init__

    @abstractmethod
    def construct(self, *args, **kwargs):
        return  # a new object from the representation

    def traverse(self, reprType, onReprFn):
        assert onReprFn.__code__.co_argcount == 1

        self._traverseChildren(reprType, onReprFn)
        if reprType is type(self):
            result = onReprFn(self)
            if result is None:
                result = self
        else:
            result = self
        
        assert isinstance(result, Representation)
        return result
    
    @abstractmethod
    def _traverseChildren(self, reprType, onReprFn):
        return  # nothing, but should call traverse() for each "child" (if any)


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
        # this function intentionally does NOT return an actual Identifier()
        # (since these will be manually created by the interpreter)
        return self._idStr

    def _traverseChildren(self, reprType, onReprFn):
        # no children
        pass


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
    
    def _traverseChildren(self, reprType, onReprFn):
        # no children
        pass


class VariableRepresentation(Representation):
    def __init__(self, idRep):
        assert type(idRep) is IdentifierRepresentation
        self._idRep = idRep

    def __repr__(self):
        return "<VariableRep '{}'>".format(self._idRep._idStr)

    def construct(self):
        nameStr = self._idRep.construct()
        return sympy.Symbol(nameStr)

    def _traverseChildren(self, reprType, onReprFn):
        self._idRep = self._idRep.traverse(reprType, onReprFn)


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

    def _traverseChildren(self, reprType, onReprFn):
        # no children
        pass


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

    def _traverseChildren(self, reprType, onReprFn):
        self._operRep = self._operRep.traverse(reprType, onReprFn)
        self._operArgs = [
            argRep.traverse(reprType, onReprFn)
            for argRep in self._operArgs
        ]


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

    def _traverseChildren(self, reprType, onReprFn):
        self._parameters = [
            paramRep.traverse(reprType, onReprFn)
            for paramRep in self._parameters
        ]


class Model(Displayable):
    pass  # intentionally empty; provides a common type to inherit from


class RoundedFloat(Model):
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
    
    def __init__(self, floatable):
        self._val = float(floatable)

    def __repr__(self):
        return "<RoundedFloat {}>".format(self._val)

    def __str__(self):
        return str(self._val)

    # TODO: algebra functions (add, sub, mult, div, etc)

class Identifier(Model):
    def __init__(self, idStr):
        assert type(idStr) is str
        self._idStr = idStr

    def __repr__(self):
        return "<Identifier: {}>".format(self._idStr)

    def __str__(self):
        return self._idStr

    @property
    def idStr(self):
        return self._idStr


class Template(Model):
    def __init__(self, templateId, paramNames, templateStr):
        self._templateId = templateId
        self._paramNames = paramNames
        self._templateStr = templateStr

    def __repr__(self):
        return "<Template: {}({}) -> {}>".format(self._templateId, self._paramNames, self._templateStr)

    def evaluate(self, params):
        adjPattern = "[^a-zA-Z]"
        patterns = [
            # essentially: a (by itself) || a (at start followed by non-char)
            #     || a (at end preceeded by non-char) || a (followed and
            #     preceded by non-char)
            "^{0}$|^{0}(?={1})|(?<={1}){0}$|(?<={1}){0}(?={1})".format(paramName, adjPattern)
            for paramName in self._paramNames
        ]
        regex = re.compile("({})".format("|".join(patterns)))
        def matchMap(match):
            matchStr = match.string[match.start():match.end()]
            paramIdx = self._paramNames.index(matchStr)
            return params[paramIdx]
        return regex.sub(matchMap, self._templateStr)


class TemplateCall(Model):
    def __init__(self, templateId, params):
        self._templateId = templateId
        self._params = params

    def __repr__(self):
        return "<TemplateCall: {}({})>".format(self._templateId, self._params)

    @property
    def templateId(self):
        return self._templateId

    @property
    def params(self):
        return self._params


if __name__ == "__main__":
    e = ExpressionRepresentation(OperatorRepresentation(lambda x, y: x + y, "+"), [
        ExpressionRepresentation(OperatorRepresentation(lambda x, y: x + y, "+"), [
            NumberRepresentation("4.5"),
            VariableRepresentation(IdentifierRepresentation("x")),
        ]),
        NumberRepresentation("6")
    ])

    def onFn(rep):
        print(rep)
        return DummyRepresentation([1,2,3])
    
    e = e.traverse(VariableRepresentation, onFn)
    print(e)
    