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
        assert onReprFn.__code__.co_argcount == 1, "traverse() callback must only accept one argument"

        self._traverseChildren(reprType, onReprFn)
        if reprType is type(self):
            result = onReprFn(self)
            if result is None:
                result = self
        else:
            result = self
        
        if not isinstance(result, Representation):
            raise TypeError("traverse() callback must return a Representation-type (or None)")
        return result
    
    @abstractmethod
    def _traverseChildren(self, reprType, onReprFn):
        return  # nothing, but should call traverse() for each "child" (if any)


class DummyRepresentation(Representation):
    def __init__(self, obj):
        self._obj = obj

    def __repr__(self):
        return "<DummyRep '{}'>".format(repr(self._obj))

    def construct(self):
        return self._obj

    def _traverseChildren(self, reprType, onReprFn):
        # no (Representation) children
        pass


class IdentifierRepresentation(Representation):
    def __init__(self, idStr):
        assert type(idStr) is str, "'idStr' must be a string"
        self._idStr = idStr

    def __repr__(self):
        return "<IdentifierRep '{}'>".format(self._idStr)

    def construct(self):
        # this function intentionally does NOT return an actual Identifier()
        # (since these will be manually created by the interpreter)
        return Identifier(self._idStr)

    def _traverseChildren(self, reprType, onReprFn):
        # no children
        pass


class NumberRepresentation(Representation):
    def __init__(self, numStr):
        assert type(numStr) is str, "'numStr' must be a string"
        self._numStr = numStr

    def __repr__(self):
        return "<NumberRep '{}'>".format(self._numStr)

    def construct(self):
        isFloat = False
        for char in self._numStr:
            if char in [".", "E", "e"]:
                isFloat = True
                break
        
        if isFloat:
            return float(self._numStr)
        else:
            return int(self._numStr)

    def _traverseChildren(self, reprType, onReprFn):
        # no children
        pass


class VariableRepresentation(Representation):
    def __init__(self, idRep):
        assert type(idRep) is IdentifierRepresentation, "Only IdentifierRepresentations are acceptable inputs"
        self._idRep = idRep

    def __repr__(self):
        return "<VariableRep '{}'>".format(self._idRep._idStr)

    def construct(self):
        identifier = self._idRep.construct()
        nameStr = str(identifier)
        return sympy.Symbol(nameStr)

    def _traverseChildren(self, reprType, onReprFn):
        newIdRep = self._idRep.traverse(reprType, onReprFn)
        if newIdRep is not self._idRep:
            return VariableRepresentation(newIdRep)
        else:
            return self



class OperatorRepresentation(Representation):
    def __init__(self, operFn, operStr):
        assert isfunction(operFn), "Operator function was not a function"
        assert type(operStr) is str, "Operator representation string was not a string"
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
    _noOper = OperatorRepresentation(lambda x: x, "$")

    @classmethod
    def wrap(cls, obj):
        # this assertion is performed to avoid treating unconstructed Representations
        # as constructed values (otherwise, we might try to add two
        # NumericRepresentations later... which is impossible)
        assert not isinstance(obj, Representation), "wrap() is meant for (cached) values that match the return type of some Representation.construct(); use convert() for Representations themselves"
        argsThatConstructToVal = [DummyRepresentation(obj)]
        return ExpressionRepresentation(cls._noOper, argsThatConstructToVal)

    @classmethod
    def convert(cls, rep):
        assert isinstance(rep, Representation), "convert() is meant to cast Representations into ExpressionRepresentations; use wrap() to provide a raw, pre-constructed value"
        argsThatConstructToVal = [rep]
        return ExpressionRepresentation(cls._noOper, argsThatConstructToVal)

    def __init__(self, operRep, operArgs):
        assert type(operRep) is OperatorRepresentation, "Can only receive Operator Representations as first argument"
        assert type(operArgs) in (tuple, list), "Operator arguments must be tuple or list"
        assert all(isinstance(arg, Representation) for arg in operArgs), "Operator arguments must contain Representation objects"
        self._operRep = operRep
        self._operArgs = operArgs

    def __repr__(self):
        return "<ExpressionRep '{}': ({})>".format(self._operRep.operStr, ",".join(repr(arg) for arg in self._operArgs))

    def construct(self):
        constructedArgs = [arg.construct() for arg in self._operArgs]
        operFn = self._operRep.construct()
        return operFn(*constructedArgs)

    def _traverseChildren(self, reprType, onReprFn):
        newOperRep = self._operRep.traverse(reprType, onReprFn)
        newOperArgs = [
            argRep.traverse(reprType, onReprFn)
            for argRep in self._operArgs
        ]
        if newOperRep is not self._operRep or any(newOperArg is not operArg for (newOperArg, operArg) in zip(newOperArgs, self._operArgs)):
            return ExpressionRepresentation(newOperRep, newOperArgs)
        else:
            return self


class TemplateCallRepresentation(Representation):
    def __init__(self, templateIdRep, parameters):
        assert type(templateIdRep) is IdentifierRepresentation, "Template ID must be a string"
        assert type(parameters) in (tuple, list), "Parameters must be a tuple or list"
        assert all(isinstance(param, Representation) for param in parameters), "Parameters must only contain Representation objects"
        self._templateIdRep = templateIdRep
        self._parameters = parameters

    def __repr__(self):
        return "<TemplateCallRep '{}': ({})>".format(self._templateIdRep._idStr, ",".join(repr(arg) for arg in self._parameters))

    def construct(self):
        identifier = self._templateIdRep.construct()
        return TemplateCall(identifier, self._parameters)

    def _traverseChildren(self, reprType, onReprFn):
        newIdRep = self._templateIdRep.traverse(reprType, onReprFn)
        newParameters = [
            paramRep.traverse(reprType, onReprFn)
            for paramRep in self._parameters
        ]
        if newIdRep is not self._templateIdRep or any(newParam is not param for (newParam, param) in zip(newParameters, self._parameters)):
            return TemplateCallRepresentation(newIdRep, newParameters)
        else:
            return self


class Model(Displayable):
    pass  # intentionally empty; provides a common type to inherit from


class RoundedFloat(Model):
    @classmethod
    def roundFloat(cls, rawFloat, sig=15):
        # special case for 0 (log10 gives domain error)
        if rawFloat == 0:
            return rawFloat
        # number of digits to most signifigant figure
        numDigits = int(log10(abs(rawFloat)))
        if numDigits >= 0:
            # because log10(1) gives 0; we want 1
            numDigits += 1
        # we care about 15 signifigant digits (python uses 16-bit floats);
        # round() using positive arg will round after decimal;
        # with negative arg will round before decimal; this is
        # the opposite of numDigits, therefore we use -numDigits
        roundTo = sig - numDigits
        roundFloat = round(rawFloat, roundTo)
        return roundFloat
    
    def __init__(self, floatable):
        self._val = float(floatable)

    def __repr__(self):
        return "<RoundedFloat {}>".format(self._val)

    def __str__(self):
        if self._val // 1 == self._val:
            return str(int(self._val))
        else:
            return str(self._val)

    def __hash__(self):
        return hash(self._val)

    def _convertArithmetic(self, other, opFn):
        if type(other) is RoundedFloat:
            return RoundedFloat(self.roundFloat(opFn(self._val, other._val)))
        elif type(other) in {int, float}:
            return RoundedFloat(self.roundFloat(opFn(self._val, other)))
        else:
            return opFn(self._val, other)

    def __add__(self, other):
        return self._convertArithmetic(other, lambda s, o: s + o)
    
    def __radd__(self, other):
        return self._convertArithmetic(other, lambda s, o: o + s)

    def __sub__(self, other):
        return self._convertArithmetic(other, lambda s, o: s - o)

    def __rsub__(self, other):
        return self._convertArithmetic(other, lambda s, o: o - s)

    def __mul__(self, other):
        return self._convertArithmetic(other, lambda s, o: s * o)

    def __rmul__(self, other):
        return self._convertArithmetic(other, lambda s, o: o * s)

    def __truediv__(self, other):
        return self._convertArithmetic(other, lambda s, o: s / o)

    def __rtruediv__(self, other):
        return self._convertArithmetic(other, lambda s, o: o / s)

    def __floordiv__(self, other):
        return self._convertArithmetic(other, lambda s, o: s // o)

    def __rfloordiv__(self, other):
        return self._convertArithmetic(other, lambda s, o: o // s)

    def __mod__(self, other):
        return self._convertArithmetic(other, lambda s, o: s % o)

    def __rmod__(self, other):
        return self._convertArithmetic(other, lambda s, o: o % s)

    def __pow__(self, other):
        return self._convertArithmetic(other, lambda s, o: s ** o)

    def __rpow__(self, other):
        return self._convertArithmetic(other, lambda s, o: o ** s)

    def __lt__(self, other):
        return self._val < other

    def __gt__(self, other):
        return self._val > other

    def __le__(self, other):
        return self._val <= other

    def __ge__(self, other):
        return self._val >= other

    def __eq__(self, other):
        return self._val == other

    def __ne__(self, other):
        return self._val != other

    def __neg__(self):
        return RoundedFloat(-self._val)

    def __pos__(self):
        return RoundedFloat(+self._val)



class Identifier(Model):
    def __init__(self, idStr):
        assert type(idStr) is str, "idStr must be a string"
        self._idStr = idStr

    def __repr__(self):
        return "<Identifier: {}>".format(self._idStr)

    def __str__(self):
        return self._idStr

    def __hash__(self):
        return hash(self._idStr)

    def __eq__(self, other):
        return type(other) is Identifier and self._idStr == other._idStr

    @property
    def idStr(self):
        return self._idStr


class Template(Model):
    def __init__(self, templateId, paramNames, templateStr):
        assert type(templateId) is Identifier, "'templateId' must be an Identifier"
        assert type(paramNames) in (tuple, list), "Parameter names must be a tuple or list"
        assert all(type(paramId) is Identifier for paramId in paramNames), "'paramNames' must be an iterable of Identifiers"
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
        assert type(templateId) is Identifier, "'templateId' must be an Identifier"
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


class SubDict(dict):
    def __init__(self, dictLike=None, conditions=None):
        if dictLike is None:
            dictLike = dict()
        if conditions is None:
            if type(dictLike) is SubDict:
                conditions = dictLike._conditions
            else:    
                conditions = set()
        
        dictLike = self._assertValidSubDictLike(dictLike)
        super().__init__(dictLike)
        self.conditions = conditions

    @property
    def conditions(self):
        return self._conditions

    @conditions.setter
    def conditions(self, newConditions):
        self._conditions = set(newConditions)
        assert all(self._isValidKey(condition) for condition in self._conditions), "SubDict got an invalid conditions set"

    def __setitem__(self, key, val):
        assert self._isValidKey(key) and self._isValidVal(val), "SubDict got an invalid key-value pair"
        return super().__setitem__(key, val)

    def update(self, dictLike):
        dictLike = self._assertValidSubDictLike(dictLike)
        return super().update(dictLike)

    def _assertValidSubDictLike(self, dictLike):
        if __debug__:
            dictLike = dict(dictLike)
            assert all(self._isValidKey(key) and self._isValidVal(val) for key in dictLike for val in [key]), "SubDict got invalid key-value pairs"
        return dictLike

    def _isValidKey(self, key):
        return isinstance(key, sympy.Expr)

    def _isValidVal(self, val):
        return isNumeric(val) or isinstance(val, sympy.Basic)


class SubDictList(list):
    def __init__(self, setLike):
        setLike = self._assertValidItems(setLike)
        super().__init__(setLike)

    def append(self, subDict):
        assert type(subDict) is SubDict, "SubDictList can only add SubDicts as items"
        super().append(subDict)

    def extend(self, setLike):
        setLike = self._assertValidItems(setLike)
        super().update(setLike)

    def insert(self, index, subDict):
        assert type(subDict) is SubDict, "SubDictList can only add SubDicts as items"
        super().insert(index, subDict)

    def _assertValidItems(self, setLike):
        if __debug__:
            setLike = set(setLike)
            assert all(type(subDict) is SubDict for subDict in setLike), "SubDictList can only update SubDicts as items"
        return setLike


def isNumeric(obj):
    isRawType = isinstance(obj, (int, float, RoundedFloat, sympy.Number, sympy.NumberSymbol))
    if isRawType:
        return True
    try:
        obj = float(obj)
        return True
    except TypeError:
        return False


class NumericSymbol(sympy.Symbol):
    def __new__(cls, repFloatOrStr):
        try:
            float(repFloatOrStr)
        except TypeError:
            raise TypeError("NumericSymbol requires a float-able argument")
        return sympy.Symbol.__new__(cls, str(repFloatOrStr))


def symbolToIdentifier(sympyObj):
    assert isinstance(sympyObj, sympy.Symbol), "symbolToIdentifier() requires a sympy object"
    assert not isNumeric(sympyObj), "symbolToIdentifier() cannot convert numbers"
    return Identifier(str(sympyObj))


def identifierToSymbol(identifier):
    assert type(identifier) is Identifier, "identifierToSymbol() only takes Identifiers"
    return sympy.Symbol(str(identifier))


def toNumber(something):
    try:
        num = float(something)
    except TypeError:
        raise TypeError("toNumber() requires something that can at least convert to a float")
    numAsInt = int(num)
    if num == numAsInt:
        return numAsInt
    else:
        return num


def iterDifference(iter1, iter2):
    for item in iter1:
        if item not in iter2:
            yield item

_firstNoDefault = object()
def first(iterable, default=_firstNoDefault):
    try:
        for item in iterable:
            firstItem = item
            # because we...
            return firstItem
    except StopIteration:
        if default is _firstNoDefault:
            raise ValueError("Iterable contained no elements (and no default value was given)")
        else:
            return default


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
    