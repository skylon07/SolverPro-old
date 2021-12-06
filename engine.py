from abc import ABC, abstractmethod, abstractproperty
from math import log10

import sympy


class Engine:
    def __init__(self):
        self._aliases = dict()

    def setAlias(self, identifier, value):
        if not isinstance(identifier, VariableValue):
            raise TypeError("Engine.setAlias(identifier, value) -- identifier was not an VariableValue()")
        if not isinstance(value, Value):
            raise TypeError("Engine.setAlias(identifier, value) -- value was not a Value()")

        self._aliases[identifier] = value

    def setAliases(self, identifiers, value):
        if not isinstance(value, Value):
            raise TypeError("Engine.setAliases(identifiers, value) -- value was not a Value()")
        for identifier in identifiers:
            if not isinstance(identifier, VariableValue):
                raise TypeError("Engine.setAliases(identifiers, value) -- identifiers was not a list of VariableValue()s")
        
        for identifier in identifiers:
            self.setAlias(identifier, value)

    def getAlias(self, identifier):
        return self._aliases.get(identifier)

    # substitutes all known values into a given object
    def substitute(self, subsable):
        if not isinstance(subsable, Substitutable):
            raise TypeError("Engine.evaluate(subsable) -- subsable must be a Substitutable()")
        
        substDict = self._aliases
        return subsable.substitute(substDict)

    def roundFloat(self, numericValue):
        if not isinstance(numericValue, NumericValue):
            raise TypeError("Engine.roundFloat() received a non-NumericValue()")
        
        rawFloat = numericValue.asSymbol(self._identifiers)
        # special case for 0 (log10 gives domain error)
        if rawFloat == 0:
            return numericValue
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
        return NumericValue(roundFloat)


# abstract class that gives a blueprint for repr strings for objects
class Displayable(ABC):
    @abstractmethod
    def __str__(self):
        return # some string representation
    
    def __repr__(self):
        return "<{}: {}>".format(self._reprName, self)

    @abstractproperty
    def _reprName(self):
        return # name that represents type(self)
    

# mixin class that provides a sympy representation of an object
class Symbolable(ABC):
    @abstractmethod
    def asSymbol(self, templatesDict):
        return # sympy.Symbol()/<something sympy can deal with, like numbers>


# mixin class that provides an evaluation interface
class Substitutable(ABC):
    @abstractmethod
    # substDict[Identifier()] -> Substitutable()
    def substitute(self, substDict):
        return # Substitutable()


# decorator that utilizes memory optimizations
# (should obviously me used only for classes/functions
# whose results can be treated as "immutable")
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
        argsHash = repr(argsTuple)
        # kwargsHash ignored; kwargs not currently allowed
        return hash((callHash, argsHash))


# represents a single name within the program
# (used as the key to a dictionary holding what it identifies)
class Identifier(Displayable):
    def __init__(self, idStr):
        self._ensureId(idStr)
        self._id = idStr

    def __str__(self):
        return self._id

    # these functions are defined to allow equivelant identifiers
    # to modify the same dictionary bucket
    def __eq__(self, otherId):
        if isinstance(otherId, Identifier):
            return self._id == otherId
        return False

    def __hash__(self):
        return hash(self._id)

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


# represents the list of all solutions for variables in a single relation
class Solution():
    def __init__(self, relation):
        pass

    def __repr__(self):
        pass

    def solveVar(symbol):
        pass


# data structure for two equal expressions
class Relation(Symbolable, Substitutable, Displayable):
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
    def asSymbol(self, templatesDict):
        return self._leftExpr - self._rightExpr

    def substitute(self, substDict):
        leftSub = self._leftExpr.substitute(substDict)
        rightSub = self.rightExpr.substitute(substDict)
        return Relation(leftSub, rightSub)

# abstract class that gives operators to expression-like objects
class Expressable(Symbolable, Substitutable, Displayable):
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
        if oper not in OPERATORS:
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
        return "{} {} {}".format(self._leftExpr, self._oper, self._rightExpr)
    
    def asSymbol(self, templatesDict):
        sym1 = self._leftExpr.asSymbol(templatesDict)
        sym2 = self._rightExpr.asSymbol(templatesDict)
        return self._operate(sym1, sym2)

    def substitute(self, substDict):
        val1 = self._leftExpr.substitute(substDict)
        val2 = self._rightExpr.substitute(substDict)
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
        if isinstance(result, NumericValue):
            return Engine.roundFloat(result)
        return result
        

class NegativeExpression(Expressable):
    def __init__(self, expr):
        if not isinstance(expr, Expressable):
            raise TypeError("first argument for NegativeExpression must be a substitutable")
        
        self._expr = expr

    def __str__(self):
        if isinstance(self._expr, (Value, TemplateCall)):
            return "-{}".format(self._expr)
        return "-({})".format(self._expr)

    def asSymbol(self, templatesDict):
        sym = self._expr.asSymbol(templatesDict)
        return -sym

    def substitute(self, substDict):
        result = self._expr.substitute(substDict)
        return -result


# abstract data structure for defining (e)value(able)s (variables or numbers)
class Value(Expressable):
    @property
    def _reprName(self):
        return "Value"
    
    def __str__(self):
        return str(self._symbol)

    def asSymbol(self, templatesDict):
        return self._symbol

    def substitute(self, substDict):
        sub = substDict.get(self)
        if sub:
            result = sub.substitute(substDict)
        else:
            result = self

        if not isinstance(result, Substitutable):
            raise TypeError("Value() substituted and got a non-Substitutable()")
        return result

    @abstractproperty
    def _symbol(self):
        return # sympy-processable value

class NumericValue(Value):
    def __init__(self, number):
        validTypes = (float, int, str)
        if type(number) not in validTypes:
            raise TypeError("NumericValue constructed with invalid value")
            
        self.__symbol = float(number)

    @property
    def _symbol(self):
        return self.__symbol

    # overrides Value.__str__ (allows printing ints)
    def __str__(self):
        forceFormatAboveThis = 1e16
        if self._symbol < forceFormatAboveThis:
            symAsInt = int(self._symbol)
            isInt = symAsInt == self._symbol
            if isInt:
                return str(symAsInt)
        return super().__str__()

    def __add__(self, other):
        if isinstance(other, NumericValue):
            newVal = self._symbol + other._symbol
            return NumericValue(newVal)
        return super().__add__(other)
    
    def __sub__(self, other):
        if isinstance(other, NumericValue):
            newVal = self._symbol - other._symbol
            return NumericValue(newVal)
        return super().__sub__(other)

    def __mul__(self, other):
        if isinstance(other, NumericValue):
            newVal = self._symbol * other._symbol
            return NumericValue(newVal)
        return super().__mul__(other)

    def __truediv__(self, other):
        if isinstance(other, NumericValue):
            newVal = self._symbol / other._symbol
            return NumericValue(newVal)
        return super().__truediv__(other)

    def __pow__(self, other):
        if isinstance(other, NumericValue):
            newVal = self._symbol ** other._symbol
            return NumericValue(newVal)
        return super().__pow__(other)


class VariableValue(Value):
    def __init__(self, identifier):
        if type(identifier) is not str:
            raise TypeError("VariableValue constructed with invalid value")

        self.__symbol = sympy.Symbol(identifier)

    @property
    def _symbol(self):
        return self.__symbol


class Template(Substitutable, Displayable):
    def __init__(self, paramNames, rightHand):
        for paramName in paramNames:
            if not isinstance(paramName, Identifier):
                raise TypeError("first argument for Template() must be a list of Identifier()s")
        if not isinstance(rightHand, Substitutable):
            raise TypeError("second argument for Template() must be a Substitutable()")

        self._argNames = paramNames
        self._rightHand = rightHand

    def _reprName(self):
        return "Template"

    def __str__(self):
        argNamesStr = ','.join(str(name) for name in self._argNames)
        return "({}) -> {}".format(argNamesStr, self._rightHand)

    def substitute(self, substDict):
        if len(substDict) < len(self._argNames):
            raise ValueError("Template.substitute() not given enough substitutions")
        for key in substDict:
            if key not in self._argNames:
                raise ValueError("Template.substitute() can only substitute arguments that were part of its definition")
        return self._rightHand.substitute(substDict)

    @property
    def argNames(self):
        return self._argNames


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

    def asSymbol(self, templatesDict):
        template = templatesDict[self._name]
        subs = self._substituteTemplate(template)
        return subs.asSymbol(templatesDict)

    def substitute(self, substDict):
        template = substDict[self._name]
        subs = self._substituteTemplate(template)
        return subs.substitute(substDict)

    def _substituteTemplate(self, template):
        namesAndValues = zip(template.argNames, self._params)
        substDict = {name: value for name, value in namesAndValues}
        return template.substitute(substDict)


if __name__ == "__main__":
    engine = Engine()
    print(repr(engine.substitute(VariableValue("a"))))