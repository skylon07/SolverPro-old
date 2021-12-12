from abc import ABC, abstractmethod

from constants import INDENT, USER_INPUT


# an abstract error class that gives an interface for tracing inputs
class TracebackError(Exception):
    def __init__(self, message, badColStarts, badColEnds):
        if type(badColStarts) is int:
            badColStarts = [badColStarts]
        if type(badColEnds) is int:
            badColEnds = [badColEnds]
        
        self.__mainMsg = "{}: {}".format(type(self).__name__, message) 
        self.__badColStarts = sorted(badColStarts)
        self.__badColEnds = sorted(badColEnds)
        self.__generateMessage()

        super().__init__(self.__message)

    @property
    def message(self):
        return self.__message

    def __generateMessage(self):
        linePtrs = ''
        for badStart, badEnd in zip(self.__badColStarts, self.__badColEnds):
            offset = len(linePtrs)
            length = badEnd - badStart
            linePtrs += ' ' * (badStart - offset) + '^' * length
        linePtrs = ' ' * len(USER_INPUT) + linePtrs
        indentedMsg = INDENT + self.__mainMsg
        self.__message = '\n'.join((linePtrs, indentedMsg)) 

class InterpreterError(Exception):
    pass # just used as a type-group


class InterpreterNotImplementedError(InterpreterError):
    pass


class InterpreterTracebackError(TracebackError, InterpreterError, ABC):
    def __init__(self, badTraces):
        message = self._generateMessage(badTraces)
        badStarts = map(lambda trace: trace["start"], badTraces)
        badEnds = map(lambda trace: trace["end"], badTraces)
        super().__init__(message, badStarts, badEnds)

    @abstractmethod
    def _generateMessage(self, badTraces):
        return # message string


# not meant to be raised; just inherits from TracebackError for its formatting capabilities
class InterpreterTracebackWarning(InterpreterTracebackError):
    def warn(self, outputFn):
        outputFn(self.message)


class UndefinedIdentifierError(InterpreterTracebackError):
    def _generateMessage(self, badTraces):
        plural = len(badTraces) > 1
        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "{}{}ndefined identifier{} {} given: {}".format(
            "An " if not plural else "",
            "u" if not plural else "U",
            "s" if plural else "",
            "were" if plural else "was",
            ','.join(badIdentifierStrs),
        )


class UnusedArgumentsWarning(InterpreterTracebackWarning):
    def _generateMessage(self, badTraces):
        plural = len(badTraces) > 1
        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "Variable{} {} {} not used in {} template definition".format(
            "s" if plural else "",
            ','.join(badIdentifierStrs),
            "were" if plural else "was",
            "their" if plural else "its",
        )


class InvalidExpressionError(InterpreterTracebackError):
    def _generateMessage(self, badTraces):
        plural = len(badTraces) > 1
        badIdentifierStrs = map(lambda trace: str(trace["obj"]), badTraces)
        return "The variable{} {} cannot be evaluated in an expression".format(
            "s" if plural else "",
            ','.join(badIdentifierStrs)
        )


class BadTemplateEvaluationError(InterpreterTracebackError):
    def _generateMessage(self, badTraces):
        templateCall = badTraces[0]["obj"]
        templateName = str(templateCall.nameId)
        return "Template {} does not evaluate to an expression; it cannot be used in an expression".format(
            templateName,
        )