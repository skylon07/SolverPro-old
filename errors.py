from abc import ABC, abstractmethod

from constants import INDENT, USER_INPUT


# an abstract error class that gives an interface for tracing inputs
# TODO: change to TracebackException
class TracebackError(Exception, ABC):
    # subclasses must implement __init__ and call this
    @abstractmethod
    def __init__(self, message, badColStarts, badColEnds):
        message = ('\n' + INDENT).join(message.split('\n'))
        
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
    pass # just used as a type-group for all "handled errors"


class InterpreterNotImplementedError(NotImplementedError, InterpreterError):
    def __init__(self, featureAsPlural):
        # line below assumes InterpreterError has no __init__
        super().__init__(INDENT + "SolverPro cannot process {} (yet)".format(featureAsPlural))


class InterpreterTracebackError(TracebackError, InterpreterError, ABC):
    def __init__(self, badTraces):
        message = self._generateMessage(badTraces)
        # indent after first line
        message = ('\n' + INDENT).join(message.split('\n'))
        badStarts = [trace.start for trace in badTraces]
        badEnds = [trace.end for trace in badTraces]
        super().__init__(message, badStarts, badEnds)

    @abstractmethod
    def _generateMessage(self, badTraces):
        return # message string


# not meant to be raised; just inherits from TracebackError for its formatting capabilities
class InterpreterTracebackWarning(InterpreterTracebackError):
    def warn(self, outputFn):
        outputFn(self.message)


class UndefinedIdentifierError(InterpreterTracebackError):
    def _generateMessage(self, idTraces):
        plural = len(idTraces) > 1
        badIdentifierStrs = (str(trace.obj.construct()) for trace in idTraces)
        return "{}{}ndefined identifier{} {} given: {}".format(
            "An " if not plural else "",
            "u" if not plural else "U",
            "s" if plural else "",
            "were" if plural else "was",
            ", ".join(badIdentifierStrs),
        )


class InvalidExpressionError(InterpreterTracebackError):
    def _generateMessage(self, valTraces):
        plural = len(valTraces) > 1
        # TODO: define a function that makes the Represent mapping clear
        badIdentifierStrs = (trace["obj"].args[0].args[0] for trace in valTraces)
        return "The variable{} {} cannot be evaluated in an expression".format(
            "s" if plural else "",
            ", ".join(badIdentifierStrs)
        )


class NotATemplateError(InterpreterTracebackError):
    def _generateMessage(self, callTraces):
        plural = len(callTraces) > 1
        badTemplateNames = (str(trace.obj.construct().templateId) for trace in callTraces)
        return "{} {} not defined as {}template{} and cannot be evaluated as {}".format(
            ','.join(badTemplateNames),
            "are" if plural else "is",
            "a " if not plural else "",
            "s" if plural else "",
            "such" if plural else "one",
        )


class TemplateMismatchError(InterpreterTracebackError):
    def _generateMessage(self, badTraces):
        raise NotImplementedError("TemplateMismatchError()")


class UnusedArgumentsWarning(InterpreterTracebackWarning):
    def _generateMessage(self, idTraces):
        plural = len(idTraces) > 1
        badIdentifierStrs = (str(trace.obj) for trace in idTraces)
        return "Variable{} {} {} not used in {} template definition".format(
            "s" if plural else "",
            ", ".join(badIdentifierStrs),
            "were" if plural else "was",
            "their" if plural else "its",
        )


class TemplateEvaluationError(InterpreterTracebackError):
    pass # TODO


# TODO: modify docs/examples.txt after this has been implememted
#       ("will be added" -> "was added")
class SingleVariableRelationWarning(InterpreterTracebackWarning):
    pass # TODO
