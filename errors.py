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
