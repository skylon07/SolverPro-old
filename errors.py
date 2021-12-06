from constants import INDENT, USER_INPUT


# an abstract error class that gives an interface for tracing inputs
class TracebackError(Exception):
    def __init__(self, message, badLine, badColStart, badColEnd):
        self._mainMsg = "{}: {}".format(type(self).__name__, message) 
        self._badLine = badLine
        self._badColStart = badColStart
        self._badColEnd = badColEnd
        self._generateMessage()

        super().__init__(self._message)

    @property
    def message(self):
        return self._message

    def _generateMessage(self):
        offset = len(USER_INPUT)
        length = self._badColEnd - self._badColStart
        linePtr = ' ' * (offset + self._badColStart) + '^' * length
        indentedMsg = INDENT + self._mainMsg
        self._message = '\n'.join((linePtr, indentedMsg)) 
