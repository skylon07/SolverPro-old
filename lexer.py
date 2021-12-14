from abc import ABC, abstractmethod, abstractproperty
import string

from immutable import immutable

class Lexer():
    class types(dict):
        def __init__(self):
            self.update({
                "FORGET": "FORGET",
                "LIST": "LIST",
                "RESET": "RESET",
                "EVAL": "EVAL",
                "SAVE": "SAVE",
                "COLON_EQUALS": "COLON_EQUALS",
                "IDENTIFIER": "IDENTIFIER",
                "NUMBER": "NUMBER",
                "E_NUMBER": "E_NUMBER",
                "PERIOD": "PERIOD",
                "COMMA": "COMMA",
                "COMMENT": "COMMENT",
                "BRACE_OPEN": "BRACE_OPEN",
                "BRACE_CLOSE": "BRACE_CLOSE",
                "BRACKET_OPEN": "BRACKET_OPEN",
                "BRACKET_CLOSE": "BRACKET_CLOSE",
                "PAREN_OPEN": "PAREN_OPEN",
                "PAREN_CLOSE": "PAREN_CLOSE",
                "CARROT_LEFT": "CARROT_LEFT",
                "CARROT_RIGHT": "CARROT_RIGHT",
                "EQUALS": "EQUALS",
                "PLUS": "PLUS",
                "DASH": "DASH",
                "STAR": "STAR",
                "SLASH": "SLASH",
                "CARROT": "CARROT",
                "EOL": "EOL",
                "INVALID": "INVALID",
            })

        def __str__(self):
            try:
                return self._str
            except KeyError:
                self._str = "lexer-types" + str({key for key in self.keys()})
                return self._str

        def __getattr__(self, attr):
            return self[attr]

    types = types()

    @immutable
    class Token:
        def __init__(self, repStr, tokenType, placementIdx):
            self._repStr = repStr
            self._type = tokenType
            self._placementStart = placementIdx
            self._checkValidType()
        
        def __str__(self):
            return self._repStr

        def __repr__(self):
            return 'Token<{}>("{}")'.format(self._type, self._repStr)
        
        def __eq__(self, other):
            if not isinstance(other, Lexer.Token):
                return False
            return self._repStr == other._repStr and self._type == other._type

        def __ne__(self, other):
            if not isinstance(other, Lexer.Token):
                return True
            return self._repStr != other._repStr or self._type != other._type

        @property
        def type(self):
            return self._type

        @property
        def placementStart(self):
            return self._placementStart

        @property
        def placementCenter(self):
            return self._placementStart + int(len(self._repStr) / 2)

        @property
        def placementEnd(self):
            return self._placementStart + len(self._repStr)

        def _checkValidType(self):
            try:
                Lexer.types[self._type]
            except KeyError:
                raise ValueError("Token created with invalid type '{}'".format(self._type))

    def process(self, lexString, withEOL=True):
        return tuple(self.processGen(lexString, withEOL))
    
    def processGen(self, lexString, withEOL=True):
        machines = [
            forgetMachine,
            listMachine,
            resetMachine,
            evalMachine,
            saveMachine,
            colonEqualsMachine,
            numberMachine,
            eNumberMachine,
            periodMachine,
            commaMachine,
            commentMachine,
            braceOpenMachine,
            braceCloseMachine,
            bracketOpenMachine,
            bracketCloseMachine,
            parenOpenMachine,
            parenCloseMachine,
            carrotLeftMachine,
            carrotRightMachine,
            equalsMachine,
            plusMachine,
            dashMachine,
            starMachine,
            slashMachine,
            carrotMachine,
            eolMachine,
            # INTENTIONALLY at the bottom to give lowest-priority
            identifierMachine,
        ]

        placeIdx = 0
        while len(lexString) > 0:
            lenBefore = len(lexString)
            lexString = self.stripWhitespace(lexString)
            lenAfter = len(lexString)
            placeIdx += lenBefore - lenAfter

            if lexString == '':
                break
            
            maxResult = LexerMachine.MachineResult(0, Lexer.types.INVALID)
            for machine in machines:
                result = machine.match(lexString)
                if result.numMatched > maxResult.numMatched:
                    maxResult = result
            
            tokenType = maxResult.tokenType
            if maxResult.numMatched == 0:
                tokenStr = lexString[0:1]
            else:
                tokenStr = lexString[:maxResult.numMatched]
            
            yield Lexer.Token(tokenStr, tokenType, placeIdx)
            lexString = lexString[len(tokenStr):]
            placeIdx += len(tokenStr)
        if withEOL:
            yield Lexer.Token("", Lexer.types.EOL, placeIdx)

    def stripWhitespace(self, lexString):
        idx = 0
        # newline intentionally left out
        whitespaces = "\t "
        while idx < len(lexString) and lexString[idx] in whitespaces:
            idx += 1
        return lexString[idx:]

    # a highly inefficient function that exists
    # only for the sake of testing the Lexer class
    def findType_forTesting(self, lexString):
        return self.process(lexString)[0].type


class LexerMachine(ABC):
    def __init__(self):
        self.resetState()

    def match(self, inp):
        result = self.state1(inp)
        if type(result) is not bool:
            raise TypeError("LexerMachine.state1() is required to return a boolean")
        if not result:
            self.stateErr()
        
        if type(self.numMatched) is not int:
            raise TypeError("Token machine returned non-int for numMatched")
        numMatched = self.numMatched
        self.resetState()

        tokenType = self.type
        return LexerMachine.MachineResult(numMatched, tokenType)
    
    class MachineResult():
        def __init__(self, numMatched, tokenType):
            self._numMatched = numMatched
            self._tokenType = tokenType

        @property
        def numMatched(self):
            return self._numMatched

        @property
        def tokenType(self):
            return self._tokenType

    @abstractproperty
    def type(self):
        return Lexer.types.INVALID

    @abstractmethod
    # jumping-off point for state chain
    def state1(self, inp):
        return

    # called when a required character is not present
    def stateErr(self):
        self.resetState()

    # called after every state-machine evaluation
    def resetState(self):
        self.numMatched = 0

    # decorator for template of a state function
    @staticmethod
    def stateFunction(fn):
        def templateWrapper(self, inp):
            if self.numMatched < len(inp):
                char = inp[self.numMatched]
                result = fn(self, inp, char)
                if result is None:
                    result = True
                if type(result) is not bool:
                    raise TypeError("LexerMachine stateFunctions must return booleans (indicating success or failure)")
                return result
            return True
        return templateWrapper

    # decorator for template of a state function (with required constraints)
    @staticmethod
    def stateFunctionRequired(fn):
        def templateWrapper(self, inp):
            if self.numMatched < len(inp):
                char = inp[self.numMatched]
                result = fn(self, inp, char)
                if result is None:
                    result = False
                if type(result) is not bool:
                    raise TypeError("LexerMachine stateFunctions must return booleans (indicating success or failure)")
                return result
            return False
        return templateWrapper


# basic matcher for any machine who wants to use it
class FixedTokenMachine(LexerMachine):
    # required chars must match token
    # (no decorator; not a standard-flow state function)
    def state1(self, inp):
        if inp[:len(self.token)] == self.token:
            self.numMatched += len(self.token)
            return self.state2(inp)
        return False

    # check for invalid follow-up characters
    @LexerMachine.stateFunction
    def state2(self, inp, char):
        if char in self.invalidTerminations:
            return False

    @abstractproperty
    def token(self):
        return ""

    @abstractproperty
    def invalidTerminations(self):
        return []


# abstract matcher for all !commands
class CommandMachine(FixedTokenMachine):
    @property
    def invalidTerminations(self):
        return string.ascii_letters


# matches !forget
class ForgetMachine(CommandMachine):
    @property
    def type(self):
        return Lexer.types.FORGET

    @property
    def token(self):
        return "!forget"

forgetMachine = ForgetMachine()


class ListMachine(CommandMachine):
    @property
    def type(self):
        return Lexer.types.LIST

    @property
    def token(self):
        return "!list"
    
listMachine = ListMachine()


class ResetMachine(CommandMachine):
    @property
    def type(self):
        return Lexer.types.RESET
    
    @property
    def token(self):
        return "!reset"
    
resetMachine = ResetMachine()


class EvalMachine(CommandMachine):
    @property
    def type(self):
        return Lexer.types.EVAL

    @property
    def token(self):
        return "!eval"

evalMachine = EvalMachine()


class SaveMachine(CommandMachine):
    @property
    def type(self):
        return Lexer.types.SAVE

    @property
    def token(self):
        return "!save"

saveMachine = SaveMachine()


class ColonEqualsMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.COLON_EQUALS

    @property
    def token(self):
        return ":="

    @property
    def invalidTerminations(self):
        return ""

colonEqualsMachine = ColonEqualsMachine()


class IdentifierMachine(LexerMachine):
    @property
    def type(self):
        return Lexer.types.IDENTIFIER
    
    # check for required first char being a letter or underscore
    @LexerMachine.stateFunctionRequired
    def state1(self, inp, char):
        if char in string.ascii_letters + '_':
            self.numMatched += 1
            return self.state2(inp)

    # check remaining for letters, underscores, and digits
    @LexerMachine.stateFunction
    def state2(self, inp, char):
        if char in string.ascii_letters + string.digits + '_':
            self.numMatched += 1
            return self.state2(inp)

identifierMachine = IdentifierMachine()


class NumberMachine(LexerMachine):
    @property
    def type(self):
        return Lexer.types.NUMBER
        
    # required digit or period
    @LexerMachine.stateFunctionRequired
    def state1(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state2(inp)
        elif char == '.':
            self.numMatched += 1
            return self.state3(inp)

    # extra digits before optional period
    @LexerMachine.stateFunction
    def state2(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state2(inp)
        elif char == '.':
            self.numMatched += 1
            return self.state3(inp)

    # required digit after a period
    @LexerMachine.stateFunctionRequired
    def state3(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state4(inp)

    # optional digits after period
    @LexerMachine.stateFunction
    def state4(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state4(inp)

numberMachine = NumberMachine()


class ENumberMachine(LexerMachine):
    @property
    def type(self):
        return Lexer.types.E_NUMBER

    # required digit or period
    @LexerMachine.stateFunctionRequired
    def state1(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state2(inp)
        elif char == '.':
            self.numMatched += 1
            return self.state3(inp)

    # required extra digits before required E or optional period
    @LexerMachine.stateFunctionRequired
    def state2(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state2(inp)
        elif char == '.':
            self.numMatched += 1
            return self.state3(inp)
        elif char in 'Ee':
            self.numMatched += 1
            return self.state5(inp)

    # required digit after a period
    @LexerMachine.stateFunctionRequired
    def state3(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state4(inp)

    # required digits after period before required E
    @LexerMachine.stateFunctionRequired
    def state4(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state4(inp)
        elif char in 'Ee':
            self.numMatched += 1
            return self.state5(inp)

    # optional plus/dash after E
    @LexerMachine.stateFunction
    def state5(self, inp, char):
        if char in '+-':
            self.numMatched += 1
        return self.state6(inp)

    # required digit after E (and possible dash)
    @LexerMachine.stateFunctionRequired
    def state6(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state7(inp)

    # optional digits after E
    @LexerMachine.stateFunction
    def state7(self, inp, char):
        if char in string.digits:
            self.numMatched += 1
            return self.state7(inp)

eNumberMachine = ENumberMachine()


class PeriodMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.PERIOD

    @property
    def token(self):
        return '.'

    @property
    def invalidTerminations(self):
        return ''

periodMachine = PeriodMachine()


class CommaMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.COMMA

    @property
    def token(self):
        return ','

    @property
    def invalidTerminations(self):
        return ''

commaMachine = CommaMachine()


class CommentMachine(LexerMachine):
    @property
    def type(self):
        return Lexer.types.COMMENT

    # required pound character
    @LexerMachine.stateFunctionRequired
    def state1(self, inp, char):
        if char == '#':
            self.numMatched += 1
            return self.state2(inp)
    
    # optional non-newline chars
    @LexerMachine.stateFunction
    def state2(self, inp, char):
        if char != '\n':
            self.numMatched += 1
            return self.state2(inp)

commentMachine = CommentMachine()


class BraceOpenMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.BRACE_OPEN

    @property
    def token(self):
        return '{'
    
    @property
    def invalidTerminations(self):
        return ''

braceOpenMachine = BraceOpenMachine()


class BraceCloseMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.BRACE_CLOSE

    @property
    def token(self):
        return '}'
    
    @property
    def invalidTerminations(self):
        return ''

braceCloseMachine = BraceCloseMachine()


class BracketOpenMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.BRACKET_OPEN

    @property
    def token(self):
        return '['
    
    @property
    def invalidTerminations(self):
        return ''

bracketOpenMachine = BracketOpenMachine()


class BracketCloseMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.BRACKET_CLOSE

    @property
    def token(self):
        return ']'
    
    @property
    def invalidTerminations(self):
        return ''

bracketCloseMachine = BracketCloseMachine()


class ParenOpenMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.PAREN_OPEN

    @property
    def token(self):
        return '('
    
    @property
    def invalidTerminations(self):
        return ''

parenOpenMachine = ParenOpenMachine()


class ParenCloseMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.PAREN_CLOSE

    @property
    def token(self):
        return ')'
    
    @property
    def invalidTerminations(self):
        return ''

parenCloseMachine = ParenCloseMachine()


class CarrotLeftMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.CARROT_LEFT

    @property
    def token(self):
        return '<'
    
    @property
    def invalidTerminations(self):
        return ''

carrotLeftMachine = CarrotLeftMachine()


class CarrotRightMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.CARROT_RIGHT

    @property
    def token(self):
        return '>'
    
    @property
    def invalidTerminations(self):
        return ''

carrotRightMachine = CarrotRightMachine()


class EqualsMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.EQUALS

    @property
    def token(self):
        return '='
    
    @property
    def invalidTerminations(self):
        return ''

equalsMachine = EqualsMachine()


class PlusMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.PLUS

    @property
    def token(self):
        return '+'
    
    @property
    def invalidTerminations(self):
        return ''

plusMachine = PlusMachine()


class DashMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.DASH

    @property
    def token(self):
        return '-'
    
    @property
    def invalidTerminations(self):
        return ''

dashMachine = DashMachine()


class StarMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.STAR

    @property
    def token(self):
        return '*'
    
    @property
    def invalidTerminations(self):
        return ''

starMachine = StarMachine()


class SlashMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.SLASH

    @property
    def token(self):
        return '/'
    
    @property
    def invalidTerminations(self):
        return ''

slashMachine = SlashMachine()


class CarrotMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.CARROT

    @property
    def token(self):
        return '^'
    
    @property
    def invalidTerminations(self):
        return ''

carrotMachine = CarrotMachine()


class EOLMachine(FixedTokenMachine):
    @property
    def type(self):
        return Lexer.types.EOL

    @property
    def token(self):
        return '\n'
    
    @property
    def invalidTerminations(self):
        return ''

eolMachine = EOLMachine()
