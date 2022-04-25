from lexer import Lexer
from errors import TracebackError

# see docs/dev-notes/parsing.puml for a list of productions
class Parser:
    def __init__(self):
        self.onFunctions = dict()

    class ParseError(TracebackError):
        def __init__(self, expectedTypes, unexpectedToken):
            try:
                iter(expectedTypes)
            except TypeError as e:
                if "is not iterable" in str(e):
                    raise TypeError("Attempted to create ParseError with invalid list of expected types")
                raise e
            if type(expectedTypes) is str or (len(expectedTypes) > 0 and type(expectedTypes[0]) is not str):
                    raise TypeError("Attempted to create ParseError with invalid list of expected types")
            self.expectedTypes = tuple(expectedTypes)
            
            if type(unexpectedToken) is not Lexer.Token:
                raise TypeError("ParseError was given an invalid token")
            self.unexpectedToken = unexpectedToken

            expectedTypesStr = " or ".join(self.expectedTypes)
            fullMessage = "Unexpected token {}; expected {}".format(
                self.unexpectedToken,
                expectedTypesStr
            )
            start = self.unexpectedToken.placementStart
            end = self.unexpectedToken.placementEnd
            super().__init__(fullMessage, start, end)

    class EOLError(TracebackError):
        def __init__(self, eolToken):
            if type(eolToken) is not Lexer.Token or eolToken.type != Lexer.types.EOL:
                raise TypeError("EOLError was given a bad EOL-token argument")
            self.unexpectedToken = eolToken

            fullMessage = "Unexpected end of line"
            start = self.unexpectedToken.placementStart
            # +1 needed since EOLs take up no space
            end = start + 1
            super().__init__(fullMessage, start, end)
    
    def inspect(self, tokens, origLineStr):
        if len(tokens) > 0 and type(tokens[0]) is not Lexer.Token:
            raise TypeError("Parser.inspect() requires a list of tokens")
        while len(tokens) > 0:
            matcher = ParserMatcher(self.onFunctions, tokens, origLineStr)
            matcher.start()
            tokens = tokens[matcher.numParsed:]

    def onStart(self, fn):
        self._addOnFunction("onStart", fn)

    def onRelation(self, fn):
        self._addOnFunction("onRelation", fn)

    def onExpression(self, fn):
        self._addOnFunction("onExpression", fn)

    def onExpressions(self, fn):
        self._addOnFunction("onExpressions", fn)

    def onEvaluation(self, fn):
        self._addOnFunction("onEvaluation", fn)

    def onValue(self, fn):
        self._addOnFunction("onValue", fn)

    def onFullIdentifier(self, fn):
        self._addOnFunction("onFullIdentifier", fn)

    def onIdentifier(self, fn):
        self._addOnFunction("onIdentifier", fn)

    def onIdentifiers(self, fn):
        self._addOnFunction("onIdentifiers", fn)

    def onNumber(self, fn):
        self._addOnFunction("onNumber", fn)

    def onUnit(self, fn):
        self._addOnFunction("onUnit", fn)

    def onOperationLow(self, fn):
        self._addOnFunction("onOperationLow", fn)

    def onOperationMid(self, fn):
        self._addOnFunction("onOperationMid", fn)

    def onOperationHigh(self, fn):
        self._addOnFunction("onOperationHigh", fn)

    def onOperationMax(self, fn):
        self._addOnFunction("onOperationMax", fn)

    def onOperatorLow(self, fn):
        self._addOnFunction("onOperatorLow", fn)

    def onOperatorMid(self, fn):
        self._addOnFunction("onOperatorMid", fn)

    def onOperatorHigh(self, fn):
        self._addOnFunction("onOperatorHigh", fn)

    def onAlias(self, fn):
        self._addOnFunction("onAlias", fn)

    def onLeftAlias(self, fn):
        self._addOnFunction("onLeftAlias", fn)

    def onRightAlias(self, fn):
        self._addOnFunction("onRightAlias", fn)

    def onLeftAliasTemp(self, fn):
        self._addOnFunction("onLeftAliasTemp", fn)

    def onRightAliasTemp(self, fn):
        self._addOnFunction("onRightAliasTemp", fn)

    def onInherits(self, fn):
        self._addOnFunction("onInherits", fn)

    def onObjectDeclaration(self, fn):
        self._addOnFunction("onObjectDeclaration", fn)

    def onObjectParameters(self, fn):
        self._addOnFunction("onObjectParameters", fn)

    def onCommand(self, fn):
        self._addOnFunction("onCommand", fn)

    def _addOnFunction(self, dictKey, fn):
        if not callable(fn):
            raise TypeError("Parser.{}() given non-callable callback".format(dictKey))
        
        if dictKey not in self.onFunctions:
            self.onFunctions[dictKey] = []
        self.onFunctions[dictKey].append(fn)


# decorator that ensures productions return their parsed token lists
# (meant to be used in ParserMatcher class ONLY!)
def production(productionFn):
    onFnNames = {
        "start": "onStart",
        "relation": "onRelation",
        "expression": "onExpression",
        "expressions": "onExpressions",
        "evaluation": "onEvaluation",
        "value": "onValue",
        "fullidentifier": "onFullIdentifier",
        "identifier": "onIdentifier",
        "identifiers": "onIdentifiers",
        "number": "onNumber",
        "unit": "onUnit",
        "operationlow": "onOperationLow",
        "operationmid": "onOperationMid",
        "operationhigh": "onOperationHigh",
        "operationmax": "onOperationMax",
        "operatorlow": "onOperatorLow",
        "operatormid": "onOperatorMid",
        "operatorhigh": "onOperatorHigh",
        "alias": "onAlias",
        "leftalias": "onLeftAlias",
        "rightalias": "onRightAlias",
        "leftaliastemp": "onLeftAliasTemp",
        "rightaliastemp": "onRightAliasTemp",
        "inherits": "onInherits",
        "objectdeclaration": "onObjectDeclaration",
        "objectparameters": "onObjectParameters",
        "command": "onCommand",
    }
    def wrapperProduction(self):
        start = self.numParsed
        branch = productionFn(self)
        if type(branch) is not str:
            raise TypeError("Parser production did not return branch type")
        end = self.numParsed

        prodName = productionFn.__name__
        onFnName = onFnNames[prodName]
        self._runOnFunctions(onFnName, start, end, branch)
    return wrapperProduction


class ParserMatcher:
    def __init__(self, onFunctions, tokens, origLineStr):
        self.numParsed = 0
        self.tokens = tokens
        self.onFunctions = onFunctions
        self.origLineStr = origLineStr

    # index incrementer and syntax validator
    def match(self, tokenType):
        if tokenType not in Lexer.types:
            raise ValueError("Parser tried to match tokens against an invalid type")

        if self.currToken.type != tokenType:
            if self.currToken.type == Lexer.types.EOL:
                self._throwEOLError()
            self._throwParseError((tokenType,))
        self.numParsed += 1

    # function that returns the question "are there any more tokens?"
    def moreTokens(self):
        return self.numParsed < len(self.tokens)

    # returns the next unparsed token
    @property
    def currToken(self):
        if self.moreTokens():
            currToken = self.tokens[self.numParsed]
            if type(currToken) is not Lexer.Token:
                raise TypeError("Parser tried to parse non-token in tokens list")
            return currToken
        # a token was expected, yet there aren't any more!
        self._throwEOLError()
    
    # productions
    @production
    def start(self):
        # branch EOL
        if self.currToken.type == Lexer.types.EOL:
            self.match(Lexer.types.EOL)
            return "EOL"

        # branch command EOL
        commandFirsts = [
            Lexer.types.FORGET,
            Lexer.types.LIST,
            Lexer.types.RESET,
            Lexer.types.EVAL,
            Lexer.types.SAVE,
        ]
        if self.currToken.type in commandFirsts:
            self.command()
            self.match(Lexer.types.EOL)
            return "command EOL"

        # distinguish relations, aliases, and expressions
        idx = self.numParsed
        isRelation = False
        isAlias = False
        while idx < len(self.tokens):
            token = self.tokens[idx]
            if token.type == Lexer.types.EQUALS:
                isRelation = True
                break
            if token.type == Lexer.types.COLON_EQUALS:
                isAlias = True
                break
            idx += 1

        # branch relation EOL
        if isRelation:
            self.relation()
            self.match(Lexer.types.EOL)
            return "relation EOL"
        # branch alias EOL
        elif isAlias:
            self.alias()
            self.match(Lexer.types.EOL)
            return "alias EOL"
        
        # default branch expression EOL
        self.expression()
        self.match(Lexer.types.EOL)
        return "expression EOL"
            
    @production
    def relation(self):
        self.expression()
        self.match(Lexer.types.EQUALS)
        self.expression()
        return "expression EQUALS expression"

    @production
    def expression(self):
        self.operationlow()
        return "operationlow"

    @production
    def expressions(self):
        # all branches
        self.expression()

        # branch expression COMMA expressions
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type == Lexer.types.COMMA:
            self.match(Lexer.types.COMMA)
            self.expressions()
            return "expression COMMA expressions"
        
        # (end of default branch expression)
        return "expression"

    @production
    def evaluation(self):
        # branch PAREN_OPEN expression PAREN_CLOSE
        if self.currToken.type == Lexer.types.PAREN_OPEN:
            self.match(Lexer.types.PAREN_OPEN)
            self.expression()
            self.match(Lexer.types.PAREN_CLOSE)
            return "PAREN_OPEN expression PAREN_CLOSE"

        # branch BRACKET_OPEN expression BRACKET_CLOSE
        if self.currToken.type == Lexer.types.BRACKET_OPEN:
            self.match(Lexer.types.BRACKET_OPEN)
            self.expression()
            self.match(Lexer.types.BRACKET_CLOSE)
            return "BRACKET_OPEN expression BRACKET_CLOSE"
            
        # default branch value
        self.value()
        return "value"

    @production
    def value(self):
        # branches number/number unit
        numberFirsts = [
            Lexer.types.NUMBER,
            Lexer.types.E_NUMBER,
        ]
        if self.currToken.type in numberFirsts:
            # both branches
            self.number()

            # branch number unit
            unitFirsts = [
                Lexer.types.CARROT_LEFT,
            ]
            moreTokens = self.moreTokens() # (no more tokens is a valid branch!)
            if moreTokens and self.currToken.type in unitFirsts:
                self.unit()
                return "number unit"

            # (end of branch number)
            return "number"
        
        # branches fullidentifier/fullidentifier unit/fullidentifier PAREN_OPEN PAREN_CLOSE/
        #     fullidentifier PAREN_OPEN expressions PAREN_CLOSE
        self.fullidentifier()

        # branch fullidentifier unit
        unitFirsts = [
            Lexer.types.CARROT_LEFT,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in unitFirsts:
            self.unit()
            return "fullidentifier unit"

        # TODO: PARSER NO LONGER SUPPORTS TEMPLATE CALLS
        # # branch fullidentifier PAREN_OPEN PAREN_CLOSE/
        # #     fullidentifier PAREN_OPEN expressions PAREN_CLOSE
        # # (would make more sense for these branches to be in
        # # evaluable, but it is easier to implement here)
        # elif moreTokens and self.currToken.type == Lexer.types.PAREN_OPEN:
        #     self.match(Lexer.types.PAREN_OPEN)
        #     # branch fullidentifier PAREN_OPEN PAREN_CLOSE
        #     if self.currToken.type == Lexer.types.PAREN_CLOSE:
        #         self.match(Lexer.types.PAREN_CLOSE)
        #         return "fullidentifier PAREN_OPEN PAREN_CLOSE"
        #     # branch fullidentifier PAREN_OPEN expressions PAREN_CLOSE
        #     self.expressions()
        #     self.match(Lexer.types.PAREN_CLOSE)
        #     return "fullidentifier PAREN_OPEN expressions PAREN_CLOSE"

        # (end of default branch fullidentifier)
        return "fullidentifier"

    @production
    def fullidentifier(self):
        self.identifier()
        return "identifier"

    @production
    def identifier(self):
        # all branches
        self.match(Lexer.types.IDENTIFIER)

        # branch IDENTIFIER PERIOD identifier
        propertyFirsts = [
            Lexer.types.PERIOD,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in propertyFirsts:
            self.match(Lexer.types.PERIOD)
            self.identifier()
            return "IDENTIFIER PERIOD identifier"

        # (end of default branch IDENTIFIER)
        return "IDENTIFIER"

    @production
    def identifiers(self):
        # all branches
        self.fullidentifier()

        # branch fullidentifier COMMA identifiers
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type == Lexer.types.COMMA:
            self.match(Lexer.types.COMMA)
            self.identifiers()
            return "fullidentifier COMMA identifiers"
        
        # (end of default branch fullidentifier)
        return "fullidentifier"

    @production
    def number(self):
        # branch ENUMBER
        if self.currToken.type == Lexer.types.E_NUMBER:
            self.match(Lexer.types.E_NUMBER)
            return "ENUMBER"

        # branch NUMBER
        if self.currToken.type == Lexer.types.NUMBER:
            self.match(Lexer.types.NUMBER)
            return "NUMBER"

        self._throwParseError((
            "a number",
            "an E-number (1.4E5)"
        ))

    @production
    def unit(self):
        self.match(Lexer.types.CARROT_LEFT)
        self.expression()
        self.match(Lexer.types.CARROT_RIGHT)
        return "CARROT_LEFT expression CARROT_RIGHT"
    
    @production
    def operationlow(self):
        # all branches
        self.operationmid()

        # branch operationmid operatorlow operationlow
        operatorFirsts = [
            Lexer.types.PLUS,
            Lexer.types.DASH,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in operatorFirsts:
            self.operatorlow()
            self.operationlow()
            return "operationmid operatorlow operationlow"

        # (end of default branch operationmid)
        return "operationmid"

    @production
    def operationmid(self):
        # all branches
        self.operationhigh()

        # branch operationhigh operatormid operationmid
        operatorFirsts = [
            Lexer.types.STAR,
            Lexer.types.SLASH,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in operatorFirsts:
            self.operatormid()
            self.operationmid()
            return "operationhigh operatormid operationmid"

        # (end of default branch operationhigh)
        return "operationhigh"
    
    @production
    def operationhigh(self):
        # all branches
        self.operationmax()

        # branch operationmax operatorhigh operationhigh
        operatorFirsts = [
            Lexer.types.CARROT,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in operatorFirsts:
            self.operatorhigh()
            self.operationhigh()
            return "operationmax operatorhigh operationhigh"

        # (end of default branch operationmax)
        return "operationmax"

    @production
    def operationmax(self):
        # branch DASH operationmax
        negativeFirsts = [
            Lexer.types.DASH,
        ]
        if self.currToken.type in negativeFirsts:
            self.match(Lexer.types.DASH)
            self.operationmax()
            return "DASH operationmax"

        # default branch evaluation
        self.evaluation()
        return "evaluation"

    @production
    def operatorlow(self):
        # branch DASH
        if self.currToken.type == Lexer.types.DASH:
            self.match(Lexer.types.DASH)
            return "DASH"
        
        # branch PLUS
        if self.currToken.type == Lexer.types.PLUS:
            self.match(Lexer.types.PLUS)
            return "PLUS"

        self._throwParseError((
            "an operator + - * / ^",
        ))

    @production
    def operatormid(self):
        # branch SLASH
        if self.currToken.type == Lexer.types.SLASH:
            self.match(Lexer.types.SLASH)
            return "SLASH"

        # branch STAR
        if self.currToken.type == Lexer.types.STAR:
            self.match(Lexer.types.STAR)
            return "STAR"

        self._throwParseError((
            "an operator + - * / ^",
        ))

    @production
    def operatorhigh(self):
        if self.currToken.type == Lexer.types.CARROT:
            self.match(Lexer.types.CARROT)
            return "CARROT"

        self._throwParseError((
            "an operator + - * / ^",
        ))

    @production
    def alias(self):
        # distinguish leftalias from leftaliastemp
        isTemplate = False
        idx = self.numParsed
        while idx < len(self.tokens):
            token = self.tokens[idx]
            if token.type == Lexer.types.PAREN_OPEN:
                isTemplate = True
                break
            if token.type == Lexer.types.COLON_EQUALS:
                break
            idx += 1
        
        # branch leftaliastemp COLON_EQUALS rightaliastemp
        if isTemplate:
            self.leftaliastemp()
            self.match(Lexer.types.COLON_EQUALS)
            self.rightaliastemp()
            return "leftaliastemp COLON_EQUALS rightaliastemp"

        # default branch leftalias COLON_EQUALS rightalias
        self.leftalias()
        self.match(Lexer.types.COLON_EQUALS)
        self.rightalias()
        return "leftalias COLON_EQUALS rightalias"

    @production
    def leftalias(self):
        # branch BRACKET_OPEN identifiers BRACKET_CLOSE
        if self.currToken.type == Lexer.types.BRACKET_OPEN:
            self.match(Lexer.types.BRACKET_OPEN)
            self.identifiers()
            self.match(Lexer.types.BRACKET_CLOSE)
            return "BRACKET_OPEN identifiers BRACKET_CLOSE"

        # default branch fullidentifier
        self.fullidentifier()
        return "fullidentifier"

    @production
    def rightalias(self):
        # branch inherits objectdeclaration
        if self.currToken.type == Lexer.types.PAREN_OPEN:
            self.inherits()
            self.objectdeclaration()
            return "inherits objectdeclaration"

        # branch objectdeclaration
        if self.currToken.type == Lexer.types.BRACE_OPEN:
            self.objectdeclaration()
            return "objectdeclaration"

        # branch BRACKET_OPEN expressions BACKET_CLOSE
        if self.currToken.type == Lexer.types.BRACKET_OPEN:
            self.match(Lexer.types.BRACKET_OPEN)
            self.expressions()
            self.match(Lexer.types.BRACKET_CLOSE)
            return "BRACKET_OPEN expressions BRACKET_CLOSE"

        # default branch expression
        self.expression()
        return "expression"

    @production
    def leftaliastemp(self):
        # all branches
        self.fullidentifier()
        self.match(Lexer.types.PAREN_OPEN)

        # branch fullidentifier PAREN_OPEN PAREN_CLOSE
        if self.currToken.type == Lexer.types.PAREN_CLOSE:
            self.match(Lexer.types.PAREN_CLOSE)
            return "fullidentifier PAREN_OPEN PAREN_CLOSE"

        # branch fullidentifier PAREN_OPEN identifiers PAREN_CLOSE
        if self.currToken.type == Lexer.types.IDENTIFIER:
            self.identifiers()
            self.match(Lexer.types.PAREN_CLOSE)
            return "fullidentifier PAREN_OPEN identifiers PAREN_CLOSE"

        self._throwParseError((
            "a list of identifiers",
            "a closing parenthesis ')'",
        ))

    @production
    def rightaliastemp(self):
        # branch command
        commandFirsts = [
            Lexer.types.FORGET,
            Lexer.types.LIST,
            Lexer.types.RESET,
            Lexer.types.EVAL,
            Lexer.types.SAVE,
        ]
        if self.currToken.type in commandFirsts:
            self.command()
            return "command"

        # branch objectdeclaration
        if self.currToken.type == Lexer.types.BRACE_OPEN:
            self.objectdeclaration()
            return "objectdeclaration"

        # distinguish expressions from relations
        idx = self.numParsed
        isRelation = False
        while idx < len(self.tokens):
            token = self.tokens[idx]
            if token.type == Lexer.types.EQUALS:
                isRelation = True
                break
            idx += 1

        # branch relation
        if isRelation:
            self.relation()
            return "relation"

        # default branch expression
        self.expression()
        return "expression"

    @production
    def inherits(self):
        self.match(Lexer.types.PAREN_OPEN)
        self.identifiers()
        self.match(Lexer.types.PAREN_CLOSE)
        return "PAREN_OPEN identifiers PAREN_CLOSE"

    @production
    def objectdeclaration(self):
        # all branches
        self.match(Lexer.types.BRACE_OPEN)

        # branch BRACE_OPEN BRACE_CLOSE
        if self.currToken.type == Lexer.types.BRACE_CLOSE:
            self.match(Lexer.types.BRACE_CLOSE)
            return "BRACE_OPEN BRACE_CLOSE"

        # default branch BRACE_OPEN objectparameters BRACE_CLOSE
        self.objectparameters()
        self.match(Lexer.types.BRACE_CLOSE)
        return "BRACE_OPEN objectparameters BRACE_CLOSE"

    @production
    def objectparameters(self):
        # distinguish relations and aliases
        idx = self.numParsed
        isRelation = False
        isAlias = False
        while idx < len(self.tokens):
            token = self.tokens[idx]
            if token.type == Lexer.types.EQUALS:
                isRelation = True
                break
            if token.type == Lexer.types.COLON_EQUALS:
                isAlias = True
                break
            if token.type == Lexer.types.COMMA:
                break
            idx += 1

        # branch relation
        if isRelation:
            self.relation()
        # branch alias
        elif isAlias:
            self.alias()
        else:
            self._throwParseError((
                "a relation",
                "an alias",
            ))

        # branch ... COMMA objectparameters
        moreTokens = self.moreTokens() # (no more tokens is a valid branch!)
        if moreTokens and self.currToken.type == Lexer.types.COMMA:
            self.match(Lexer.types.COMMA)
            self.objectparameters()
            # (end of branch relation COMMA objectparameters)
            if isRelation:
                return "relation COMMA objectparameters"
            # (end of branch alias COMMA objectparameters)
            elif isAlias:
                return "alias COMMA objectparameters"
            else:
                self._throwParseError((
                    "this should never happen", # or
                    "the universe will blow up"
                ))
        # (end of branch relation)
        if isRelation:
            return "relation"
        # (end of branch alias)
        elif isAlias:
            return "alias"
        else:
            self._throwParseError((
                "this should never happen", # or
                "the universe will blow up"
            ))        
        
    @production
    # this one is weird... since all tokens afterward are accepted (kind of its own thing)
    def command(self):
        returnType = None

        # branch FORGET
        if self.currToken.type == Lexer.types.FORGET:
            returnType = "FORGET"

        # branch LIST
        if self.currToken.type == Lexer.types.LIST:
            returnType = "LIST"

        # branch RESET
        if self.currToken.type == Lexer.types.RESET:
            returnType = "RESET"

        # branch EVAL
        if self.currToken.type == Lexer.types.EVAL:
            returnType = "EVAL"

        # branch SAVE
        if self.currToken.type == Lexer.types.SAVE:
            returnType = "SAVE"

        if returnType is None:
            self._throwParseError((
                "'!forget'",
                "'!list'",
                "'!reset'",
                "'!eval'",
                "'!save'",
            ))

        # match the whole dang command! don't care what it is
        while self.currToken.type != Lexer.types.EOL:
            self.match(self.currToken.type)
        return returnType

    # helper function to execute callbacks
    def _runOnFunctions(self, dictKey, tokensStart, tokensEnd, branch):
        # optimization: because onFunctions is initially blank,
        # 'tokens' slices are not generated unless there are functions to call
        if dictKey in self.onFunctions:
            tokens = self.tokens[tokensStart:tokensEnd]
            for fn in self.onFunctions[dictKey]:
                try:
                    fn(tuple(tokens), branch)
                except TypeError as e:
                    if "positional argument" in str(e):
                        if str(e)[0:2] == "on":
                            raise TypeError("Parser tried to execute callback defined with the wrong number of arguments; {}".format(e))
                    raise e

    # helper functions to throw errors with expected types
    def _throwParseError(self, expectedTypes):
        raise Parser.ParseError(expectedTypes, self.currToken)

    def _throwEOLError(self):
        raise Parser.EOLError(self.currToken)
