from typing import Type
from lexer import Lexer
from errors import TracebackError


# start -> relation EOL | expression EOL | alias EOL | command EOL | EOL
#         FIRST: [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] EQUALS
#                [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN]
#                         [IDENTIFIER|BRACKET_OPEN|PAREN_OPEN]                 COLON_EQUALS
#                FORGET | LIST | RESET | EVAL | SAVE

# relation -> expression EQUALS expression
#         FIRST: <NA>
# expression -> operationlow
#         FIRST: <NA>

# evaluation -> value | PAREN_OPEN expression PAREN_CLOSE |
#     BRACKET_OPEN expression BRACKET_CLOSE
#         FIRST: IDENTIFIER | NUMBER | E_NUMBER
#                PAREN_OPEN
#                BRACKET_OPEN
# templatearguments -> expression |
#     expression COMMA templatearguments
# value -> fullidentifier | fullidentifier unit | number | number unit |
#     fullidentifier PAREN_OPEN templatearguments PAREN_CLOSE |
#     fullidentifier PAREN_OPEN PAREN_CLOSE
#         FIRST: IDENTIFIER [PERIOD|_]
#                IDENTIFIER [PERIOD|_] CARROT_LEFT
#                [NUMBER|E_NUMBER]
#                [NUMBER|E_NUMBER] CARROT_LEFT
# fullidentifier -> identifier
# identifier -> IDENTIFIER | IDENTIFIER PERIOD identifier
#         FIRST: IDENTIFIER
#                IDENTIFIER PERIOD
# identifiers -> fullidentifier | fullidentifier COMMA identifiers
#         FIRST: IDENTIFIER [PERIOD|_]
#                IDENTIFIER [PERIOD|_] COMMA
# number -> NUMBER | E_NUMBER
#         FIRST: NUMBER
#                E_NUMBER
# unit -> CARROT_LEFT expression CARROT_RIGHT
#         FIRST: <NA>

# operationlow -> operationmid operatorlow operationlow | operationmid
#         FIRST: [______] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] [PLUS|DASH]
#                [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] [STAR|SLASH|CARROT]
# operationmid -> operationhigh operatormid operationmid | operationhigh
#         FIRST: [______] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] [STAR|SLASH]
#                [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] CARROT
# operationhigh -> operationmax operatorhigh operationhigh | operationmax
#         FIRST: [______] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] CARROT
#                [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN]
# operationmax -> DASH operationmax | evaluation
#         FIRST: DASH
#                IDENTIFIER | NUMBER | E_NUMBER | PAREN_OPEN | BRACKET_OPEN
# operatorlow -> PLUS | DASH
#         FIRST: PLUS
#                DASH
# operatormid -> STAR | SLASH
#         FIRST: STAR
#                SLASH
# operatorhigh -> CARROT
#         FIRST: <NA>

# alias -> leftalias COLON_EQUALS rightalias |
#     leftaliastemp COLON_EQUALS rightaliastemp
#         FIRST: [IDENTIFIER|IDENTIFIER PERIOD|BRACKET_OPEN]
#                IDENTIFIER [PERIOD|_] PAREN_OPEN
# leftalias -> fullidentifier | BRACKET_OPEN identifiers BRACKET_CLOSE
#         FIRST: IDENTIFIER
#                BRACKET_OPEN
# rightalias -> inherits objectdeclaration | objectdeclaration | expression
#         FIRST: PAREN_OPEN
#                BRACE_OPEN
# leftaliastemp -> fullidentifier PAREN_OPEN PAREN_CLOSE |
#     fullidentifier PAREN_OPEN identifiers PAREN_CLOSE
#         FIRST: IDENTIFIER [PERIOD|_] PAREN_OPEN PAREN_CLOSE
#                IDENTIFIER [PERIOD|_] PAREN_OPEN IDENTIFIER
# rightaliastemp -> relation | expression | command | objectdeclaration
#         FIRST: [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] EQUALS
#                [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN]
#                FORGET | LIST | RESET | EVAL | SAVE
#                BRACE_OPEN

# inherits -> PAREN_OPEN identifiers PAREN_CLOSE
#         FIRST: <NA>
# objectdeclaration -> BRACE_OPEN objectparameters BRACE_CLOSE | BRACE_OPEN BRACE_CLOSE
#         FIRST: BRACE_OPEN [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN]
#                BRACE_OPEN BRACE_CLOSE
# objectparameters -> relation | alias |
#     relation COMMA objectparameters |
#     alias COMMA objectparameters
#         FIRST: [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] EQUALS
#                [DASH|_] [IDENTIFIER|NUMBER|E_NUMBER|PAREN_OPEN|BRACKET_OPEN] EQUALS COMMA

# command -> FORGET <any/none> | LIST <any/none> |
#     RESET <any/none> | EVAL <any/none> | SAVE <any/none>
#         FIRST: FORGET
#                LIST
#                RESET
#                EVAL
#                SAVE


class Parser:
    def __init__(self):
        self.onFunctions = dict()

    class ParseError(TracebackError):
        def __init__(self, expectedTypes, unexpectedToken, lineStr):
            try:
                iter(expectedTypes)
            except TypeError as e:
                if "not iterable" in str(e):
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
            super().__init__(fullMessage, lineStr, self.unexpectedToken.placementCenter)

    class EOLError(TracebackError):
        def __init__(self, eolToken, lineStr):
            if type(eolToken) is not Lexer.Token or eolToken.type != Lexer.types.EOL:
                raise TypeError("EOLError was given a bad EOL-token argument")
            self.unexpectedToken = eolToken

            fullMessage = "Unexpected end of line"
            super().__init__(fullMessage, lineStr, self.unexpectedToken.placementCenter)
    
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

    def onEvaluation(self, fn):
        self._addOnFunction("onEvaluation", fn)
        
    def onTemplateArguments(self, fn):
        self._addOnFunction("onTemplateArguments", fn)

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
        "evaluation": "onEvaluation",
        "templatearguments": "onTemplateArguments",
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

        # branch co EOL
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
            return "co EOL"

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

        # branch re EOL
        if isRelation:
            self.relation()
            self.match(Lexer.types.EOL)
            return "re EOL"
        # branch al EOL
        elif isAlias:
            self.alias()
            self.match(Lexer.types.EOL)
            return "al EOL"
        
        # default branch ex EOL
        self.expression()
        self.match(Lexer.types.EOL)
        return "ex EOL"
            
    @production
    def relation(self):
        self.expression()
        self.match(Lexer.types.EQUALS)
        self.expression()
        return "ex EQ ex"

    @production
    def expression(self):
        self.operationlow()
        return "opl"

    @production
    def evaluation(self):
        # branch PAO ex PAC
        if self.currToken.type == Lexer.types.PAREN_OPEN:
            self.match(Lexer.types.PAREN_OPEN)
            self.expression()
            self.match(Lexer.types.PAREN_CLOSE)
            return "PAO ex PAC"

        # branch BRO ex BRC
        if self.currToken.type == Lexer.types.BRACKET_OPEN:
            self.match(Lexer.types.BRACKET_OPEN)
            self.expression()
            self.match(Lexer.types.BRACKET_CLOSE)
            return "BRO ex BRC"
            
        # default branch va
        self.value()
        return "va"

    @production
    def templatearguments(self):
        # all branches
        self.expression()

        # branch ex CO tes
        moreTokens = self.moreTokens() # (no more tokens is a valid branch!)
        if moreTokens and self.currToken.type == Lexer.types.COMMA:
            self.match(Lexer.types.COMMA)
            self.templatearguments()
            return "ex CO tes"

        # (end of default branch ex)
        return "ex"

    @production
    def value(self):
        # branches nu/nu un
        numberFirsts = [
            Lexer.types.NUMBER,
            Lexer.types.E_NUMBER,
        ]
        if self.currToken.type in numberFirsts:
            # both branches
            self.number()

            # branch nu un
            unitFirsts = [
                Lexer.types.CARROT_LEFT,
            ]
            moreTokens = self.moreTokens() # (no more tokens is a valid branch!)
            if moreTokens and self.currToken.type in unitFirsts:
                self.unit()
                return "nu un"

            # (end of branch nu)
            return "nu"
        
        # branches fu/fu un/fu PAO PAC/fu PAO tes PAC
        self.fullidentifier()

        # branch fu un
        unitFirsts = [
            Lexer.types.CARROT_LEFT,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in unitFirsts:
            self.unit()
            return "fu un"

        # branch fu PAO PAC/fu PAO tes PAC
        # (would make more sense for these branches to be in
        # evaluable, but it is easier to implement here)
        elif moreTokens and self.currToken.type == Lexer.types.PAREN_OPEN:
            self.match(Lexer.types.PAREN_OPEN)
            # branch fu PAO PAC
            if self.currToken.type == Lexer.types.PAREN_CLOSE:
                self.match(Lexer.types.PAREN_CLOSE)
                return "fu PAO PAC"
            # branch fu PAO tes PAC
            self.templatearguments()
            self.match(Lexer.types.PAREN_CLOSE)
            return "fu PAO tes PAC"

        # (end of default branch fu)
        return "fu"

    @production
    def fullidentifier(self):
        self.identifier()
        return "id"

    @production
    def identifier(self):
        # all branches
        self.match(Lexer.types.IDENTIFIER)

        # branch ID PE id
        propertyFirsts = [
            Lexer.types.PERIOD,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in propertyFirsts:
            self.match(Lexer.types.PERIOD)
            self.identifier()
            return "ID PE id"

        # (end of default branch ID)
        return "ID"

    @production
    def identifiers(self):
        # all branches
        self.fullidentifier()

        # branch fu CO ids
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type == Lexer.types.COMMA:
            self.match(Lexer.types.COMMA)
            self.identifiers()
            return "fu CO ids"
        
        # (end of default branch fu)
        return "fu"

    @production
    def number(self):
        # branch EN
        if self.currToken.type == Lexer.types.E_NUMBER:
            self.match(Lexer.types.E_NUMBER)
            return "EN"

        # branch NU
        if self.currToken.type == Lexer.types.NUMBER:
            self.match(Lexer.types.NUMBER)
            return "NU"

        self._throwParseError((
            "a number",
            "an E-number (1.4E5)"
        ))

    @production
    def unit(self):
        self.match(Lexer.types.CARROT_LEFT)
        self.expression()
        self.match(Lexer.types.CARROT_RIGHT)
        return "CAL ex CAR"
    
    @production
    def operationlow(self):
        # all branches
        self.operationmid()

        # branch opm opl opl
        operatorFirsts = [
            Lexer.types.PLUS,
            Lexer.types.DASH,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in operatorFirsts:
            self.operatorlow()
            self.operationlow()
            return "opm opl opl"

        # (end of default branch opm)
        return "opm"

    @production
    def operationmid(self):
        # all branches
        self.operationhigh()

        # branch oph opm opm
        operatorFirsts = [
            Lexer.types.STAR,
            Lexer.types.SLASH,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in operatorFirsts:
            self.operatormid()
            self.operationmid()
            return "oph opm opm"

        # (end of default branch oph)
        return "oph"
    
    @production
    def operationhigh(self):
        # all branches
        self.operationmax()

        # branch opx oph oph
        operatorFirsts = [
            Lexer.types.CARROT,
        ]
        moreTokens = self.moreTokens() # no more tokens is a valid branch!
        if moreTokens and self.currToken.type in operatorFirsts:
            self.operatorhigh()
            self.operationhigh()
            return "opx oph oph"

        # (end of default branch opx)
        return "opx"

    @production
    def operationmax(self):
        # branch DA ev
        negativeFirsts = [
            Lexer.types.DASH,
        ]
        if self.currToken.type in negativeFirsts:
            self.match(Lexer.types.DASH)
            self.operationmax()
            return "DA opx"

        # default branch ev
        self.evaluation()
        return "ev"

    @production
    def operatorlow(self):
        # branch DA
        if self.currToken.type == Lexer.types.DASH:
            self.match(Lexer.types.DASH)
            return "DA"
        
        # branch PL
        if self.currToken.type == Lexer.types.PLUS:
            self.match(Lexer.types.PLUS)
            return "PL"

        self._throwParseError((
            "an operator + - * / ^",
        ))

    @production
    def operatormid(self):
        # branch SL
        if self.currToken.type == Lexer.types.SLASH:
            self.match(Lexer.types.SLASH)
            return "SL"

        # branch ST
        if self.currToken.type == Lexer.types.STAR:
            self.match(Lexer.types.STAR)
            return "ST"

        self._throwParseError((
            "an operator + - * / ^",
        ))

    @production
    def operatorhigh(self):
        if self.currToken.type == Lexer.types.CARROT:
            self.match(Lexer.types.CARROT)
            return "CA"

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
        
        # branch let CO rit
        if isTemplate:
            self.leftaliastemp()
            self.match(Lexer.types.COLON_EQUALS)
            self.rightaliastemp()
            return "let CO rit"

        # default branch le CO ri
        self.leftalias()
        self.match(Lexer.types.COLON_EQUALS)
        self.rightalias()
        return "le CO ri"

    @production
    def leftalias(self):
        # branch BRO ids BRC
        if self.currToken.type == Lexer.types.BRACKET_OPEN:
            self.match(Lexer.types.BRACKET_OPEN)
            self.identifiers()
            self.match(Lexer.types.BRACKET_CLOSE)
            return "BRO ids BRC"

        # default branch fu
        self.fullidentifier()
        return "fu"

    @production
    def rightalias(self):
        # branch ins ob
        if self.currToken.type == Lexer.types.PAREN_OPEN:
            self.inherits()
            self.objectdeclaration()
            return "ins ob"

        # branch ob
        if self.currToken.type == Lexer.types.BRACE_OPEN:
            self.objectdeclaration()
            return "ob"

        # default branch ex
        self.expression()
        return "ex"

    @production
    def leftaliastemp(self):
        # all branches
        self.fullidentifier()
        self.match(Lexer.types.PAREN_OPEN)

        # branch fu PAO PAC
        if self.currToken.type == Lexer.types.PAREN_CLOSE:
            self.match(Lexer.types.PAREN_CLOSE)
            return "fu PAO PAC"

        # branch fu PAO ids PAC
        if self.currToken.type == Lexer.types.IDENTIFIER:
            self.identifiers()
            self.match(Lexer.types.PAREN_CLOSE)
            return "fu PAO ids PAC"

        self._throwParseError((
            "a list of identifiers",
            "a closing parenthesis ')'",
        ))

    @production
    def rightaliastemp(self):
        # branch co
        commandFirsts = [
            Lexer.types.FORGET,
            Lexer.types.LIST,
            Lexer.types.RESET,
            Lexer.types.EVAL,
            Lexer.types.SAVE,
        ]
        if self.currToken.type in commandFirsts:
            self.command()
            return "co"

        # branch ob
        if self.currToken.type == Lexer.types.BRACE_OPEN:
            self.objectdeclaration()
            return "ob"

        # distinguish expressions from relations
        idx = self.numParsed
        isRelation = False
        while idx < len(self.tokens):
            token = self.tokens[idx]
            if token.type == Lexer.types.EQUALS:
                isRelation = True
                break
            idx += 1

        # branch re
        if isRelation:
            self.relation()
            return "re"

        # default branch expression
        self.expression()
        return "ex"

    @production
    def inherits(self):
        self.match(Lexer.types.PAREN_OPEN)
        self.identifiers()
        self.match(Lexer.types.PAREN_CLOSE)
        return "PAO ids PAC"

    @production
    def objectdeclaration(self):
        # all branches
        self.match(Lexer.types.BRACE_OPEN)

        # branch BRO BRC
        if self.currToken.type == Lexer.types.BRACE_CLOSE:
            self.match(Lexer.types.BRACE_CLOSE)
            return "BRO BRC"

        # default branch BRO obs BRC
        self.objectparameters()
        self.match(Lexer.types.BRACE_CLOSE)
        return "BRO obs BRC"

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

        # branch re
        if isRelation:
            self.relation()
        # branch al
        elif isAlias:
            self.alias()
        else:
            self._throwParseError((
                "a relation",
                "an alias",
            ))

        # branch ... CO obs
        moreTokens = self.moreTokens() # (no more tokens is a valid branch!)
        if moreTokens and self.currToken.type == Lexer.types.COMMA:
            self.match(Lexer.types.COMMA)
            self.objectparameters()
            # (end of branch re CO obs)
            if isRelation:
                return "re CO obs"
            # (end of branch al CO obs)
            elif isAlias:
                return "al CO obs"
            else:
                self._throwParseError((
                    "this should never happen", # or
                    "the universe will blow up"
                ))
        # (end of branch re)
        if isRelation:
            return "re"
        # (end of branch al)
        elif isAlias:
            return "al"
        else:
            self._throwParseError((
                "this should never happen", # or
                "the universe will blow up"
            ))        
        
    @production
    # this one is weird... since all tokens afterward are accepted (kind of its own thing)
    def command(self):
        returnType = None

        # branch FO
        if self.currToken.type == Lexer.types.FORGET:
            returnType = "FO"

        # branch LI
        if self.currToken.type == Lexer.types.LIST:
            returnType = "LI"

        # branch RE
        if self.currToken.type == Lexer.types.RESET:
            returnType = "RE"

        # branch EV
        if self.currToken.type == Lexer.types.EVAL:
            returnType = "EV"

        # branch SA
        if self.currToken.type == Lexer.types.SAVE:
            returnType = "SA"

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
                        raise TypeError("Parser tried to execute callback defined with the wrong number of arguments; {}".format(e))
                    raise e

    # helper functions to throw errors with expected types
    def _throwParseError(self, expectedTypes):
        raise Parser.ParseError(expectedTypes, self.currToken, self.origLineStr)

    def _throwEOLError(self):
        raise Parser.EOLError(self.currToken, self.origLineStr)
