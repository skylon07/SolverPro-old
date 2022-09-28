from email.policy import strict
from main import *
from algebramaster import Solver
from structures import *

ISP = ' ' * len(USER_INPUT)

class TestSuites:
    @classmethod
    def Lexer(cls):
        def T(string):
            tokenType = lexer.findType_forTesting(string)
            return lexer.Token(string, tokenType, 0)

        def EOL():
            return lexer.Token("", lexer.types.EOL, 0)

        def mapTokenType(iterable):
            return map(lambda token: token.type, iterable)

        # token stringification
        Tester.assertEqual(str(T("myToken")), "myToken", "tokens can be stringified")
        
        # token equality
        Tester.assertEqual(T(":"), T(":"), "tokens can be 'equal'")
        Tester.assertEqual(T("myVar"), T("myVar"), "tokens can be 'equal'")
        Tester.assertEqual(T("μ"), T("μ"), "tokens can be 'equal'")

        Tester.assertNotEqual(T(":"), T("."), "tokens can be 'unequal'")
        Tester.assertNotEqual(T("myVar"), T("myVar2"), "tokens can be 'unequal'")
        Tester.assertNotEqual(T("μ"), T("x̅"), "tokens can be 'unequal'")
        Tester.stopIfFailed()

        # token typing
        Tester.assertEqual(lexer.types.FORGET, "FORGET", "type FORGET is 'FORGET'")
        Tester.assertEqual(lexer.types.LIST, "LIST", "type LIST is 'LIST'")
        Tester.assertEqual(lexer.types.RESET, "RESET", "type RESET is 'RESET'")
        Tester.assertEqual(lexer.types.EVAL, "EVAL", "type EVAL is 'EVAL'")
        Tester.assertEqual(lexer.types.SAVE, "SAVE", "type SAVE is 'SAVE'")
        Tester.assertEqual(lexer.types.COLON_EQUALS, "COLON_EQUALS", "type COLON_EQUALS is 'COLON_EQUALS'")
        Tester.assertEqual(lexer.types.IDENTIFIER, "IDENTIFIER", "type IDENTIFIER is 'IDENTIFIER'")
        Tester.assertEqual(lexer.types.NUMBER, "NUMBER", "type NUMBER is 'NUMBER'")
        Tester.assertEqual(lexer.types.E_NUMBER, "E_NUMBER", "type E_NUMBER is 'E_NUMBER'")
        Tester.assertEqual(lexer.types.PERIOD, "PERIOD", "type PERIOD is 'PERIOD'")
        Tester.assertEqual(lexer.types.COMMA, "COMMA", "type COMMA is 'COMMA'")
        Tester.assertEqual(lexer.types.COMMENT, "COMMENT", "type COMMENT is 'COMMENT'")
        Tester.assertEqual(lexer.types.BRACE_OPEN, "BRACE_OPEN", "type BRACE_OPEN is 'BRACE_OPEN'")
        Tester.assertEqual(lexer.types.BRACE_CLOSE, "BRACE_CLOSE", "type BRACE_CLOSE is 'BRACE_CLOSE'")
        Tester.assertEqual(lexer.types.BRACKET_OPEN, "BRACKET_OPEN", "type BRACKET_OPEN is 'BRACKET_OPEN'")
        Tester.assertEqual(lexer.types.BRACKET_CLOSE, "BRACKET_CLOSE", "type BRACKET_CLOSE is 'BRACKET_CLOSE'")
        Tester.assertEqual(lexer.types.PAREN_OPEN, "PAREN_OPEN", "type PAREN_OPEN is 'PAREN_OPEN'")
        Tester.assertEqual(lexer.types.PAREN_CLOSE, "PAREN_CLOSE", "type PAREN_CLOSE is 'PAREN_CLOSE'")
        Tester.assertEqual(lexer.types.CARROT_LEFT, "CARROT_LEFT", "type CARROT_LEFT is 'CARROT_LEFT'")
        Tester.assertEqual(lexer.types.CARROT_RIGHT, "CARROT_RIGHT", "type CARROT_RIGHT is 'CARROT_RIGHT'")
        Tester.assertEqual(lexer.types.EQUALS, "EQUALS", "type EQUALS is 'EQUALS'")
        Tester.assertEqual(lexer.types.PLUS, "PLUS", "type PLUS is 'PLUS'")
        Tester.assertEqual(lexer.types.DASH, "DASH", "type DASH is 'DASH'")
        Tester.assertEqual(lexer.types.STAR, "STAR", "type STAR is 'STAR'")
        Tester.assertEqual(lexer.types.SLASH, "SLASH", "type SLASH is 'SLASH'")
        Tester.assertEqual(lexer.types.CARROT, "CARROT", "type CARROT is 'CARROT'")
        Tester.assertEqual(lexer.types.EOL, "EOL", "type EOL is 'EOL'")
        Tester.assertEqual(lexer.types.INVALID, "INVALID", "type INVALID is 'INVALID'")
        Tester.stopIfFailed()
        
        Tester.assertEqual(T(":=").type, lexer.types.COLON_EQUALS, "can get type of COLON_EQUALS Token")
        Tester.assertEqual(T("+").type, lexer.types.PLUS, "can get type of PLUS Token")
        Tester.assertEqual(T("myVar").type, lexer.types.IDENTIFIER, "can get type of IDENTIFIER Token")
        Tester.assertEqual(T("μ").type, lexer.types.INVALID, "can get type of INVALID Token")
        Tester.stopIfFailed()
        
        # correct type return
        results = lexer.process("")
        Tester.assertEqual(type(results), tuple, "returns processed strings as tuples (uses empty string)")
        Tester.stopIfFailed()

        # empty string processing
        results = lexer.process("")
        Tester.assertItersEqual(results, [EOL()], "can process empty string")

        # (ignored) whitespace processing
        results = lexer.process(" ")
        Tester.assertItersEqual(results, [EOL()], "finds no tokens in single spaces")

        results = lexer.process("     ")
        Tester.assertItersEqual(results, [EOL()], "finds no tokens in multiple spaces")

        results = lexer.process("\t")
        Tester.assertItersEqual(results, [EOL()], "finds no tokens in single tabs")

        results = lexer.process("\t    \t        \t  \t ")
        Tester.assertItersEqual(results, [EOL()], "finds no tokens in combinations of spaces and tabs")

        results = lexer.process("  \t\t\t\t  \t\t\t\t   \t  \t")
        Tester.assertItersEqual(results, [EOL()], "finds no tokens in combinations of spaces and tabs")
        Tester.stopIfFailed()

        # newline/EOL/input termination
        results = lexer.process("\n")
        Tester.assertItersEqual(results, [T("\n"), EOL()], "can process single newlines/EOL")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.EOL, lexer.types.EOL], "can process single newlines/EOL")

        results = lexer.process("\n\n\n")
        Tester.assertItersEqual(results, [T("\n"), T("\n"), T("\n"), EOL()], "can process multiple newlines/EOLs")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.EOL, lexer.types.EOL, lexer.types.EOL, lexer.types.EOL], "can process multiple newlines/EOLs")

        # ALL whitespace processing
        results = lexer.process("\n\t\n \n\t\n \n ") # \n * 5
        Tester.assertItersEqual(results, [T("\n"), T("\n"), T("\n"), T("\n"), T("\n"), EOL()], "can process multiple newlines/EOLs (x5)")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.EOL, lexer.types.EOL, lexer.types.EOL, lexer.types.EOL, lexer.types.EOL, lexer.types.EOL], "can process multiple newlines/EOLs (x5)")

        results = lexer.process(" \t\t  \n\t    \t  \n\t\t  \t \n\t\t") # \n * 3
        Tester.assertItersEqual(results, [T("\n"), T("\n"), T("\n"), EOL()], "can process multiple newlines/EOLs (x3)")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.EOL, lexer.types.EOL, lexer.types.EOL, lexer.types.EOL], "can process multiple newlines/EOLs (x3)")

        results = lexer.process("\t\t\t    \t\t\t   \n\t \n\t\n\t\n   ") # \n * 4
        Tester.assertItersEqual(results, [T("\n"), T("\n"), T("\n"), T("\n"), EOL()], "can process multiple newlines/EOLs (x4)")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.EOL, lexer.types.EOL, lexer.types.EOL, lexer.types.EOL, lexer.types.EOL], "can process multiple newlines/EOLs (x4)")
        Tester.stopIfFailed()

        # command keywords
        results = lexer.process("!forget")
        Tester.assertItersEqual(results, [T("!forget"), EOL()], "'!forget' command")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.FORGET, lexer.types.EOL], "'!forget' command")

        results = lexer.process("!list")
        Tester.assertItersEqual(results, [T("!list"), EOL()], "'!list' command")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.LIST, lexer.types.EOL], "'!list' command")

        results = lexer.process("!reset")
        Tester.assertItersEqual(results, [T("!reset"), EOL()], "'!reset' command")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.RESET, lexer.types.EOL], "'!reset' command")

        results = lexer.process("!eval")
        Tester.assertItersEqual(results, [T("!eval"), EOL()], "'!eval' command")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.EVAL, lexer.types.EOL], "'!eval' command")

        results = lexer.process("!save")
        Tester.assertItersEqual(results, [T("!save"), EOL()], "'!save' command")
        Tester.assertItersEqual(mapTokenType(results), [lexer.types.SAVE, lexer.types.EOL], "'!save' command")

        # process object declarations
        results = lexer.process("myVar :=")
        Tester.assertItersEqual(
            results,
            [T("myVar"), T(":="), EOL()],
            "can process object declarations (basic form)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.IDENTIFIER, lexer.types.COLON_EQUALS, lexer.types.EOL],
            "can process object declarations (basic form)"
        )

        results = lexer.process("{}")
        Tester.assertItersEqual(
            results,
            [T("{"), T("}"), EOL()],
            "can process object declarations (brace pairs)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.BRACE_OPEN, lexer.types.BRACE_CLOSE, lexer.types.EOL],
            "can process object declarations (brace pairs)"
        )

        results = lexer.process("myObj {}")
        Tester.assertItersEqual(
            results,
            [T("myObj"), T("{"), T("}"), EOL()],
            "can process object declarations (name and brace pairs)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.IDENTIFIER, lexer.types.BRACE_OPEN, lexer.types.BRACE_CLOSE, lexer.types.EOL],
            "can process object declarations (name and brace pairs)"
        )

        results = lexer.process("{ myProp = 2 }")
        Tester.assertItersEqual(
            results,
            [T("{"), T("myProp"), T("="), T("2"), T("}"), EOL()],
            "can process object declarations (single property definition)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.BRACE_OPEN, lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.BRACE_CLOSE, lexer.types.EOL],
            "can process object declarations (single property definition)"
        )

        results = lexer.process("{p1=4,p2=3,p3=2}")
        Tester.assertItersEqual(
            results,
            [
                T("{"),
                T("p1"), T("="), T("4"), T(","),
                T("p2"), T("="), T("3"), T(","),
                T("p3"), T("="), T("2"),
                T("}"),
                EOL()
            ],
            "can process object declarations (multiple property definitions)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.BRACE_OPEN,
                lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.COMMA,
                lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.COMMA,
                lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER,
                lexer.types.BRACE_CLOSE,
                lexer.types.EOL
            ],
            "can process object declarations (multiple property definitions)"
        )

        results = lexer.process("{ p1 = 4 <meters> }")
        Tester.assertItersEqual(
            results,
            [T("{"), T("p1"), T("="), T("4"), T("<"), T("meters"), T(">"), T("}"), EOL()],
            "can process object declarations (single property definition with unit)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.BRACE_OPEN, lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT, lexer.types.BRACE_CLOSE, lexer.types.EOL],
            "can process object declarations (single property definition with unit)"
        )

        results = lexer.process("{ p1 = 2<m>, p2 = 4<s> }")
        Tester.assertItersEqual(
            results,
            [
                T("{"),
                T("p1"), T("="), T("2"), T("<"), T("m"), T(">"), T(","),
                T("p2"), T("="), T("4"), T("<"), T("s"), T(">"),
                T("}"),
                EOL()
            ],
            "can process object declarations (multiple property definitions with units)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.BRACE_OPEN,
                lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT, lexer.types.COMMA,
                lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT,
                lexer.types.BRACE_CLOSE,
                lexer.types.EOL
            ],
            "can process object declarations (multiple property definitions with units)"
        )

        results = lexer.process("myObj := (otherObj) {}")
        Tester.assertItersEqual(
            results,
            [
                T("myObj"), T(":="),
                T("("), T("otherObj"), T(")"),
                T("{"), T("}"),
                EOL()
            ],
            "can process object declarations (object merge-copy syntax)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.IDENTIFIER, lexer.types.COLON_EQUALS,
                lexer.types.PAREN_OPEN, lexer.types.IDENTIFIER, lexer.types.PAREN_CLOSE,
                lexer.types.BRACE_OPEN, lexer.types.BRACE_CLOSE,
                lexer.types.EOL
            ],
            "can process object declarations (object merge-copy syntax)"
        )

        results = lexer.process("{ prop = otherProp + 3 }")
        Tester.assertItersEqual(
            results,
            [T("{"), T("prop"), T("="), T("otherProp"), T("+"), T("3"), T("}"), EOL()],
            "can process object declarations (single property definition with expression)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.BRACE_OPEN, lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.IDENTIFIER, lexer.types.PLUS, lexer.types.NUMBER, lexer.types.BRACE_CLOSE, lexer.types.EOL],
            "can process object declarations (single property definition with expression)"
        )

        results = lexer.process("{ prop = 2*otherProp + 4, otherProp = something / 3 - 2 }")
        Tester.assertItersEqual(
            results,
            [
                T("{"),
                T("prop"), T("="), T("2"), T("*"), T("otherProp"), T("+"), T("4"), T(","),
                T("otherProp"), T("="), T("something"), T("/"), T("3"), T("-"), T("2"),
                T("}"),
                EOL()
            ],
            "can process object declarations (multiple property definitions with expressions)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.BRACE_OPEN,
                lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.STAR, lexer.types.IDENTIFIER, lexer.types.PLUS, lexer.types.NUMBER, lexer.types.COMMA,
                lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.IDENTIFIER, lexer.types.SLASH, lexer.types.NUMBER, lexer.types.DASH, lexer.types.NUMBER,
                lexer.types.BRACE_CLOSE,
                lexer.types.EOL
            ],
            "can process object declarations (multiple property definitions with expressions)"
        )

        results = lexer.process("{ p1 + p2 = p3 + p4, p2 / 2 = p1 - p3 * p4 }")
        Tester.assertItersEqual(
            results,
            [
                T("{"),
                T("p1"), T("+"), T("p2"), T("="), T("p3"), T("+"), T("p4"), T(","),
                T("p2"), T("/"), T("2"), T("="), T("p1"), T("-"), T("p3"), T("*"), T("p4"),
                T("}"),
                EOL()
            ],
            "can process object declarations (multiple property definitions with expressions)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.BRACE_OPEN,
                lexer.types.IDENTIFIER, lexer.types.PLUS, lexer.types.IDENTIFIER, lexer.types.EQUALS, lexer.types.IDENTIFIER, lexer.types.PLUS, lexer.types.IDENTIFIER, lexer.types.COMMA,
                lexer.types.IDENTIFIER, lexer.types.SLASH, lexer.types.NUMBER, lexer.types.EQUALS, lexer.types.IDENTIFIER, lexer.types.DASH, lexer.types.IDENTIFIER, lexer.types.STAR, lexer.types.IDENTIFIER,
                lexer.types.BRACE_CLOSE,
                lexer.types.EOL
            ],
            "can process object declarations (multiple property definitions with expressions)"
        )

        results = lexer.process("{ p1<m> / p2<s> = p3<v> }")
        Tester.assertItersEqual(
            results,
            [
                T("{"),
                T("p1"), T("<"), T("m"), T(">"),
                T("/"),
                T("p2"), T("<"), T("s"), T(">"),
                T("="),
                T("p3"), T("<"), T("v"), T(">"),
                T("}"),
                EOL()
            ],
            "can process object declarations (single property expression with units)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.BRACE_OPEN,
                lexer.types.IDENTIFIER, lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT,
                lexer.types.SLASH,
                lexer.types.IDENTIFIER, lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT,
                lexer.types.EQUALS,
                lexer.types.IDENTIFIER, lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT,
                lexer.types.BRACE_CLOSE,
                lexer.types.EOL
            ],
            "can process object declarations (single property expression with units)"
        )

        results = lexer.process("[merge1, merge2] := (obj1, obj2)")
        Tester.assertItersEqual(
            results,
            [
                T("["), T("merge1"), T(","), T("merge2"), T("]"), T(":="),
                T("("), T("obj1"), T(","), T("obj2"), T(")"),
                EOL()
            ],
            "can process object declarations (merging syntax)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.BRACKET_OPEN, lexer.types.IDENTIFIER, lexer.types.COMMA, lexer.types.IDENTIFIER, lexer.types.BRACKET_CLOSE, lexer.types.COLON_EQUALS,
                lexer.types.PAREN_OPEN, lexer.types.IDENTIFIER, lexer.types.COMMA, lexer.types.IDENTIFIER, lexer.types.PAREN_CLOSE,
                lexer.types.EOL
            ],
            "can process object declarations (merging syntax)"
        )

        # process other declarations
        results = lexer.process("<meters>")
        Tester.assertItersEqual(
            results,
            [T("<"), T("meters"), T(">"), EOL()],
            "can process unit declarations"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT, lexer.types.EOL],
            "can process unit declarations"
        )

        results = lexer.process("<m>:=<meters>")
        Tester.assertItersEqual(
            results,
            [T("<"), T("m"), T(">"), T(":="), T("<"), T("meters"), T(">"), EOL()],
            "can process alias declarations (for units)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT, lexer.types.COLON_EQUALS, lexer.types.CARROT_LEFT, lexer.types.IDENTIFIER, lexer.types.CARROT_RIGHT, lexer.types.EOL],
            "can process alias declarations (for units)"
        )

        results = lexer.process("!forget object1, obj2.prop2, unit3")
        Tester.assertItersEqual(
            results,
            [
                T("!forget"),
                T("object1"), T(","),
                T("obj2"), T("."), T("prop2"), T(","),
                T("unit3"),
                EOL()
            ],
            "can process forget statements"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.FORGET,
                lexer.types.IDENTIFIER, lexer.types.COMMA,
                lexer.types.IDENTIFIER, lexer.types.PERIOD, lexer.types.IDENTIFIER, lexer.types.COMMA,
                lexer.types.IDENTIFIER,
                lexer.types.EOL
            ],
            "can process forget statements"
        )

        results = lexer.process("!reset")
        Tester.assertItersEqual(
            results,
            [T("!reset"), EOL()],
            "can process reset statements"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.RESET, lexer.types.EOL],
            "can process reset statements"
        )

        results = lexer.process("!eval /user/object/somefile.txt")
        Tester.assertItersEqual(
            results,
            [T("!eval"), T("/"), T("user"), T("/"), T("object"), T("/"), T("somefile"), T("."), T("txt"), EOL()],
            "can process eval statements"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.EVAL, lexer.types.SLASH, lexer.types.IDENTIFIER, lexer.types.SLASH, lexer.types.IDENTIFIER, lexer.types.SLASH, lexer.types.IDENTIFIER, lexer.types.PERIOD, lexer.types.IDENTIFIER, lexer.types.EOL],
            "can process eval statements"
        )

        results = lexer.process("!save /user/object/somefile.slv")
        Tester.assertItersEqual(
            results,
            [T("!save"), T("/"), T("user"), T("/"), T("object"), T("/"), T("somefile"), T("."), T("slv"), EOL()],
            "can process save statements"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.SAVE, lexer.types.SLASH, lexer.types.IDENTIFIER, lexer.types.SLASH, lexer.types.IDENTIFIER, lexer.types.SLASH, lexer.types.IDENTIFIER, lexer.types.PERIOD, lexer.types.IDENTIFIER, lexer.types.EOL],
            "can process save statements"
        )

        # comment tests
        results = lexer.process("# this is a test")
        Tester.assertItersEqual(
            results,
            [T("# this is a test"), EOL()],
            "inline comments (single valid)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.COMMENT, lexer.types.EOL],
            "inline comments (single valid)"
        )

        results = lexer.process("some ids # this is a test")
        Tester.assertItersEqual(
            results,
            [T("some"), T("ids"), T("# this is a test"), EOL()],
            "inline comments (valid with ids before)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.IDENTIFIER, lexer.types.IDENTIFIER, lexer.types.COMMENT, lexer.types.EOL],
            "inline comments (valid with ids before)"
        )

        results = lexer.process("# this is a test # and this is still one comment")
        Tester.assertItersEqual(
            results,
            [T("# this is a test # and this is still one comment"), EOL()],
            "inline comments (comment with '#' inside)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.COMMENT, lexer.types.EOL],
            "inline comments (comment with '#' inside)"
        )

        results = lexer.process("##")
        Tester.assertItersEqual(
            results,
            [T("##"), EOL()],
            "inline comments (two '#')"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.COMMENT, lexer.types.EOL],
            "inline comments (two '#')"
        )

        results = lexer.process("#####")
        Tester.assertItersEqual(
            results,
            [T("#####"), EOL()],
            "inline comments (multiple '#')"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.COMMENT, lexer.types.EOL],
            "inline comments (multiple '#')"
        )

        results = lexer.process("not a comment# this is a comment\nthis is not")
        Tester.assertItersEqual(
            results,
            [
                T("not"), T("a"), T("comment"),
                T("# this is a comment"), T("\n"),
                T("this"), T("is"), T("not"),
                EOL()
            ],
            "inline comments (EOL-terminated)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.IDENTIFIER, lexer.types.IDENTIFIER, lexer.types.IDENTIFIER,
                lexer.types.COMMENT, lexer.types.EOL,
                lexer.types.IDENTIFIER, lexer.types.IDENTIFIER, lexer.types.IDENTIFIER,
                lexer.types.EOL
            ],
            "inline comments (EOL-terminated)"
        )

        # other random/edge cases
        results = lexer.process("((<[{>)}])")
        Tester.assertItersEqual(
            results,
            [T("("), T("("), T("<"), T("["), T("{"), T(">"), T(")"), T("}"), T("]"), T(")"), EOL()],
            "multiple mismatching braces and brackets"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.PAREN_OPEN, lexer.types.PAREN_OPEN, lexer.types.CARROT_LEFT, lexer.types.BRACKET_OPEN, lexer.types.BRACE_OPEN, lexer.types.CARROT_RIGHT, lexer.types.PAREN_CLOSE, lexer.types.BRACE_CLOSE, lexer.types.BRACKET_CLOSE, lexer.types.PAREN_CLOSE, lexer.types.EOL],
            "multiple mismatching braces and brackets"
        )

        results = lexer.process("4 1.5 a6 a 6 1_b a.b1 4.5.6")
        Tester.assertItersEqual(
            results,
            [T("4"), T("1.5"), T("a6"), T("a"), T("6"), T("1"), T("_b"), T("a"), T("."), T("b1"), T("4.5"), T(".6"), EOL()],
            "knows difference between numbers and identifiers"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.IDENTIFIER, lexer.types.IDENTIFIER, lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.IDENTIFIER, lexer.types.IDENTIFIER, lexer.types.PERIOD, lexer.types.IDENTIFIER, lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.EOL],
            "knows difference between numbers and identifiers"
        )

        results = lexer.process("4=a+3=-4*=5/2^6*4")
        Tester.assertItersEqual(
            results,
            [
                T("4"), T("="), T("a"), T("+"), T("3"), T("="),
                T("-"), T("4"), T("*"), T("="), T("5"), T("/"),
                T("2"), T("^"), T("6"), T("*"), T("4"),
                EOL()
            ],
            "can process mathematical operators"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [
                lexer.types.NUMBER, lexer.types.EQUALS, lexer.types.IDENTIFIER, lexer.types.PLUS, lexer.types.NUMBER, lexer.types.EQUALS,
                lexer.types.DASH, lexer.types.NUMBER, lexer.types.STAR, lexer.types.EQUALS, lexer.types.NUMBER, lexer.types.SLASH,
                lexer.types.NUMBER, lexer.types.CARROT, lexer.types.NUMBER, lexer.types.STAR, lexer.types.NUMBER,
                lexer.types.EOL
            ],
            "can process mathematical operators"
        )

        results = lexer.process("3E4 4E-12 7 E14 4E -14 3.4E4.5")
        Tester.assertItersEqual(
            results,
            [T("3E4"), T("4E-12"), T("7"), T("E14"), T("4"), T("E"), T("-"), T("14"), T("3.4E4"), T(".5"), EOL()],
            "can process scientific numbers with 'E'"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.E_NUMBER, lexer.types.E_NUMBER, lexer.types.NUMBER, lexer.types.IDENTIFIER, lexer.types.NUMBER, lexer.types.IDENTIFIER, lexer.types.DASH, lexer.types.NUMBER, lexer.types.E_NUMBER, lexer.types.NUMBER, lexer.types.EOL],
            "can process scientific numbers with 'E'"
        )

        results = lexer.process("3e+4 4e-12 7 e+14 4e -14 3.4e+4.5")
        Tester.assertItersEqual(
            results,
            [T("3e+4"), T("4e-12"), T("7"), T("e"), T("+"), T("14"), T("4"), T("e"), T("-"), T("14"), T("3.4e+4"), T(".5"), EOL()],
            "can process scientific numbers with 'E' (little e cases)"
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.E_NUMBER, lexer.types.E_NUMBER, lexer.types.NUMBER, lexer.types.IDENTIFIER, lexer.types.PLUS, lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.IDENTIFIER, lexer.types.DASH, lexer.types.NUMBER, lexer.types.E_NUMBER, lexer.types.NUMBER, lexer.types.EOL],
            "can process scientific numbers with 'E' (little e cases)"
        )

        results = lexer.process("1 2 3. 4.0 5.12 6.")
        Tester.assertItersEqual(
            results,
            [T("1"), T("2"), T("3."), T("4.0"), T("5.12"), T("6."), EOL()],
            "can process numbers with no period, single period and no 'rest', or single period with 'rest'",
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.NUMBER, lexer.types.EOL],
            "can process numbers with no period, single period and no 'rest', or single period with 'rest'",
        )

        results = lexer.process(".E+4 .1e5 4.E-7")
        Tester.assertItersEqual(
            results,
            [T('.'), T("E"), T("+"), T("4"), T(".1e5"), T("4.E-7"), EOL()],
            "can process E-numbers with different period combinations",
        )
        Tester.assertItersEqual(
            mapTokenType(results),
            [lexer.types.PERIOD, lexer.types.IDENTIFIER, lexer.types.PLUS, lexer.types.NUMBER, lexer.types.E_NUMBER, lexer.types.E_NUMBER, lexer.types.EOL],
            "can process E-numbers with different period combinations",
        )
        Tester.stopIfFailed()

        # test token error data
        results = lexer.process("even + odd")
        Tester.assertEqual(results[0].placementStart, 0, "placement start idx (first token)")
        Tester.assertEqual(results[0].placementCenter, 2, "placement center idx (first token)")
        Tester.assertEqual(results[1].placementStart, 5, "placement start idx (second token)")
        Tester.assertEqual(results[1].placementCenter, 5, "placement center idx (second token)")
        Tester.assertEqual(results[2].placementStart, 7, "placement start idx (third token)")
        Tester.assertEqual(results[2].placementCenter, 8, "placement center idx (third token)")
        Tester.stopIfFailed()
    
    @classmethod
    def Parser(cls):
        # only global variables can be accessed inside exec()
        # (used below to generate "on-functions")
        # https://stackoverflow.com/questions/23168282/setting-variables-with-exec-inside-a-function
        global PARSER_ON_DICT
        PARSER_ON_DICT = dict()
        def resetOnDict():
            PARSER_ON_DICT["onStart"] = []

            PARSER_ON_DICT["onRelation"] = []
            PARSER_ON_DICT["onExpression"] = []
            PARSER_ON_DICT["onExpressions"] = []

            PARSER_ON_DICT["onEvaluation"] = []
            PARSER_ON_DICT["onValue"] = []
            PARSER_ON_DICT["onFullIdentifier"] = []
            PARSER_ON_DICT["onIdentifier"] = []
            PARSER_ON_DICT["onIdentifiers"] = []
            PARSER_ON_DICT["onNumber"] = []
            PARSER_ON_DICT["onUnit"] = []
            
            PARSER_ON_DICT["onOperationLow"] = []
            PARSER_ON_DICT["onOperationMid"] = []
            PARSER_ON_DICT["onOperationHigh"] = []
            PARSER_ON_DICT["onOperationMax"] = []
            PARSER_ON_DICT["onOperatorLow"] = []
            PARSER_ON_DICT["onOperatorMid"] = []
            PARSER_ON_DICT["onOperatorHigh"] = []

            PARSER_ON_DICT["onAlias"] = []
            PARSER_ON_DICT["onLeftAlias"] = []
            PARSER_ON_DICT["onRightAlias"] = []
            PARSER_ON_DICT["onLeftAliasTemp"] = []
            PARSER_ON_DICT["onRightAliasTemp"] = []
            
            PARSER_ON_DICT["onInherits"] = []
            PARSER_ON_DICT["onObjectDeclaration"] = []
            PARSER_ON_DICT["onObjectParameters"] = []
            
            PARSER_ON_DICT["onCommand"] = []
        resetOnDict()

        for onFnName in PARSER_ON_DICT:
            exec("""
            def {0}(*args):
                PARSER_ON_DICT["{0}"].append(args)
            """
            .replace("            ", "")
            .format(onFnName), globals(), locals())

        # return type is none
        Tester.assertEqual(parser.inspect([], ""), None, "inspect() returns nothing (on empty list)")

        # provides interface methods
        for onFnName in PARSER_ON_DICT:
            # Tester.assertIs(type(parser.onRelation), methodType, 'parser provides onRelation()')
            exec("Tester.assertIs(callable(parser.{0}), True, 'parser provides {0}() function')".format(onFnName))
        Tester.stopIfFailed()

        # set up for testing interface methods
        resetOnDict()
        for onFnName in PARSER_ON_DICT:
            # parser.onRelation(onRelation)
            exec("parser.{0}({0})".format(onFnName))

        def testInspectPerforms(tokensToInspect, origLineStr, onFnName, calledWithList, extraTestName):
            parser.inspect(tokensToInspect, origLineStr)
            Tester.assertEqual(len(PARSER_ON_DICT[onFnName]), len(calledWithList), "{}() runs the correct number of times ({})".format(onFnName, extraTestName))
            Tester.assertItersEqual(PARSER_ON_DICT[onFnName], calledWithList, "{}() runs with the right arguments ({})".format(onFnName, extraTestName))
            resetOnDict()

        def testInspectErrors(tokensToInspect, origLineStr, errorType, whenGivenTestName):
            def wrapper():
                parser.inspect(tokensToInspect, origLineStr)
            Tester.assertFunctionErrors(wrapper, errorType, "parser should error when given {}".format(whenGivenTestName))
            resetOnDict()

        # ensure parser has error types
        Tester.assertIs(isinstance(parser.ParseError, type), True, "ensure parser has error types (ParseError)")
        Tester.assertIs(isinstance(parser.EOLError, type), True, "ensure parser has error types (EOLError)")
        Tester.stopIfFailed()

        # test bottom productions
        # (fullidentifier, identifier, number, operator-s <relies on expression>, command)
        lineStr = "myVar"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myVar", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onFullIdentifier", [(expTokens, "identifier")], "single identifier")
        lineStr = "myVar.myProp"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myVar.myProp", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onFullIdentifier", [(expTokens, "identifier")], "two-id single-property identifier")
        lineStr = "myVar.myObj.myProp"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myVar.myObj.myProp", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onFullIdentifier", [(expTokens, "identifier")], "three-id two-property identifier")

        lineStr = "myVar"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myVar", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onIdentifier", [(expTokens, "IDENTIFIER")], "single identifier")
        lineStr = "myVar.myProp"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myVar.myProp", withEOL=False)
        propTokens = lexer.process("myProp", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onIdentifier", [(propTokens, "IDENTIFIER"), (expTokens, "IDENTIFIER PERIOD identifier")], "two-id single-property identifier")
        lineStr = "myVar.myObj.myProp"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myVar.myObj.myProp", withEOL=False)
        objTokens = lexer.process("myObj.myProp", withEOL=False)
        propTokens = lexer.process("myProp", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onIdentifier", [(propTokens, "IDENTIFIER"), (objTokens, "IDENTIFIER PERIOD identifier"), (expTokens, "IDENTIFIER PERIOD identifier")], "three-id two-property identifier")
        
        lineStr = "myVar..myProp"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "a property separated by two periods")
        lineStr = "myVar.4567"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "a numeric where property should be")
        lineStr = "myVar.myObj..myProp"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "a property separated by two periods after another property")

        lineStr = "45"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("45", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onNumber", [(expTokens, "NUMBER")], "single number")
        lineStr = "5.6"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("5.6", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onNumber", [(expTokens, "NUMBER")], "single decimal number")
        lineStr = "2e10"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("2e10", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onNumber", [(expTokens, "ENUMBER")], "single E-number")
        lineStr = "4.7E-16"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("4.7E-16", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onNumber", [(expTokens, "ENUMBER")], "single E-number with decimal")

        lineStr = "a + b"
        tokens = lexer.process(lineStr)
        opTokens = lexer.process("+", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperatorLow", [(opTokens, "PLUS")], "add operator")
        lineStr = "a - b"
        tokens = lexer.process(lineStr)
        opTokens = lexer.process("-", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperatorLow", [(opTokens, "DASH")], "subtract operator")
        lineStr = "a * b"
        tokens = lexer.process(lineStr)
        opTokens = lexer.process("*", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperatorMid", [(opTokens, "STAR")], "multiply operator")
        lineStr = "a / b"
        tokens = lexer.process(lineStr)
        opTokens = lexer.process("/", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperatorMid", [(opTokens, "SLASH")], "divide operator")
        lineStr = "a ^ b"
        tokens = lexer.process(lineStr)
        opTokens = lexer.process("^", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperatorHigh", [(opTokens, "CARROT")], "exponentiate operator")

        lineStr = "!forget"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!forget", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "FORGET")], "!forget command")
        lineStr = "!list"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!list", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "LIST")], "!list command")
        lineStr = "!reset"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!reset", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "RESET")], "!reset command")
        lineStr = "!eval"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!eval", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "EVAL")], "!eval command")
        lineStr = "!save"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!save", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "SAVE")], "!save command")
        
        lineStr = "!forget arg1 arg2 arg3"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!forget arg1 arg2 arg3", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "FORGET")], "!forget command")
        lineStr = "!list arg1 arg2 arg3"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!list arg1 arg2 arg3", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "LIST")], "!list command")
        lineStr = "!reset arg1 arg2 arg3"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!reset arg1 arg2 arg3", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "RESET")], "!reset command")
        lineStr = "!eval arg1 arg2 arg3"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!eval arg1 arg2 arg3", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "EVAL")], "!eval command")
        lineStr = "!save arg1 arg2 arg3"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!save arg1 arg2 arg3", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onCommand", [(expTokens, "SAVE")], "!save command")
        Tester.stopIfFailed()

        # test middle productions
        # (value (no units), identifiers <relies on leftalias>,
        # leftalias <relies on alias>, leftaliastemp <relies on alias>,
        # inherits <relies on rightalias>)
        lineStr = "myIdentifier"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myIdentifier", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(expTokens, "fullidentifier")], "single identifier")
        lineStr = "myIdentifier.myProperty"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myIdentifier.myProperty", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(expTokens, "fullidentifier")], "two-id single-property identifier")
        lineStr = "14"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("14", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(expTokens, "number")], "single number")
        lineStr = "21.20"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("21.20", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(expTokens, "number")], "single decimal number")
        lineStr = "45e-2"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("45e-2", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(expTokens, "number")], "single E-number")
        lineStr = "0.00123E+45"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("0.00123E+45", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(expTokens, "number")], "single E-number with decimal")

        lineStr = "[myId1]:={}"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("myId1", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onIdentifiers", [(expTokens, "fullidentifier")], "single identifier")
        lineStr = "[myId1,myId2]:={}"
        tokens = lexer.process(lineStr)
        tokens2 = lexer.process("myId1,myId2", withEOL=False)
        tokens1 = lexer.process("myId2", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onIdentifiers", [(tokens1, "fullidentifier"), (tokens2, "fullidentifier COMMA identifiers")], "two identifiers")
        lineStr = "[ myId1, myId2, myId3 ] := { }"
        tokens = lexer.process(lineStr)
        tokens3 = lexer.process("myId1, myId2, myId3", withEOL=False)
        tokens2 = lexer.process("myId2, myId3", withEOL=False)
        tokens1 = lexer.process("myId3", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onIdentifiers", [(tokens1, "fullidentifier"), (tokens2, "fullidentifier COMMA identifiers"), (tokens3, "fullidentifier COMMA identifiers")], "three identifiers")
        lineStr = "[ myId1.prop1.prop2, myId2.prop, myId3 ] := { }"
        tokens = lexer.process(lineStr)
        tokens3 = lexer.process("myId1.prop1.prop2, myId2.prop, myId3", withEOL=False)
        tokens2 = lexer.process("myId2.prop, myId3", withEOL=False)
        tokens1 = lexer.process("myId3", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onIdentifiers", [(tokens1, "fullidentifier"), (tokens2, "fullidentifier COMMA identifiers"), (tokens3, "fullidentifier COMMA identifiers")], "three identifiers with variable-length properties")

        lineStr = "[myId1, 2, myId3] := {}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "a non-identifier (number) in a list of identifiers")
        lineStr = "[myId1, myId2,,myId3] := {}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "extra commas in a list of identifiers")

        lineStr = "objName := {}"
        tokens = lexer.process(lineStr)
        nameTokens = lexer.process("objName", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onLeftAlias", [(nameTokens, "fullidentifier")], "single identifier")
        lineStr = "[obj1, obj2, obj3] := {}"
        tokens = lexer.process(lineStr)
        nameTokens = lexer.process("[obj1, obj2, obj3]", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onLeftAlias", [(nameTokens, "BRACKET_OPEN identifiers BRACKET_CLOSE")], "multiple identifiers")
        lineStr = "[onlyOneObj] := {}"
        tokens = lexer.process(lineStr)
        nameTokens = lexer.process("[onlyOneObj]", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onLeftAlias", [(nameTokens, "BRACKET_OPEN identifiers BRACKET_CLOSE")], "single identifier in list of identifiers")

        lineStr = "[] := {}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "no identifiers in identifier list")
        lineStr = "[[id1, id2] := {}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "extra open brace in identifier list")

        lineStr = "template() := {}"
        tokens = lexer.process(lineStr)
        templateTokens = lexer.process("template()", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onLeftAliasTemp", [(templateTokens, "fullidentifier PAREN_OPEN PAREN_CLOSE")], "template alias with no arguments")
        lineStr = "template(arg1) := {}"
        tokens = lexer.process(lineStr)
        templateTokens = lexer.process("template(arg1)", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onLeftAliasTemp", [(templateTokens, "fullidentifier PAREN_OPEN identifiers PAREN_CLOSE")], "template alias with one argument")
        lineStr = "template(arg1, arg2) := {}"
        tokens = lexer.process(lineStr)
        templateTokens = lexer.process("template(arg1, arg2)", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onLeftAliasTemp", [(templateTokens, "fullidentifier PAREN_OPEN identifiers PAREN_CLOSE")], "template alias with two arguments")
        lineStr = "template(arg1, arg2, arg3) := {}"
        tokens = lexer.process(lineStr)
        templateTokens = lexer.process("template(arg1, arg2, arg3)", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onLeftAliasTemp", [(templateTokens, "fullidentifier PAREN_OPEN identifiers PAREN_CLOSE")], "template alias with three arguments")

        lineStr = "template(() := {}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "extra open-parenthesis in template statement")

        lineStr = "objName := (merge1) {}"
        tokens = lexer.process(lineStr)
        mergeTokens = lexer.process("(merge1)", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onInherits", [(mergeTokens, "PAREN_OPEN identifiers PAREN_CLOSE")], "inherit statement with one parent")
        lineStr = "objName := (merge1, merge2) {}"
        tokens = lexer.process(lineStr)
        mergeTokens = lexer.process("(merge1, merge2)", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onInherits", [(mergeTokens, "PAREN_OPEN identifiers PAREN_CLOSE")], "inherit statement with two parents")
        lineStr = "objName := (merge1, merge2, merge3) {}"
        tokens = lexer.process(lineStr)
        mergeTokens = lexer.process("(merge1, merge2, merge3)", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onInherits", [(mergeTokens, "PAREN_OPEN identifiers PAREN_CLOSE")], "inherit statement with three parents")

        lineStr = "objName := (merge1, (merge2) {}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "extra open-parenthesis in inherit statement")
        Tester.stopIfFailed()

        # test top/recursive productions
        # (relation, expression, evaluation, expressions, value (with units),
        # unit <relies on value>, operation-s, rightalias <relies on alias>, rightaliastemp <relies on alias>,
        # objectdeclaration <relies on rightalias>, objectparameters <relies on objectdeclaration>)
        lineStr = "a + b = c + d"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a + b = c + d", withEOL=True) # start uses EOL
        testInspectPerforms(tokens, lineStr, "onStart", [(expTokens, "relation EOL")], "start -> relation")
        lineStr = "a + b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a + b", withEOL=True) # start uses EOL
        testInspectPerforms(tokens, lineStr, "onStart", [(expTokens, "expression EOL")], "start -> expression")
        lineStr = "someAlias := {}"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("someAlias := {}", withEOL=True) # start uses EOL
        testInspectPerforms(tokens, lineStr, "onStart", [(expTokens, "alias EOL")], "start -> alias")
        lineStr = "!forget all the things"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("!forget all the things", withEOL=True) # start uses EOL
        testInspectPerforms(tokens, lineStr, "onStart", [(expTokens, "command EOL")], "start -> command")
        lineStr = ""
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("", withEOL=True) # start uses EOL
        testInspectPerforms(tokens, lineStr, "onStart", [(expTokens, "EOL")], "start -> EOL")
        
        lineStr = "a = b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a = b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRelation", [(expTokens, "expression EQUALS expression")], "simple two-var relationship")
        lineStr = "a + b = c + d"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a + b = c + d", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRelation", [(expTokens, "expression EQUALS expression")], "simple four-var relationship")
        
        lineStr = "a = b = c"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "triple-relation/two equals")

        lineStr = "a + b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a + b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onExpression", [(expTokens, "operationlow")], "simple two-var expression")
        lineStr = "a - b + c * d / e ^ f - g + h / a * j / l + 4 - 5 ^ 7"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a - b + c * d / e ^ f - g + h / a * j / l + 4 - 5 ^ 7", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onExpression", [(expTokens, "operationlow")], "multi-var multi-operation expression")
        lineStr = "a + -b * -c / -d - -e"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a + -b * -c / -d - -e", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onExpression", [(expTokens, "operationlow")], "can process negatives correctly")
        lineStr = "--a - --b + - --- -c"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("--a - --b + - --- -c", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onExpression", [(expTokens, "operationlow")], "can process repeating negatives correctly")
        
        lineStr = "a - + b"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "double operation (sub - add)")
        lineStr = "a * / b"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "double operation (mul - div)")

        lineStr = "a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onEvaluation", [(expTokens, "value")], "single variable evaluation")
        lineStr = "5"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("5", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onEvaluation", [(expTokens, "value")], "single number evaluation")
        lineStr = "2E+8"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("2E+8", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onEvaluation", [(expTokens, "value")], "single E-number evaluation")
        lineStr = "(expression)"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("(expression)", withEOL=False)
        inner = lexer.process("expression", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onEvaluation", [(inner, "value"), (expTokens, "PAREN_OPEN expression PAREN_CLOSE")], "evaluation on a pair of parenthesis")
        lineStr = "[45]"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("[45]", withEOL=False)
        inner = lexer.process("45", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onEvaluation", [(inner, "value"), (expTokens, "BRACKET_OPEN expression BRACKET_CLOSE")], "evaluation on a pair of brackets")
        lineStr = "a * (b + c)"
        tokens = lexer.process(lineStr)
        eval1 = lexer.process("a", withEOL=False)
        eval2 = lexer.process("b", withEOL=False)
        eval3 = lexer.process("c", withEOL=False)
        eval4 = lexer.process("(b + c)", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onEvaluation", [(eval1, "value"), (eval2, "value"), (eval3, "value"), (eval4, "PAREN_OPEN expression PAREN_CLOSE")], "(low-key order-ops test)")

        # TODO: PARSER NO LONGER SUPPORTS TEMPLATE CALLS
        # lineStr = "a(b)"
        # tokens = lexer.process(lineStr)
        # expTokens = lexer.process("b", withEOL=False)
        # testInspectPerforms(tokens, lineStr, "onExpressions", [(expTokens, "expression")], "can process a single argument")
        # lineStr = "a(b, c)"
        # tokens = lexer.process(lineStr)
        # expTokens1 = lexer.process("c", withEOL=False)
        # expTokens2 = lexer.process("b, c", withEOL=False)
        # testInspectPerforms(tokens, lineStr, "onExpressions", [(expTokens1, "expression"), (expTokens2, "expression COMMA expressions")], "can process multiple singular arguments")
        # lineStr = "a(b + c, d * e - f)"
        # tokens = lexer.process(lineStr)
        # expTokens1 = lexer.process("d * e - f", withEOL=False)
        # expTokens2 = lexer.process("b + c, d * e - f", withEOL=False)
        # testInspectPerforms(tokens, lineStr, "onExpressions", [(expTokens1, "expression"), (expTokens2, "expression COMMA expressions")], "can process multiple complex arguments")
        lineStr = "a(b)"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "template call evaluation")
        lineStr = "a()"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "template call evaluation")
        lineStr = "a(b, c, d, e)"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "template call evaluation")

        lineStr = "var<unit>"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("var<unit>", withEOL=False)
        unitTokens = lexer.process("unit", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(unitTokens, "fullidentifier"), (expTokens, "fullidentifier unit")], "identifier with units")
        lineStr = "50<unit>"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("50<unit>", withEOL=False)
        unitTokens = lexer.process("unit", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(unitTokens, "fullidentifier"), (expTokens, "number unit")], "number with units")
        lineStr = ".8e-6<unit>"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process(".8e-6<unit>", withEOL=False)
        unitTokens = lexer.process("unit", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onValue", [(unitTokens, "fullidentifier"), (expTokens, "number unit")], "E-number with units")
        # TODO: PARSER NO LONGER SUPPORTS TEMPLATE CALLS
        # lineStr = "a(b, c)"
        # tokens = lexer.process(lineStr)
        # eval1 = lexer.process("b", withEOL=False)
        # eval2 = lexer.process("c", withEOL=False)
        # eval3 = lexer.process("a(b, c)", withEOL=False)
        # testInspectPerforms(tokens, lineStr, "onValue", [(eval1, "fullidentifier"), (eval2, "fullidentifier"), (eval3, "fullidentifier PAREN_OPEN expressions PAREN_CLOSE")], "can evaluate template arguments (single identifiers)")
        # lineStr = "a(b + c, d)"
        # tokens = lexer.process(lineStr)
        # eval1 = lexer.process("b", withEOL=False)
        # eval2 = lexer.process("c", withEOL=False)
        # eval3 = lexer.process("d", withEOL=False)
        # eval4 = lexer.process("a(b + c, d)", withEOL=False)
        # testInspectPerforms(tokens, lineStr, "onValue", [(eval1, "fullidentifier"), (eval2, "fullidentifier"), (eval3, "fullidentifier"), (eval4, "fullidentifier PAREN_OPEN expressions PAREN_CLOSE")], "can evaluate template arguments (with expressions)")
        # lineStr = "a()"
        # tokens = lexer.process(lineStr)
        # expTokens = lexer.process("a()", withEOL=False)
        # testInspectPerforms(tokens, lineStr, "onValue", [(expTokens, "fullidentifier PAREN_OPEN PAREN_CLOSE")], "can evaluate templates (with no arguments)")

        lineStr = "var<unit>"
        tokens = lexer.process(lineStr)
        unitTokens = lexer.process("<unit>", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onUnit", [(unitTokens, "CARROT_LEFT expression CARROT_RIGHT")], "identifier with units")
        lineStr = "50<unit>"
        tokens = lexer.process(lineStr)
        unitTokens = lexer.process("<unit>", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onUnit", [(unitTokens, "CARROT_LEFT expression CARROT_RIGHT")], "number with units")
        lineStr = ".8e-6<unit>"
        tokens = lexer.process(lineStr)
        unitTokens = lexer.process("<unit>", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onUnit", [(unitTokens, "CARROT_LEFT expression CARROT_RIGHT")], "E-number with units")

        lineStr = "a + b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a + b", withEOL=False)
        varTokens = lexer.process("b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationLow", [(varTokens, "operationmid"), (expTokens, "operationmid operatorlow operationlow")], "addition")
        lineStr = "a - b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a - b", withEOL=False)
        varTokens = lexer.process("b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationLow", [(varTokens, "operationmid"), (expTokens, "operationmid operatorlow operationlow")], "subtraction")
        lineStr = "a * b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a * b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationLow", [(expTokens, "operationmid")], "multiplication")
        lineStr = "a / b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a / b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationLow", [(expTokens, "operationmid")], "division")
        lineStr = "a ^ b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a ^ b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationLow", [(expTokens, "operationmid")], "exponentiation")
        lineStr = "-a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("-a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationLow", [(expTokens, "operationmid")], "negation")
        lineStr = "a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationLow", [(expTokens, "operationmid")], "regular variable")

        lineStr = "a * b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a * b", withEOL=False)
        varTokens = lexer.process("b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationMid", [(varTokens, "operationhigh"), (expTokens, "operationhigh operatormid operationmid")], "multiplication")
        lineStr = "a / b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a / b", withEOL=False)
        varTokens = lexer.process("b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationMid", [(varTokens, "operationhigh"), (expTokens, "operationhigh operatormid operationmid")], "division")
        lineStr = "a ^ b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a ^ b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationMid", [(expTokens, "operationhigh")], "exponentiation")
        lineStr = "-a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("-a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationMid", [(expTokens, "operationhigh")], "negation")
        lineStr = "a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationMid", [(expTokens, "operationhigh")], "regular variable")

        lineStr = "a ^ b"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a ^ b", withEOL=False)
        varTokens = lexer.process("b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationHigh", [(varTokens, "operationmax"), (expTokens, "operationmax operatorhigh operationhigh")], "exponentiation")
        lineStr = "-a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("-a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationHigh", [(expTokens, "operationmax")], "negation")
        lineStr = "a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationHigh", [(expTokens, "operationmax")], "regular variable")

        lineStr = "a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationMax", [(expTokens, "evaluation")], "regular variable")
        lineStr = "-a"
        tokens = lexer.process(lineStr)
        expTokens = lexer.process("-a", withEOL=False)
        insideTokens = lexer.process("a", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onOperationMax", [(insideTokens, "evaluation"), (expTokens, "DASH operationmax")], "negation")

        lineStr = "obj := {}"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("{}", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAlias", [(rightTokens, "objectdeclaration")], "object declaration - only (empty object)")
        lineStr = "obj := (merge1) {}"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("(merge1) {}", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAlias", [(rightTokens, "inherits objectdeclaration")], "object declaration - merge syntax (empty object)")
        lineStr = "val := 4 + 5"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("4 + 5", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAlias", [(rightTokens, "expression")], "expression (single operator)")
        lineStr = "a := [1, 2, 3]"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("[1, 2, 3]", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAlias", [(rightTokens, "BRACKET_OPEN expressions BRACKET_CLOSE")], "single variable list assignment")
        lineStr = "[a, b, c] := [-1,4,x,-y]"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("[-1,4,x,-y]", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAlias", [(rightTokens, "BRACKET_OPEN expressions BRACKET_CLOSE")], "multi variable list assignment")

        lineStr = "obj := () {}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "an empty inheriting list")
        lineStr = "[a, b, c] := []"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "no values in value list")

        lineStr = "template() := a = b"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("a = b", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAliasTemp", [(rightTokens, "relation")], "relation declaration")
        lineStr = "template() := a + b + c"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("a + b + c", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAliasTemp", [(rightTokens, "expression")], "multiple-operation expression")
        lineStr = "template() := !list things"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("!list things", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAliasTemp", [(rightTokens, "command")], "command declaration")
        lineStr = "template() := {a = b, c = d}"
        tokens = lexer.process(lineStr)
        rightTokens = lexer.process("{a = b, c = d}", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onRightAliasTemp", [(rightTokens, "objectdeclaration")], "multiple-expression object declaration")
        
        lineStr = "obj := {}"
        tokens = lexer.process(lineStr)
        objTokens = lexer.process("{}", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onObjectDeclaration", [(objTokens, "BRACE_OPEN BRACE_CLOSE")], "empty object declaration")
        lineStr = "obj := {a = b}"
        tokens = lexer.process(lineStr)
        objTokens = lexer.process("{a = b}", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onObjectDeclaration", [(objTokens, "BRACE_OPEN objectparameters BRACE_CLOSE")], "single expression")
        lineStr = "obj := {a = b, c = d}"
        tokens = lexer.process(lineStr)
        objTokens = lexer.process("{a = b, c = d}", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onObjectDeclaration", [(objTokens, "BRACE_OPEN objectparameters BRACE_CLOSE")], "two expressions")
        lineStr = "obj := {a := 4, b = a + 1}"
        tokens = lexer.process(lineStr)
        objTokens = lexer.process("{a := 4, b = a + 1}", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onObjectDeclaration", [(objTokens, "BRACE_OPEN objectparameters BRACE_CLOSE")], "alias and expression")
        
        lineStr = "obj := {a = b, b = c}"
        tokens = lexer.process(lineStr)
        rel2 = lexer.process("a = b, b = c", withEOL=False)
        rel1 = lexer.process("b = c", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onObjectParameters", [(rel1, "relation"), (rel2, "relation COMMA objectparameters")], "can process lists of relations (two)")
        lineStr = "obj := {a = a, b = b, c = c, d = d}"
        tokens = lexer.process(lineStr)
        rel4 = lexer.process("a = a, b = b, c = c, d = d", withEOL=False)
        rel3 = lexer.process("b = b, c = c, d = d", withEOL=False)
        rel2 = lexer.process("c = c, d = d", withEOL=False)
        rel1 = lexer.process("d = d", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onObjectParameters", [(rel1, "relation"), (rel2, "relation COMMA objectparameters"), (rel3, "relation COMMA objectparameters"), (rel4, "relation COMMA objectparameters")], "can process lists of relations (four)")
        lineStr = "obj := {a := 4, b = 5, c := b, d := 7}"
        tokens = lexer.process(lineStr)
        rel4 = lexer.process("a := 4, b = 5, c := b, d := 7", withEOL=False)
        rel3 = lexer.process("b = 5, c := b, d := 7", withEOL=False)
        rel2 = lexer.process("c := b, d := 7", withEOL=False)
        rel1 = lexer.process("d := 7", withEOL=False)
        testInspectPerforms(tokens, lineStr, "onObjectParameters", [(rel1, "alias"), (rel2, "alias COMMA objectparameters"), (rel3, "relation COMMA objectparameters"), (rel4, "alias COMMA objectparameters")], "can process lists of relations AND aliases")
        
        lineStr = "obj := {e = e, a + b, g = h}"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "object declaration with non-relation")
        Tester.stopIfFailed()

        # other edge-casey tests
        lineStr = "this is invalid"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "multiple identifiers in a row (no period)")
        lineStr = ":= no + alias + identifier"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "missing identifier on left side of alias")
        lineStr = "= b"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "missing expression on left side of relation")
        lineStr = "a = "
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.EOLError, "missing expression on right side of relation")
        lineStr = "a + "
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.EOLError, "missing expression on right side of operation (addition)")
        lineStr = "a / "
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.EOLError, "missing expression on right side of operation (division)")
        lineStr = "a ^ "
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.EOLError, "missing expression on right side of operation (exponentiation)")
        lineStr = "(a + b))"
        tokens = lexer.process(lineStr)
        testInspectErrors(tokens, lineStr, parser.ParseError, "extra parenthesis token at end")
        Tester.stopIfFailed()

        # test errors have correct messages
        lineStr = "a + b + "
        tokens = lexer.process(lineStr)
        try:
            parser.inspect(tokens, lineStr)
        except parser.EOLError as e:
            expectedMessage = "           ^\n    EOLError: Unexpected end of line"
            Tester.assertEqual(e.message, expectedMessage, "EOLError has correct printout")

        lineStr = "something + else BAD + more * things"
        tokens = lexer.process(lineStr)
        try:
            parser.inspect(tokens, lineStr)
        except parser.ParseError as e:
            expectedMessage = "                    ^^^\n    ParseError: Unexpected token BAD; expected EOL"
            Tester.assertEqual(e.message, expectedMessage, "ParseError has correct printout")
        Tester.stopIfFailed()

    @classmethod
    def Solver(cls):
        def dictIncludes(someDict, includesKeysAndValues):
            for (includeKey, includeVal) in includesKeysAndValues.items():
                if includeKey not in someDict:
                    return False
                someVal = someDict[includeKey]
                roundedValIfNumeric = round(someVal, 100) if isNumeric(someVal) else someVal
                if roundedValIfNumeric != includeVal:
                    return False
            return True

        def conditionsMatch(subDict1, subDict2):
            if type(subDict1) is not SubDict:
                subDict1 = SubDict(subDict1)
            if type(subDict2) is not SubDict:
                subDict2 = SubDict(subDict2)

            return subDict1.conditions == subDict2.conditions

        def testSolver(relations, expectedSolutions, testName, *args, allSolutionsProvided=None):
            if allSolutionsProvided is None:
                raise TypeError("testSolver() requires the 'allSolutionsProvided' boolean keyword argument")
            
            if isinstance(expectedSolutions, dict):
                expectedSolutions = SubDictList([SubDict(expectedSolutions)])
            elif type(expectedSolutions) in (tuple, list, set):
                expectedSolutions = SubDictList(SubDict(dictLike) for dictLike in expectedSolutions)

            # TODO: all combinations need to be done here
            # (except how do I manupulate aht vars get evaluated first?)
            solver = Solver()
            solver.extractGivenRelationalNumerics(relations)
            solver.solveSymbolsByBackSubstitution()
            solutions = solver.solutions

            for expectedContainsDict in expectedSolutions:
                solutionsContainsMatch = any(
                    dictIncludes(subDict, expectedContainsDict)
                    for subDict in solutions
                )
                if not solutionsContainsMatch:
                    # should always fail; just provides a useful error message
                    Tester.assertIn(expectedContainsDict, solutions, testName)
            if allSolutionsProvided:
                Tester.assertEqual(len(expectedSolutions), len(solutions), testName)
            elif len(expectedSolutions) != len(solutions):
                print("WARNING! Solver() len-tests did not match (but strict mode was not enabled...)")
                print("    solutions keys: {}".format([tuple(solution.keys()) for solution in solutions]))
                print("    exp sols keys:  {}".format([tuple(expDict.keys()) for expDict in expectedSolutions]))
            Tester.stopIfFailed()

        # a + b = 4
        # a - b = 2
        # (a = 3)
        # (b = 1)
        (a, b) = sympy.symbols("a, b")
        testSolver(
            [
                a + b  -  4,
                a - b  -  2,
            ],
            [{
                a: 3,
                b: 1,
            }],
            "basic two-relation two-variable one-universe system",
            allSolutionsProvided=True,
        )
        Tester.stopIfFailed()

        # a + 2b + c = 20
        # 2a + c = 14
        # b - a = 1
        # (a = 4)
        # (b = 5)
        # (c = 6)
        (a, b, c) = sympy.symbols("a, b, c")
        testSolver(
            [
                a + 2*b + c  -  20,
                2*a + c  -  14,
                b - a  -  1,
            ],
            [{
                a: 4,
                b: 5,
                c: 6,
            }],
            "basic three-relation three-variable one-universe system",
            allSolutionsProvided=True
        )
        Tester.stopIfFailed()

        # a + b = 6
        # b - c = 7
        # c = d - 8
        # d + e = -7
        # e + a = -10
        # (a = 2)
        # (b = 4)
        # (c = -3)
        # (d = 5)
        # (e = -12)
        (a, b, c, d, e) = sympy.symbols("a, b, c, d, e")
        testSolver(
            [
                a + b  -  6,
                b - c  -  7,
                c  -  (d - 8),
                d + e  -  -7,
                e + a  -  -10,
            ],
            [{
                a: 2,
                b: 4,
                c: -3,
                d: 5,
                e: -12,
            }],
            "five-variable chained relations system",
            allSolutionsProvided=True,
        )
        Tester.stopIfFailed()

        # ab = 8
        # b = 2a
        # (a = -2, 2)
        # (b = -4, 4)
        (a, b) = sympy.symbols("a, b")
        testSolver(
            [
                a*b  -  8,
                b  -  2*a,
            ],
            [{
                a: 2,
                b: 4,
            }, {
                a: -2,
                b: -4,
            }],
            "two-variable two-universe multiplication system",
            allSolutionsProvided=True
        )
        Tester.stopIfFailed()

        # f = m * a
        # f = 80
        # m = 20
        # (a = 4)
        (f, m, a) = sympy.symbols("f, m, a")
        testSolver(
            [
                f  -  m*a,
                f - 80,
                m - 20,
            ],
            [{
                f: 80,
                m: 20,
                a: 4,
            }],
            "system with one variable-only relation",
            allSolutionsProvided=True,
        )
        Tester.stopIfFailed()

        # k1i = 1/2 * m1 * v1i^2
        # k2i = 1/2 * m2 * v2i^2
        # k1f = 1/2 * m1 * v1f^2
        # k2f = 1/2 * m2 * v2f^2
        # kt = k1i + k2i
        # kt = k1f + k2f
        # m1 = 10
        # m2 = 16
        # v1i = 15
        # v2i = 10
        # v1f = 5
        # (v2f = 15)
        # (k1i = 1125)
        # (k2i = 800)
        # (kt = 1925)
        # (k1f = 125)
        # (k2f = 1800)

        (m1, m2, v1i, v2i, v1f, v2f, k1i, k2i, k1f, k2f, kt) = sympy.symbols("m1, m2, v1i, v2i, v1f, v2f, k1i, k2i, k1f, k2f, kt")
        testSolver(
            [
                k1i  -  1/2 * m1 * v1i**2,
                k2i  -  1/2 * m2 * v2i**2,
                k1f  -  1/2 * m1 * v1f**2,
                k2f  -  1/2 * m2 * v2f**2,
                kt  -  (k1i + k2i),
                kt  -  (k1f + k2f),
                m1 - 10,
                m2 - 16,
                v1i - 15,
                v2i - 10,
                v1f - 5,
            ],
            [{
                m1: 10,
                m2: 16,
                v1i: 15,
                v2i: 10,
                v1f: 5,
                v2f: 15,
                k1i: 1125,
                k2i: 800,
                k1f: 125,
                k2f: 1800,
                kt: 1925,
            }, {
                m1: 10,
                m2: 16,
                v1i: 15,
                v2i: 10,
                v1f: 5,
                v2f: -15, # the only thing different (because ±√(2*k2f/m2))
                k1i: 1125,
                k2i: 800,
                k1f: 125,
                k2f: 1800,
                kt: 1925,
            }],
            "system with multiple variable-only relations",
            allSolutionsProvided=True
        )
        Tester.stopIfFailed()

        # a + c = b
        # d - b = c
        # 2*c = d - a
        # a * 10 = d * 4
        # (a = 4)
        # (b = 7)
        # (c = 3)
        # (d = 10)
        (a, b, c, d) = sympy.symbols("a, b, c, d")
        testSolver(
            [
                a + c  -  b,
                d - b  -  c,
                2*c  -  (d - a),
                a*10  -  d*4,
            ],
            [{
                a: 4,
                b: 7,
                c: 3,
                d: 10,
            }],
            "system with relations missing numerics",
            allSolutionsProvided=False
        )
        Tester.stopIfFailed()

        # a + b = -c
        # b * b + c = a
        # b + b - a = -c
        # (a = 1)
        # (b = 2)
        # (c = -3)
        (a, b, c) = sympy.symbols("a, b, c")
        testSolver(
            [
                a + b  +  c,
                b * b + c  -  a,
                b + b - a  +  c,
            ],
            [{
                a: 1,
                b: 2,
                c: -3,
            }],
            "system with all relations missing numerics (and one positive-only)",
            allSolutionsProvided=False
        )
        Tester.stopIfFailed()
    
    @classmethod
    def Interpreter(cls):
        state = dict()
        def fakePrint(*args, **kwargs):
            argsStrs = map(str, args)
            sep = kwargs.get('sep')
            sep = sep if sep is not None else ' '
            end = kwargs.get('end')
            end = end if end else '\n'
            state["fakePrintResults"].append(sep.join(argsStrs))
            state["lastPrints"] = end.join(state["fakePrintResults"])
        def resetState():
            state["interpreter"] = Interpreter(fakePrint)
            state["lastPrints"] = None
            state["fakePrintResults"] = []
        resetState()
        def testLineOnInterpreter(line, expectedOutput, testName):
            state["lastPrints"] = ""
            state["fakePrintResults"] = []
            state["interpreter"].executeLine(line)
            if expectedOutput is None:
                Tester.assertEqual("", state["lastPrints"], testName)
            elif type(expectedOutput) is str:
                Tester.assertEqual(INDENT + expectedOutput, state["lastPrints"], testName)
            else:
                for expected in expectedOutput:
                    if not Tester.assertIn(expected, state["lastPrints"], testName):
                        break

        # test interpreter prints errors
        testLineOnInterpreter(
            "+",
            ("nexpected token +",),
            "prints unexpected token errors (lone plus)",
        )
        resetState()
        testLineOnInterpreter(
            "this is a test",
            ("nexpected token is",),
            "prints unexpected token errors (double identifiers)",
        )
        resetState()
        testLineOnInterpreter(
            "a + b * ",
            ("nexpected end of line",),
            "prints unexpected token errors (EOL/missing identifier)",
        )
        resetState()
        Tester.stopIfFailed()

        # ensure basic aliases work
        testLineOnInterpreter(
            "val := 4",
            None,
            "stores values in alias form (regular numbers)",
        )
        testLineOnInterpreter(
            "otherVal := 8E16",
            None,
            "stores values in alias form (E-numbers)",
        )
        Tester.stopIfFailed()

        # test printing values
        testLineOnInterpreter(
            "val",
            "4",
            "can print previously assigned variables (regular numbers)",
        )
        testLineOnInterpreter(
            "otherVal",
            "8e+16",
            "can print previously assigned variables (E-numbers)",
        )
        testLineOnInterpreter(
            "80000000000000000",
            "8e+16",
            "converts ints to e-number floats",
        )
        Tester.stopIfFailed()
        
        testLineOnInterpreter(
            "someUndefinedVal",
            (ISP + "^^^^^^^^^^^^^^^^\n", "rror", "ndefined"),
            "errors when trying to access undefined values",
        )
        testLineOnInterpreter(
            "a := 4 + b",
            (ISP + "         ^\n", "rror", "ndefined"),
            "errors when trying to access undefined values",
        )
        testLineOnInterpreter(
            "a := b + cc",
            (ISP + "     ^   ^^\n", "rror", "ndefined"),
            "errors when trying to access undefined values (printing arrows for all undefined values)",
        )
        resetState()
        Tester.stopIfFailed()

        # test expression evaluation
        testLineOnInterpreter(
            "1 + 2 * 3",
            "7",
            "can evaluate expressions (no variables)",
        )
        testLineOnInterpreter(
            "(.1 + .2) * -10",
            "-3",
            "can evaluate expressions with floats (and return ints)",
        )
        testLineOnInterpreter(
            "val := 6 + 3",
            None,
            "stores values from expressions in alias form",
        )
        testLineOnInterpreter(
            "val * 2 + 4",
            "22",
            "can evaluate expressions (with variables)",
        )
        testLineOnInterpreter(
            "val := val + 1",
            None,
            "can change values relative to previous values",
        )
        testLineOnInterpreter(
            "val",
            "10",
            "can print new values",
        )
        testLineOnInterpreter(
            "-val---5 +-3",
            "-18",
            "can process negative (and double negative) values",
        )
        testLineOnInterpreter(
            "val + 5 * 3",
            "25",
            "correctly performs order of operations (no evaluations)",
        )
        testLineOnInterpreter(
            "[val + 5] * 3",
            "45",
            "correctly performs order of operations (with evaluations)",
        )
        testLineOnInterpreter(
            "-[val - 1]",
            "-9",
            "correctly performs negation on evaluations",
        )
        testLineOnInterpreter(
            "2 ^ 2 ^ 3",
            "256",
            "correctly evaluates exponents right-to-left",
        )
        testLineOnInterpreter(
            "2 - 1 + 4",
            "5",
            "correctly evaluates other expressions left-to-right",
        )
        resetState()
        Tester.stopIfFailed()

        # test comments
        testLineOnInterpreter(
            "val := 2 # comment",
            None,
            "can set values with comments",
        )
        testLineOnInterpreter(
            "val # comment",
            "2",
            "can print stored values with comments",
        )
        testLineOnInterpreter(
            "val * 7 # comment",
            "14",
            "can evaluate expressions with comments",
        )
        resetState()
        Tester.stopIfFailed()

        # test template aliases (creation only)
        testLineOnInterpreter(
            "alias() := 4",
            None,
            "can create template aliases (empty)",
        )
        resetState()
        testLineOnInterpreter(
            "alias(x) := x",
            None,
            "can create template aliases (one argument)",
        )
        testLineOnInterpreter(
            "alias(x, y) := x + y",
            None,
            "can create/override template aliases (two arguments)",
        )
        testLineOnInterpreter(
            "alias(x, y) := x + 5",
            ("nused", "ariable ", "y"),
            "prints a warning for unused template variables (one unused)",
        )
        testLineOnInterpreter(
            "alias(x, y) := 4 + 5",
            ("nused", "ariables", "x", "y"),
            "prints a warning for unused template variables (two unused)",
        )
        testLineOnInterpreter(
            "myTemplate := alias",
            None,
            "can assign aliases to template aliases",
        )
        testLineOnInterpreter(
            "myTemplate := myTemplate",
            None,
            "can assign aliases to self",
        )
        testLineOnInterpreter(
            "myTemplate",
            "(x, y) -> 4 + 5",
            "can print template aliases (through another alias)",
        )
        testLineOnInterpreter(
            "sqrt(x) := x^.5",
            None,
            "can create sqrt alias",
        )
        testLineOnInterpreter(
            "myTemplate() := sqrt(global)",
            None,
            "can reassign any variable (in this case, a previous alias)",
        )
        testLineOnInterpreter(
            "alias",
            "(x, y) -> 4 + 5",
            "overwriting myTemplate (previously an alias for 'alias') does not change 'alias'",
        )
        testLineOnInterpreter(
            "myTemplate",
            "() -> sqrt(global)",
            "does not replace 'sqrt(' in instances of referencing definitions",
        )
        testLineOnInterpreter(
            "4 + myTemplate",
            (ISP + "    ^^^^^^^^^^\n", "rror", "xpression", "ariable"),
            "errors when trying to operate on template reference",
        )
        testLineOnInterpreter(
            "myTemplate ^ 2",
            (ISP + "^^^^^^^^^^\n", "rror", "xpression", "ariable"),
            "errors when trying to operate on template reference (template first)",
        )
        testLineOnInterpreter(
            "-myTemplate",
            (ISP + " ^^^^^^^^^^\n", "rror", "xpression", "ariable"),
            "errors when trying to negate a template reference",
        )
        resetState()
        Tester.stopIfFailed()

        # test relation creation/evaluation
        testLineOnInterpreter(
            "a = b + c",
            None,
            "can create basic relations",
        )
        testLineOnInterpreter(
            "b = d * e",
            None,
            "can create multiple relations",
        )
        testLineOnInterpreter(
            "a",
            "b + c",
            "can print previous relations (in simplest form; b is not substituted)",
        )
        testLineOnInterpreter(
            "c",
            "a - b",
            "can print relations based on solutions for variables (again, in simplest form)",
        )
        testLineOnInterpreter(
            "b",
            ("a - c", "d*e"),
            "can print multiple solutions for one variable",
        )
        testLineOnInterpreter(
            "e := 2",
            None,
            "can store values with relations in the database",
        )
        testLineOnInterpreter(
            "b",
            ("a - c", "2*d"),
            "can print up-to-date solutions for one variable",
        )
        testLineOnInterpreter("a := 4", None, "can store more variables after relations are created")
        testLineOnInterpreter("c := 2", None, "can store more variables after relations are created")
        testLineOnInterpreter("d := 1", None, "can store more variables after relations are created")
        testLineOnInterpreter(
            "b",
            "2",
            "prints a single solution for variables in multiple (non-contradictory) relations",
        )
        resetState()
        testLineOnInterpreter(
            "b^2 = a",
            None,
            "can relate variables with exponentiation",
        )
        testLineOnInterpreter(
            "a",
            "b^2",
            "correctly replaces '**' with '^'",
        )
        testLineOnInterpreter(
            "b",
            ("√(a)", "-√(a)"),
            "replaces 'sqrt(' (given by sympy) with '√('",
        )
        testLineOnInterpreter(
            "a := 4",
            None,
            "can store values and exponentiate them",
        )
        testLineOnInterpreter(
            "b",
            ("-2", "or", "2"),
            "can print multiple (complete) solutions for a variable",
        )
        testLineOnInterpreter(
            "-b + 4",
            ("2", "or", "6"),
            "processes all possible values when considering variables",
        )
        resetState()
        testLineOnInterpreter(
            "template() := 4",
            None,
            "can process template alias before relations",
        )
        testLineOnInterpreter(
            "a + b = template()",
            None,
            "can process template aliases in relations",
        )
        testLineOnInterpreter(
            "template() := 2",
            None,
            "can redefine template aliases after being used",
        )
        testLineOnInterpreter(
            "a + b",
            "4",
            "evaluates the template alias when the relation is instantiated, not when the relation is checked (aka result here should not be 2)",
        )
        resetState()
        testLineOnInterpreter(
            "a = b",
            None,
            "can create one-on-one relations",
        )
        testLineOnInterpreter(
            "a - a + a",
            "b",
            "can simplify expressions",
        )
        resetState()
        Tester.stopIfFailed()

        # ensure interpreter catches contradictions
        testLineOnInterpreter(
            "a + b = c",
            None,
            "can create a single relation (no contradictions yet...)",
        )
        testLineOnInterpreter("b := 1", None, "can store variables (no contradictions yet...)")
        testLineOnInterpreter("c := 3", None, "can store variables (no contradictions yet...)")
        testLineOnInterpreter(
            "a",
            "2",
            "can get variables from an otherwise solved relation",
        )
        testLineOnInterpreter(
            "a = b + c",
            ("ontradiction"),
            "can detect new relations that impose contradictions",
        )
        resetState()
        testLineOnInterpreter(
            "a + b = c",
            None,
            "can create a single relation (no contradictions yet... again...)",
        )
        testLineOnInterpreter(
            "a = b + c",
            None,
            "can create a second relation (which would be a contradiction in other circumstances...)",
        )
        testLineOnInterpreter(
            "b",
            "0",
            "infers zero as only possibility for b"
        )
        testLineOnInterpreter(
            "b := 2",
            ("ontradiction"),
            "can detect contradictions between inferred values and new ones"
        )
        testLineOnInterpreter(
            "2 + 2 = 10",
            ("ontradiction"),
            "can detect blatant numerical contradictions"
        )
        resetState()
        Tester.stopIfFailed()

        # test multi-value assignments
        testLineOnInterpreter(
            "a := [1, 2]",
            None,
            "can assign variable to a set of values",
        )
        testLineOnInterpreter(
            "a",
            ("1", "or", "2"),
            "can print variables assigned with multiple values",
        )
        testLineOnInterpreter(
            "a + a",
            ("2", "or", "4"), # not 3!
            "can evaluate expressions using variables with multiple values",
        )
        resetState()
        Tester.stopIfFailed()

        # test single-var relations overwrite, like re-alias
        # (printing a warning, but only once!)
        testLineOnInterpreter(
            "a = 5",
            ("arning", "single", "variable", "alias"),
            "warns on first single-variable relation"
        )
        testLineOnInterpreter(
            "a",
            "5",
            "prints variables assigned from single-var relations"
        )
        # TODO: is this really what we want? How does alias dict work if
        #       a^2 = 4 is given? (a = -2, 2)
        testLineOnInterpreter(
            "a = 60",
            None,
            "doesn't warn on subsequent single-var relations"
        )
        testLineOnInterpreter(
            "a",
            "60",
            "prints variables REassigned from single-var relations"
        )
        resetState()
        testLineOnInterpreter(
            "4 * a = 2 * a + 6",
            ("arning", "single", "variable", "alias"),
            "warns on complicated single-variable relations",
        )
        testLineOnInterpreter(
            "a",
            "3",
            "still solves for variable after warning",
        )
        resetState()
        Tester.stopIfFailed()
        
        # test object creation/evaluation/printing
        # (including printing "known" and "inferred" values)
        testLineOnInterpreter(
            "obj := {f = m * a, m := 4}",
            None,
            "can create objects with relations and initialized values",
        )
        testLineOnInterpreter(
            "obj",
            "{m=4}(f=?, a=?)",
            "can print objects"
        )
        Tester.stopIfFailed()
        # test object properties
        testLineOnInterpreter(
            "obj.f",
            "4*obj.a",
            "prints up-to-date solutions for properties"
        )
        testLineOnInterpreter(
            "obj.force",
            (ISP + "    ^^^^^\n", "rror", "ndefined"),
            "errors when trying to access undefined object properties",
        )
        testLineOnInterpreter(
            "obj.a = 5",
            ("arning", "single", "variable", "alias"),
            "can relate a property not yet initiialized",
        )
        testLineOnInterpreter(
            "obj.f",
            "20",
            "prints up-to-date solutions for properties when solved completely",
        )
        Tester.stopIfFailed()
        # test object sub-objects
        testLineOnInterpreter(
            "obj.sub := {a + b = obj.f, b := 8}",
            None,
            "can define sub-objects",
        )
        testLineOnInterpreter(
            "obj.sub",
            "{b=8}(a=12)",
            "can print sub-objects",
        )
        resetState()
        Tester.stopIfFailed()
        # test using properties in relations
        # (including seeing new "undefined" properties)
        testLineOnInterpreter(
            "obj := {a = 3}",
            ("arning", "single", "variable", "alias"),
            "interpreter should warn when single-var relations are present in objects",
        )
        testLineOnInterpreter(
            "a := 19",
            None,
            "can set variables of the same name as properties",
        )
        testLineOnInterpreter(
            "obj.a",
            "3",
            "can print the correct property",
        )
        testLineOnInterpreter(
            "a",
            "19",
            "can print the correct variable",
        )
        testLineOnInterpreter(
            "obj.f = obj.m * obj.a",
            None,
            "can relate (undefined) object properties outside of object declaration",
        )
        testLineOnInterpreter(
            "obj.m",
            "obj.f/3",
            "can print up-to-date solutions after relating outside of declaration",
        )
        testLineOnInterpreter(
            "obj.m := 6",
            None,
            "can set object variables outside of declarations through alias-syntax",
        )
        testLineOnInterpreter(
            "obj.f",
            "18",
            "can print complete solution after relating outside of object declaration",
        )
        resetState()
        Tester.stopIfFailed()
        # test object copying
        testLineOnInterpreter(
            "obj1 := {val := 5}",
            None,
            "can create objects that will be copied later",
        )
        testLineOnInterpreter(
            "copy1 := (obj1)",
            None,
            "can copy objects created earlier",
        )
        testLineOnInterpreter(
            "same1 := obj1",
            None,
            "can alias objects created earlier",
        )
        testLineOnInterpreter(
            "obj1.val := 10",
            None,
            "can reassign object properties after copies/aliases were made",
        )
        testLineOnInterpreter(
            "copy1.val",
            "5",
            "prints old property values (object was copied!)",
        )
        testLineOnInterpreter(
            "same1.val",
            "10",
            "prints same property values (object was aliased!)",
        )
        Tester.stopIfFailed()
        # test object inheritance
        testLineOnInterpreter(
            "obj2 := {special := 3}",
            None,
            "can create multiple objects under different names",
        )
        testLineOnInterpreter(
            "merged1 := (obj1, obj2)",
            None,
            "can merge objects",
        )
        testLineOnInterpreter(
            "merged1",
            "{val=10, special=3}",
            "can print merged objects",
        )
        testLineOnInterpreter(
            "merged2 := (obj2, obj1) {more := 8}",
            None,
            "can make merged objects with a declaration",
        )
        testLineOnInterpreter(
            "merged2",
            "{special=3, val=10, more=8}",
            "can print merged objects created with declarations",
        )
        # test object copying (through array-like syntax)
        testLineOnInterpreter(
            "[same2a, same2b, same2c] := merged2",
            None,
            "can alias many names at once",
        )
        testLineOnInterpreter(
            "[copy2a, copy2b, copy2c] := (merged2)",
            None,
            "can copy to many names at once",
        )
        testLineOnInterpreter(
            "merged2.val := 2",
            None,
            "can change properties after merging",
        )
        testLineOnInterpreter(
            "copy2a.val",
            "10",
            "copied properties do not change (copy-a)",
        )
        testLineOnInterpreter(
            "copy2b.val",
            "10",
            "copied properties do not change (copy-b)",
        )
        testLineOnInterpreter(
            "copy2b.val",
            "10",
            "copied properties do not change (copy-c)",
        )
        testLineOnInterpreter(
            "same2a.val",
            "2",
            "aliased properties change with original (same-a)",
        )
        testLineOnInterpreter(
            "same2b.val := 4",
            None,
            "can change properties from alias",
        )
        testLineOnInterpreter(
            "merged2.val",
            "4",
            "original properties change with alias (same-b)",
        )
        testLineOnInterpreter(
            "same2c.val",
            "4",
            "aliased properties change with original (same-c)",
        )
        resetState()
        Tester.stopIfFailed()

        # ensure template aliases are evaluated properly
        testLineOnInterpreter(
            "add(x, y) := x + y",
            None,
            "can create template aliases",
        )
        testLineOnInterpreter(
            "add(4, 3)",
            "7",
            "correctly evaluates template aliases (with numbers)"
        )
        testLineOnInterpreter(
            "val1 := 5",
            None,
            "can create variables after template aliases",
        )
        testLineOnInterpreter(
            "add(val1, 5)",
            "10",
            "correctly evaluates template aliases (with variables)"
        )
        testLineOnInterpreter(
            "addps(obj1, obj2) := obj1.p + obj2.p",
            None,
            "can create template aliases that utilize object properties",
        )
        testLineOnInterpreter(
            "o1 := {p := 3}",
            None,
            "can create objects different from a template alias parameter name"
        )
        testLineOnInterpreter(
            "obj1 := {p := 2}",
            None,
            "can create objects the same as a template alias parameter name"
        )
        testLineOnInterpreter(
            "addps(o1, obj1)",
            "5",
            "can correctly evaluate template aliases with (not) conflicting names (order 1)"
        )
        testLineOnInterpreter(
            "addps(obj1, o1)",
            "5",
            "can correctly evaluate template aliases with (not) conflicting names (order 2)"
        )
        testLineOnInterpreter(
            "temptemp(a) := a(1, 2) + 3",
            None,
            "can create template aliases that utilize other template aliases",
        )
        testLineOnInterpreter(
            "temptemp(add)",
            "6",
            "can correctly evaluate aliases that utilize template aliases",
        )
        testLineOnInterpreter(
            "obj1(5)",
            (ISP + "^^^^^^^\n", "rror", "valuate", "lias"),
            "errors when trying to evaluate a non-template alias as a template alias",
        )
        testLineOnInterpreter(
            "temptemp.doesnt_have_properties",
            ("ccess", "roperty", "non-object"),
            "errors when trying to access properties of non-object types"
        )
        testLineOnInterpreter(
            "add(4, 5, 6)",
            (ISP + "        ^^^\n", "rror", "rguments"),
            "errors when template alias is given too many arguments",
        )
        testLineOnInterpreter(
            "add(4)",
            (ISP + "     ^\n", "rror", "rguments"),
            "errors when template alias is not given enough arguments",
        )
        testLineOnInterpreter(
            "objtemp() := {}",
            None,
            "can create a template alias that doesn't evaluate to an expression (this test uses an empty object)",
        )
        testLineOnInterpreter(
            "4 + objtemp()",
            (ISP + "    ^^^^^^^^^\n", "rror", "not", "xpression"),
            "Templates that do not return expressions error when used in expressions",
        )
        testLineOnInterpreter(
            "objtemp() * 4 + objtemp()",
            (ISP + "^^^^^^^^^       ^^^^^^^^^\n", "rror", "not", "xpression"),
            "Templates that do not return expressions error when used in expressions (and the interpreter highlights all of them)",
        )
        testLineOnInterpreter(
            "metaTemp() := 1 + objTemp()",
            None,
            "Can create templates that would be invalid if created immediately (objTemp could be redefined)",
        )
        testLineOnInterpreter(
            "metaTemp()",
            (ISP + "^^^^^^^^^^\n", "rror", "not", "xpression"),
            "Templates that return invalid expressions error",
        )
        testLineOnInterpreter(
            "metaTemp() := 1 + metaTemp()",
            (ISP + "                  ^^^^^^^^^^\n", "rror", "ecursive"),
            "Errors when attempting to make recursive template definitions",
        )
        testLineOnInterpreter(
            "metaTemp2() := 1 + metaTemp()",
            None,
            "Doesn't error (yet) when making a non-recursive template call",
        )
        testLineOnInterpreter(
            "metaTemp() := 2 + metaTemp2()",
            (ISP + "                  ^^^^^^^^^^\n", "rror", "ecursive"),
            "Errors when attempting to make recursive template definitions (two-deep)",
        )
        resetState()
        testLineOnInterpreter(
            "setVars(val) := [a, b, c] := val",
            None,
            "Can create template aliases that define aliases",
        )
        testLineOnInterpreter(
            "setVars(4)",
            None,
            "Can use template aliases to define aliases to numbers",
        )
        testLineOnInterpreter(
            "a + b + c",
            "12",
            "Correctly evaluates aliases from template alias (numbers)",
        )
        testLineOnInterpreter(
            "setVars({ val := 6 })",
            None,
            "Can use template aliases to define aliases to objects"
        )
        testLineOnInterpreter(
            "b",
            "{val=6}",
            "Correctly evaluates aliases from template alias (objects)"
        )
        testLineOnInterpreter(
            "a.val := 4",
            None,
            "Can use objects made from a template alias (that makes aliases)",
        )
        testLineOnInterpreter(
            "b",
            "{val=4}",
            "Simultaneous definitions made from a template alias (that makes aliases) work correctly with objects"
        )
        resetState()
        Tester.stopIfFailed()

        # test definitions/inferences with units
        # (aliases, objects, global relations)
        testLineOnInterpreter(
            "distance := 4<m>",
            None,
            "can define variables with values and units",
        )
        testLineOnInterpreter(
            "distance",
            "4m",
            "prints values with their units",
        )
        testLineOnInterpreter(
            "<N> := <kg*m/s^2> ",
            None,
            "can create aliases for units",
        )
        testLineOnInterpreter(
            "obj := {force<kg*m/s^2> := 20<N>}",
            None,
            "can create objects with unit-ful properties (with matching types, given alias works)",
        )
        testLineOnInterpreter(
            "obj.force",
            "4N",
            "prints values with their smallest (len-str-wise) available representation"
        )
        testLineOnInterpreter(
            "obj.mass := 5",
            None,
            "can add unit-less properties to unit-ful object",
        )
        testLineOnInterpreter(
            "obj.mass * obj.acc = obj.force",
            None,
            "can create relations with some unit-ful values and some not",
        )
        testLineOnInterpreter(
            "obj.acc",
            "4",
            "doesn't infer units when not enough values (in the relation) have units",
        )
        testLineOnInterpreter(
            "obj.mass := 5<kg>",
            None,
            "can redefine values with units later",
        )
        testLineOnInterpreter(
            "obj.acc",
            "4m/s^2",
            "can infer units when enough values (in relation) have units",
        )
        testLineOnInterpreter(
            "obj.mass := 10",
            None,
            "can override unit-ful values and keep previous units",
        )
        testLineOnInterpreter(
            "obj.mass",
            "10kg",
            "keeps overriden value but doesn't change units",
        )
        testLineOnInterpreter(
            "obj.acc",
            "2m/s^2",
            "actually changes other values when overridden (and their units stay the same too)",
        )
        resetState()
        testLineOnInterpreter(
            "f<kg*m/s^2> = m<kg> * a<m/s/s>",
            None,
            "can set a basic relation for future unit-ness",
        )
        testLineOnInterpreter(
            "a",
            "f/m m/s^2",
            "separates units when a symbolic solution is given",
        )
        resetState()
        Tester.stopIfFailed()

        # ensure units can catch relational (math) errors
        testLineOnInterpreter(
            "a<u1> = b",
            None,
            "can relate with arbitrary units",
        )
        testLineOnInterpreter(
            "b = c<u2> * d<u3>",
            ("ontradiction", "ismatch", "nits"),
            "can catch basic unit contradiction errors"
        )
        testLineOnInterpreter(
            "<u2 * u3> := <u1>",
            None,
            "can set unit alias with complicated unit on lefthand side",
        )
        testLineOnInterpreter(
            "b = c<u2> * d<u3>",
            None,
            "will not catch contradictions after alias is set",
        )
        testLineOnInterpreter(
            "c = 2 * c2",
            None,
            "can infer units (again)",
        )
        testLineOnInterpreter(
            "c2",
            "c/2 u2",
            "can print correct units after inference",
        )
        testLineOnInterpreter(
            "c + c2",
            "3*c u2",
            "can add units together (NOT u2+u2)",
        )
        testLineOnInterpreter(
            "c + d",
            ("ontradiction", "ismatch", "nit"),
            "errors when adding two different units together",
        )
        testLineOnInterpreter(
            "c - d",
            ("ontradiction", "ismatch", "nit"),
            "errors when subtracting two different units together",
        )
        resetState()
        Tester.stopIfFailed()

        # test commands
        testLineOnInterpreter(
            "a := 4",
            None,
            "can store values before listing",
        )
        testLineOnInterpreter(
            "!list",
            "1:  a := 4",
            "can list aliases",
        )
        testLineOnInterpreter(
            "b = a + 5",
            None,
            "can store relations before listing",
        )
        testLineOnInterpreter(
            "!list",
            ("1:  a := 4", "2:  b = a + 5"),
            "can list stored values (not inferred!) and relation lines in order"
        )
        testLineOnInterpreter(
            "a^2 + b ^ 2 =c^  2",
            None,
            "can add a strangely spaced relation",
        )
        testLineOnInterpreter(
            "!list",
            ("1:  a := 4", "2:  b = a + 5", "3:  a^2 + b ^ 2 =c^  2"),
            "lists relations with original spacing",
        )
        testLineOnInterpreter(
            "!list b",
            ("2:  b = a + 5", "3:  a^2 + b ^ 2 =c^  2"),
            "can list with a single filter",
        )
        testLineOnInterpreter(
            "c := 97 ^ .5",
            None,
            "can store non-contradictory value",
        )
        testLineOnInterpreter(
            "!list a, c",
            "3:  a^2 + b ^ 2 =c^  2",
            "can list with multiple AND filters"
        )
        Tester.stopIfFailed()
        testLineOnInterpreter(
            "!forget",
            (ISP + "^^^^^^^\n", "rror", "equired"),
            "errors when trying to forget with no arguments",
        )
        testLineOnInterpreter(
            "b",
            "9",
            "can get inferred values when nothing is forgotten",
        )
        testLineOnInterpreter(
            "!forget c, 2",
            None,
            "can forget given multiple parameters (even of different types)",
        )
        testLineOnInterpreter(
            "b",
            "a + 5",
            "reverts to symbolic solution when inferred solution is forgotten",
        )
        testLineOnInterpreter(
            "!list",
            ("1:  a := 4", "3:  a^2 + b ^ 2 =c^  2"),
            "only lists unforgotten items",
        )
        Tester.stopIfFailed()
        testLineOnInterpreter(
            "!reset",
            "Are you sure you want to reset?",
            "displays confirmation before resetting",
        )
        testLineOnInterpreter(
            "b",
            "a + 5",
            "variables are not deleted if reset is not confirmed",
        )
        testLineOnInterpreter(
            "!reset",
            "Are you sure you want to reset?",
            "displays confirmation before resetting (even if already displayed)",
        )
        testLineOnInterpreter(
            "!reset",
            "Reset complete\n\n",
            "displays message when reset was successful",
        )
        testLineOnInterpreter(
            "b",
            (ISP + "^\n", "rror", "ndefined"),
            "resetting removes all variables"
        )
        testLineOnInterpreter(
            "b := 4",
            None,
            "can redefine variables after a reset",
        )
        testLineOnInterpreter(
            "!list",
            "1:  b := 4",
            "starts listing counter back at 1 after reset",
        )
        testLineOnInterpreter(
            "!reset confirm",
            "Reset complete\n\n",
            "bypasses confirmation when given 'confirm'"
        )
        resetState()
        Tester.stopIfFailed()
        # TODO: test eval and save

        # other tests
        testLineOnInterpreter(
            "a() := 4",
            None,
            "can create (kind of useless) template aliases",
        )
        testLineOnInterpreter(
            "b() := 5",
            None,
            "can create multiple (kind of useless) template aliases",
        )
        testLineOnInterpreter(
            "a() * b() + a()",
            "24",
            "can evaluate template aliases in expressions",
        )
        testLineOnInterpreter(
            "createObj(p1, p2) := {p1 := 1, p2 := 2, sum = p1 + p2}",
            None,
            "can create template aliases for objects",
        )
        testLineOnInterpreter(
            "obj := createObj(a, b)",
            None,
            "can create objects from template aliases",
        )
        testLineOnInterpreter(
            "obj.a",
            "1",
            "can evaluate object properties from template aliases",
        )
        testLineOnInterpreter(
            "obj.sum",
            "3",
            "can evaluate object relations from template aliases",
        )
        testLineOnInterpreter(
            "obj",
            "{a=1, b=2}(sum=3)",
            "can print objects derived from template aliases",
        )



class Tester:
    allTests = [
        'Lexer',
        'Parser',
        'Solver',
        'Interpreter',
    ]
    currentTest = None
    hasFailed = False

    # interface methods
    @classmethod
    def runTest(cls, name):
        cls.checkAllTestsAccounted()
        try:
            cls.currentTest = name
            eval("TestSuites.{}()".format(name))
        except AttributeError as e:
            if "'TestSuites' has no" in str(e):
                raise AttributeError("Test '{}' does not exist".format(name))
            else:
                raise e

    @classmethod
    def runAll(cls):
        print("Running all tests...")
        print()
        for testName in cls.allTests:
            cls.runTest(testName)
            print("Tests for {} passed!".format(testName))
        print("Done!")

    @classmethod
    def stopIfFailed(cls):
        if cls.hasFailed:
            raise InterruptedError("Tests failed!")

    # helper assertion methods
    @classmethod
    def assertEqual(cls, val1, val2, testName):
        if val1 == val2:
            return True

        # format to differentiate ints and strings
        if type(val1) is str:
            val1 = '"{}"'.format(val1)
        if type(val2) is str:
            val2 = '"{}"'.format(val2)

        cls.assertFailed(
            testName,
            "Values {} and {} are not equal".format(val1, val2)
        )
        return False
    
    @classmethod
    def assertNotEqual(cls, val1, val2, testName):
        if val1 != val2:
            return True

        # format to differentiate ints and strings
        if type(val1) is str:
            val1 = '"{}"'.format(val1)
        if type(val2) is str:
            val2 = '"{}"'.format(val2)

        cls.assertFailed(
            testName,
            "Values {} and {} are not unequal".format(val1, val2)
        )
        return False
    
    @classmethod
    def assertIs(cls, val1, val2, testName):
        if val1 is val2:
            return True

        # format to differentiate ints and strings
        if type(val1) is str:
            val1 = '"{}"'.format(val1)
        if type(val2) is str:
            val2 = '"{}"'.format(val2)

        cls.assertFailed(
            testName,
            "{} is not {}".format(val1, val2)
        )
        return False

    @classmethod
    def assertItersEqual(cls, iter1, iter2, testName):
        try:
            arr1 = tuple(x for x in iter1)
            arr2 = tuple(x for x in iter2)
        except TypeError as e:
            if "is not iterable" in str(e):
                cls.assertFailed(
                    testName,
                    "Given values were not iterable",
                    "Iter1: {}".format(iter1),
                    "Iter2: {}".format(iter2)
                )
                return False
            else:
                raise e
        if len(arr1) != len(arr2):
            cls.assertFailed(
                testName,
                "Iterable-lengths were not equal",
                "Iter1: {} (of type {})".format(arr1, type(iter1)),
                "Iter2: {} (of type {})".format(arr2, type(iter2))
            )
            return False
        
        mismatches = []
        for idx in range(len(arr1)):
            if arr1[idx] != arr2[idx]:
                mismatches.append(idx)
        if len(mismatches) > 0:
            cls.assertFailed(
                testName,
                "Elements did not match: {}".format(tuple(mismatches)),
                "Iter1: {} (of type {})".format(arr1, type(iter1)),
                "Iter2: {} (of type {})".format(arr2, type(iter2))
            )
            return False
        return True

    @classmethod
    def assertFunctionErrors(cls, fn, errorType, testName):
        expected = "Expected an error of type {}".format(
            errorType if not isinstance(errorType, Exception)
            else "'{}' with message '{}'".format(type(errorType).__name__, errorType)
        )
        try:
            fn()
            cls.assertFailed(testName, expected, "(No error received)")
            return False
        except Exception as e:
            if type(errorType) is str:
                if errorType in str(e):
                    return True
            elif isinstance(errorType, type):
                if isinstance(e, errorType):
                    return True
            elif isinstance(errorType, Exception):
                if isinstance(e, type(errorType)) and str(errorType) in str(e):
                    return True
            
            cls.assertFailed(testName, expected, "Got {}: {}".format(type(e).__name__, e))
            return False

    @classmethod
    def assertIn(cls, val1, val2, testName):
        if val1 in val2:
            return True

        cls.assertFailed(testName, "Expected '{}' to include '{}'".format(val2, val1))
        return False

    @classmethod
    def assertFailed(cls, testName, *args):
        cls.hasFailed = True
        print("Testing '{}' failed! {}".format(cls.currentTest, testName))
        if len(args) > 0:
            for arg in args:
                print('    ', arg)
        print()

    # other helper methods
    _checkedAllTestsAccounted = False
    @classmethod
    def checkAllTestsAccounted(cls):
        if not cls._checkedAllTestsAccounted:
            numAttrsNotTests = 4 # obtained via len(__dict__) with no methods defined
            if len(cls.allTests) != len(TestSuites.__dict__) - numAttrsNotTests:
                print(len(TestSuites.__dict__))
                raise IndexError("Some tests were not accounted for!")
            cls._checkedAllTestsAccounted = True

if __name__ == "__main__":
    Tester.runAll()
    