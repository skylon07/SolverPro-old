from constants import *

from lexer import Lexer
lexer = Lexer()

from parser import Parser
parser = Parser()

from interpreter import Interpreter
interpreter = Interpreter(print)

if __name__ == "__main__":
    while True:
        userInp = input(USER_INPUT)
        interpreter.executeLine(userInp)

    def onExpression(tokens, branch):
        print("expression:", branch, tokens)
    # parser.onExpression(onExpression)
    def onOperationLow(tokens, branch):
        print("operationLow:", branch, tokens)
    # parser.onOperationLow(onOperationLow)
    def onOperationMid(tokens, branch):
        print("operationMid:", branch, tokens)
    # parser.onOperationMid(onOperationMid)
    def onOperationHigh(tokens, branch):
        print("operationHigh:", branch, tokens)
    # parser.onOperationHigh(onOperationHigh)
    def onOperationMax(tokens, branch):
        print("operationMax:", branch, tokens)
    # parser.onOperationMax(onOperationMax)
    while True:
        try:
            import re
            inp = input(USER_INPUT)
            tokens = lexer.process(inp)
            # print(re.sub("Token", "T", str(tokens)))
            parser.inspect(tokens, inp)
            print()
        except (parser.ParseError, parser.EOLError) as e:
            print(e.message)
        except Exception as e:
            print(type(e).__name__ + ":", e)
