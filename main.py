from constants import *

from lexer import Lexer
lexer = Lexer()

from parser import Parser
parser = Parser()

from interpreter import Interpreter
interpreter = Interpreter(print)

if __name__ == "__main__":
    # TODO: add help command
    print("=== SolverPro v{} ===".format(VERSION))
    while True:
        userInp = input(USER_INPUT).replace('\\n', '\n')
        def runLine():
            try:
                return interpreter.evaluateLine(userInp)
            except Exception as e:
                interpreter._handleError(e)
        print(INDENT + "interpretation:\n" + INDENT * 2 + str(runLine()))
        interpreter.executeLine(userInp)
