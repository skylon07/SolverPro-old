from constants import *

from lexer import Lexer
lexer = Lexer()

from solverparser import Parser
parser = Parser()

from interpreter import Interpreter
def printIndented(*args):
    # first arg is left alone (for traceback errors)
    joinedArgs = ('\n' + INDENT).join((str(arg) for arg in args))
    print(joinedArgs)
interpreter = Interpreter(printIndented)

if __name__ == "__main__":
    # TODO: add help command
    print("=== SolverPro v{} ===".format(VERSION))
    while True:
        userInp = input(USER_INPUT).replace('\\n', '\n')
        interpreter.executeLine(userInp)
