import re
from Lexer import Lexer
from TokenKinds import *

class HplLexer(Lexer):

    def __init__(self):

        symbolChars = "a-zA-Z!@#$%^&*~\-_=+\|:,<.>/?"
        literalChar = r"(?:\\[strn0'\"\\]|[^\r\n'\"\\])"
        
        whiteSpacePattern = re.compile(r"(?:\s+|;[^\n]*)+")
        symbolPattern = re.compile('[' + symbolChars + '][0-9' + symbolChars + ']*')
        operatorPattern = re.compile(r"[\[\]{}()]")
        hexPattern = re.compile(r"-?0[xX][a-fA-F0-9]+")
        intPattern = re.compile(r"-?\d+")
        charPattern = re.compile("'" + literalChar + "'")
        stringPattern = re.compile('"' + literalChar + '*"')
        errorPattern = re.compile(r".?")
        
        self.__patterns = [
            (WHITESPACE, whiteSpacePattern),
            (SYMBOL, symbolPattern),
            (OPERATOR, operatorPattern),
            (HEX_LITERAL, hexPattern),
            (INT_LITERAL, intPattern),
            (CHAR_LITERAL, charPattern),
            (STRING_LITERAL, stringPattern),
            (ERROR, errorPattern)
        ]

    def patterns(self):
        return self.__patterns

