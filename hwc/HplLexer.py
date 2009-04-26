import re
from Lexer import Lexer
from TokenKinds import *

class HplLexer(Lexer):

    def __init__(self):

        whiteSpacePattern = r"(?:\s+|;[^\n]*)+"
        operatorPattern = "[()]"
        
        terminate = r"(?=[\s()]|$)"
        
        intPattern    =  "-?\d+" + terminate
        hexPattern    =  "-?0[xX][0-9a-fA-F]+" + terminate
        symbolPattern = r"[0-9a-zA-Z~!@#$%^&*\-_=+\|:,<.>/?]+" + terminate
    
        literalChar   = r"(?:\\[strn0'\"\\]|[^\r\n'\"\\])"
        charPattern   = "'" + literalChar + "'" + terminate
        stringPattern = '"' + literalChar + '*"' + terminate
    
        errorPattern  = ".?"

        uncompiledPatterns = [
            (WHITESPACE, whiteSpacePattern),
            (OPERATOR, operatorPattern),
            (INT_LITERAL, intPattern),
            (HEX_LITERAL, hexPattern),
            (SYMBOL, symbolPattern),
            (CHAR_LITERAL, charPattern),
            (STRING_LITERAL, stringPattern),
            (ERROR, errorPattern) ]
            
        compiledPatterns = [(kind, re.compile(pattern))
                            for (kind, pattern) in uncompiledPatterns]
        
        self.__patterns = compiledPatterns

    def patterns(self):
        return self.__patterns

