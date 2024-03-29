from hwc.Parser import Parser
from hwc.escape import unescape
from hwc.TokenKinds import *

class HplLexemeParser(Parser):

    def __init__(self, tokens):
        Parser.__init__(self, tokens)
        self.__reserved = ["import", "var", "asm", "def", "inline", "let", "in", "if", "do"]
    
    def string(self):
        t = self.lexeme(kind=STRING_LITERAL)
        escapedString = t.string[1:-1]
        unescapedString = unescape(escapedString)
        return unescapedString
    
    def number(self):
        return self.either(self.intNumber,
                           self.hexNumber,
                           self.charNumber)
    
    def charNumber(self):
        t = self.lexeme(kind=CHAR_LITERAL)
        escapedChar = t.string[1:-1]
        unescapedChar = unescape(escapedChar)
        return ord(unescapedChar)
        
    def intNumber(self):
        t = self.lexeme(kind=INT_LITERAL)
        return int(t.string)
        
    def hexNumber(self):
        t = self.lexeme(kind=HEX_LITERAL)
        return int(t.string, 16)
        
    def reserved(self, keyword):
        t = self.lexeme(kind=SYMBOL)
        if t.string != keyword:
            self.fail("Expected: " + keyword)
        elif not keyword in self.__reserved:
            self.fail("Keyword '" + keyword + "' not reserved.")
        else:
            return t.string
        
    def identifier(self):
        t = self.lexeme(kind=SYMBOL)
        if t.string in self.__reserved:
            self.fail("Identifiers cannot be reserved keywords.")
        else:
            return t.string
    
    def whiteSpace(self):
        self.many(lambda: self.match(kind=WHITESPACE))
        
    def lexeme(self, kind=None, string=None):
        self.whiteSpace()
        return self.match(kind=kind, string=string)
        
