from Parser import Parser
from TokenKinds import *

def unescape(str):
    
    chars = []
    escaped = False
    escapeLookup = {'s': ' ',
                    't': '\t',
                    'r': '\r',
                    'n': '\n',
                    '0': '\0'}
    
    for c in str:
        if escaped:
            if c in escapeLookup:
                chars.append(escapeLookup[c])
            else:
                chars.append(c)
        else:
            if c == '\\':
                escaped = True
            else:
                chars.append(c)
    
    return ''.join(chars)

class HplLexemeParser(Parser):

    def __init__(self, tokens):
        Parser.__init__(self, tokens)
        self.__reserved = ["import", "var", "asm", "def", "let", "in", "if", "do"]

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
        keyword = keyword.lower()
        t = self.lexeme(kind=SYMBOL)
        if t.string.lower() != keyword:
            self.fail("Expected: " + keyword)
        elif not keyword in self.__reserved:
            self.fail("Keyword '" + keyword + "' not reserved.")
        else:
            return t.string
        
    def identifier(self):
        t = self.lexeme(kind=SYMBOL)
        if t.string.lower() in self.__reserved:
            self.fail("Identifiers cannot be reserved keywords.")
        else:
            return t.string

    def whiteSpace(self):
        self.match(kind=WHITESPACE)
        
    def lexeme(self, kind=None, string=None):
        self.many(self.whiteSpace)
        return self.match(kind=kind, string=string)
        
