from Parser import Parser
from escape import unescape
from TokenKinds import *

class HplLexemeParser(Parser):

    def __init__(self, tokens):
        Parser.__init__(self, tokens)
        self.__reserved = ["import", "var", "def", "inline", "let", "in", "if", "do"]

    def group(self, opOpen, parser, opClose):
        self.lexeme(kind=OPERATOR, string=opOpen)
        result = parser()
        self.lexeme(kind=OPERATOR, string=opClose)
        return result
            
    def parens(self, parser):
        return self.group('(', parser, ')')

    def braces(self, parser):
        return self.group('{', parser, '}')                
    
    def brackets(self, parser):
        return self.group('[', parser, ']')
        
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
        self.many(lambda: self.match(kind=WHITESPACE))
        
    def lexeme(self, kind=None, string=None):
        self.whiteSpace()
        return self.match(kind=kind, string=string)
        
