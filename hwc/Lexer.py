from hwc.Token import Token

class LexicalError(Exception):
    pass

class Lexer:

    def patterns(self):
        return []
        
    def tokenizeFile(self, file):
        
        stream = open(file)
        text = stream.read()
        stream.close()
        tokens = self.tokenizeText(text)
        
        for t in tokens:
            t.file = file
        
        return tokens
        
        
    def tokenizeText(self, text):
    
        tokens = []
        lines = text.split('\n')
        lineNumber = 1
    
        for line in lines:
            
            tokensInLine = self.tokenizeLine(line)
        
            for t in tokensInLine:
                t.lineNumber = lineNumber
            
            tokens.extend(tokensInLine)
            lineNumber += 1
        
        return tokens
        
        
    def tokenizeLine(self, line):
        
        tokens = []
        index = 0
        
        while index < len(line):
            t = self.matchToken(line, index)
            tokens.append(t)
            index += len(t.string)
        
        return tokens
                
                
    def matchToken(self, line, index=0):
        
        match = None
        
        for (kind, regex) in self.patterns():
            match = regex.match(line, index)
            if match != None:
                break
                
        if match == None:
            raise LexicalError("Patterns not comprehensive.")
        else:
            t = Token()
            t.line = line
            t.index = index
            t.kind = kind
            t.string = match.group()
            return t
