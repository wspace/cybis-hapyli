
class SyntaxError(Exception):
    
    def __init__(self, message, tokenIndex, token=None):
        
        self.tokenIndex = tokenIndex
        self.token = token
        
        if token == None:
            fullMessage = message
        else:
            fullMessage = str(token) + '\n' + message
            
        Exception.__init__(self, fullMessage)

class Parser:

    def __init__(self, tokens):
        self.__tokens = list(tokens)
        self.__tokenIndex = -1
                
    def hasMoreTokens(self):
        return self.__tokenIndex < len(self.__tokens) - 1
        
    def current(self):
        if 0 <= self.__tokenIndex < len(self.__tokens):
            return self.__tokens[self.__tokenIndex]
        else:
            return None
           
    def fail(self, message):
    
        tokenIndex = self.__tokenIndex
        tokens = self.__tokens
        
        if 0 <= tokenIndex < len(tokens):
            raise SyntaxError(message, tokenIndex, tokens[tokenIndex])
        else:
            raise SyntaxError(message, tokenIndex)
           
    def many(self, parser):
        
        results = []
        
        while True:
            lastValidTokenIndex = self.__tokenIndex
            try:
                nextResult = parser()
                results.append(nextResult)
            except SyntaxError, err:
                self.__tokenIndex = lastValidTokenIndex
                break
                
        return results
        
    def many1(self, parser):
        results = [parser()]
        results.extend(self.many(parser))
        return results
    
    def optional(self, parser):

        result = None
        lastValidTokenIndex = self.__tokenIndex

        try:
            result = parser()
        except SyntaxError, err:
            self.__tokenIndex = lastValidTokenIndex
        
        return result
    
    def either(self, parser, *parsers):
        
        parsers = [parser]+list(parsers)
        lastValidTokenIndex = self.__tokenIndex
        errorToRaise = None
        success = False
        result = None
        
        for p in parsers:
            try:
                result = p()
                success = True
                break
            except SyntaxError, err:
                if (errorToRaise == None or 
                    errorToRaise.tokenIndex < err.tokenIndex):
                    errorToRaise = err
                self.__tokenIndex = lastValidTokenIndex
                
        if success:
            return result
        else:
            raise errorToRaise
        
    def match(self, kind=None, string=None):
    
        if self.__tokenIndex < len(self.__tokens) - 1:
            
            self.__tokenIndex += 1
            token = self.__tokens[self.__tokenIndex]
            
            if (kind != None and
                kind != token.kind):
                self.fail("Expected kind: " + str(kind))
            
            if (string != None and
                string.lower() != token.string.lower()):
                self.fail("Expected string: " + string)
        
            return token
            
        else:
        
            self.fail("Unexpected end of file.")

