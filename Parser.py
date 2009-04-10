
class SyntaxError(Exception):
    
    def __init__(self, tokenIndex, token, message):
        
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
        token = self.current()
        raise SyntaxError(tokenIndex, token, message)
           
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
        
    def next(self):
        if self.hasMoreTokens():
            self.__tokenIndex += 1
            return self.__tokens[self.__tokenIndex]
        else:
            self.fail("End of file.")
        
    def match(self, kind=None, string=None):
    
        token = self.next()
        
        if token == None:
            self.fail("End of file.")
            
        if (kind != None and
            kind != token.kind):
            self.fail("Expected kind: " + str(kind))
            
        if (string != None and
            string.lower() != token.string.lower()):
            self.fail("Expected string: " + string)
        
        return token
        
