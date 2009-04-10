
class SyntaxError(Exception):
    pass

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
        
        t = self.current()
        
        if t == None:
            raise SyntaxError(message)
        else:
            raise SyntaxError(str(t) + '\n' + message)
           
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
        oldTokenIndex = self.__tokenIndex
        highestTokenIndexReached = oldTokenIndex
        errorToRaise = None
        success = False
        result = None
        
        for p in parsers:
            try:
                result = p()
                success = True
                break
            except SyntaxError, err:
                if self.__tokenIndex > highestTokenIndexReached:
                    highestTokenIndexReached = self.__tokenIndex
                    errorToRaise = err
                self.__tokenIndex = oldTokenIndex
                
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
        
