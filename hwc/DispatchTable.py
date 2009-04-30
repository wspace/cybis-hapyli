
class DispatchTableError(Exception):
    pass

class DispatchEntry:
    
    def __init__(self, function):
        self.function = function
        self.compiling = False
    
class DispatchTable:

    def __init__(self):
        self.__entryLookup = {}

    def __contains__(self, item):
        return item in self.__entryLookup
        
    def __getitem__(self, signature):
        
        if signature not in self.__entryLookup:
            raise DispatchTableError("Function '" + signature + "' not defined.")
            
        return self.__entryLookup[signature]
        
    def addFunction(self, function):
        
        signature = function.signature
        
        if signature in self.__entryLookup:
            raise DispatchTableError("Function '" + signature + "' already defined.")
            
        entry = DispatchEntry(function)
        self.__entryLookup[signature] = entry
