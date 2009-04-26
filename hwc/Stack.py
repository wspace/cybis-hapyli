
class StackError(Exception):
    pass
    
class Stack:

    def __init__(self):
        self.__symbols = []

    def __contains__(self, item):
        return item in self.__symbols
        
    def push(self, name):
        
        if name in self.__symbols:
            raise StackError("Symbol '" + name + "' already defined.")
        
        self.__symbols.append(name)
    
    def pop(self):
        return self.__symbols.pop()
    
    def index(self, name):
        
        if name not in self.__symbols:
            raise StackError("Symbol '" + name + "' not defined.")
            
        index = len(self.__symbols) - self.__symbols.index(name) - 1
        return index
        
