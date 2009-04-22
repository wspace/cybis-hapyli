
class HeapError(Exception):
    pass

class Heap:

    def __init__(self, heapPtr):
        self.__heapTable = {}
        self.__heapPtr = heapPtr
        
    def __iter__(self):
        return self.__heapTable.itervalues()
        
    def __getitem__(self, name):
    
        if name not in self.__heapTable:
            raise HeapError("Symbol '" + name + "' not defined.")
    
        return self.__heapTable[name]
        
    def getHeapPtr(self):
        return self.__heapPtr
        
    def reserve(self, name, capacity, data):
        
        if len(data) > capacity:
            raise HeapError("Length of data cannot be greater than capacity.")
        
        if name in self.__heapTable:
            raise HeapError("Symbol '" + name + "' already defined.")
        
        if capacity <= 0:
            raise HeapError("Capacity must be greater than 0.")
        
        address = self.__heapPtr
        entry = (address, capacity, data)
        
        self.__heapTable[name] = entry
        self.__heapPtr += capacity

