
class HeapError(Exception):
    pass

class HeapEntry:
    
    def __init__(self, name, address, capacity, data):
        self.name = name
        self.address = address
        self.capacity = capacity
        self.data = data
    
class Heap:

    def __init__(self, heapPtr):
        self.__heapTable = {}
        self.heapPtr = heapPtr
        
    def __iter__(self):
        return self.__heapTable.itervalues()

    def __contains__(self, item):
        return item in self.__heapTable
        
    def __getitem__(self, name):
    
        if name not in self.__heapTable:
            raise HeapError("Symbol '" + name + "' not defined.")
    
        return self.__heapTable[name]
        
    def reserve(self, name, capacity, data):
        
        if len(data) > capacity:
            raise HeapError("Length of data cannot be greater than capacity.")
        
        if name in self.__heapTable:
            raise HeapError("Symbol '" + name + "' already defined.")
        
        if capacity <= 0:
            raise HeapError("Capacity must be greater than 0.")
        
        address = self.heapPtr
        entry = HeapEntry(name, address, capacity, data)
        
        self.__heapTable[name] = entry
        self.heapPtr += capacity

