import ast

class SemanticError(Exception):

    def __init__(self, message, node=None):
    
        self.node = node
    
        if node == None:
            fullMessage = message
        else:
            fullMessage = str(node.token) + '\n' + message
            
        Exception.__init__(self, fullMessage)

        
class DuplicateEntryError(Exception):
    pass
        
        
class HeapTable:

    def __init__(self):
        self.__table = {}
        self.heapPtr = 1
        
    def __contains__(self, name):
        return name in self.__table

    def __getitem__(self, name):
        return self.__table[name]
        
    def __iter__(self):
        return iter(self.__table.values())
        
    def add(self, name, size, data):
        
        if name in self:
            raise DuplicateEntryError(name)
        
        address = self.heapPtr
        entry = (address, size, data)
        self.__table[name] = entry
        self.heapPtr += size

        
class StackTable:

    def __init__(self):
        self.__table = []
        
    def __contains__(self, name):
        return name in self.__table
        
    def __getitem__(self, name):
        indexFromLeft = self.__table.index(name)
        indexFromRight = len(self.__table) - indexFromLeft - 1
        return indexFromRight
        
    def add(self, name):
        
        if name in self.__table:
            raise DuplicateEntryError(name)
        
        self.__table.append(name)
        
        
class DispatchTable:

    def __init__(self):
        self.__table = {}
        
    def __contains__(self, signature):
        return signature in self.__table
    
    def __getitem__(self, signature):
        return self.__table[signature]
        
    def __setitem__(self, signature, routine):
        
        if signature in self.__table:
            raise DuplicateEntryError(signature)
        
        self.__table[signature] = routine

def newid_generator():
    auto = 0
    while True:
        auto += 1
        id = '~' + str(auto) + '~'
        yield id
        
newid = newid_generator().next

def stringToArray(string):
    chars = map(ord, string)
    chars.append(0)
    return chars

def buildDispatchTable(routines):
    
    dispatch = DispatchTable()
    
    for r in routines:
        
        signature = r.name + '~' + str(len(r.parameters))
        
        if signature in dispatch:
            raise SemanticError("Routine already defined.", r)
        
        dispatch[signature] = r
        
    return dispatch

def buildHeapTable(variables):
    
    heap = HeapTable()
    
    for v in variables:
        
        if v.name in heap:
            raise SemanticError("Variable already defined.", v)
            
        name = v.name
        
        if isinstance(v, IntegerVariable):
            data = [v.value]
            size = 1
        elif isinstance(v, ArrayVariable):
            data = v.values
            size = len(data)
        elif isinstance(v, StringVariable):
            data = stringToArray(v.string)
            size = len(data)
        elif isinstance(v, UninitializedString):
            data = []
            size = v.size
        else:
            raise SemanticError("Unknown variable type.", v)
            
        heap.add(name, size, data)
        
    return heap
    
def compileProgram(program):
    
    heap = buildHeapTable(program.variables)
    dispatch = buildDispatchTable(program.routines)
    functions = filter(lambda r: isinstance(r, ast.Function), program.routines)
    
    functionCode = []
    
    for f in functions:
        functionCode += compileFunction(dispatch, heap, f)
        
    heapCode = compileHeap(heap)
    
    loweredFunctionSignatures = [f.name.lower() + '~' + str(len(f.parameters)) 
                                 for f in functions]
                                 
    if "main~0" not in loweredFunctionSignatures:
        raise SemanticError("Function main() not defined.")
        
    programCode = heapCode
    programCode += [call("main~0"),
                    end()]
    programCode += functionCode
    
    return programCode
    
def compileHeap(heap):
    
    code = []
    
    for (address, size, data) in heap:
        
        dataPtr = address
        
        for item in data:
        
            code += [push(dataPtr),
                     push(item),
                     store()]
            
            dataPtr += 1
                     
    code += [push(heap.heapPtr),
             push(0),
             store(),
             push(0),
             push(heap.heapPtr),
             store()]
             
    return code
    
def compileFunction(dispatch, heap, function):
    
    signature = function.name + '~' + str(len(function.parameters))
    
    code = [label(signature)]
    
    stack = StackTable()

    for name in function.parameters:
        if name in stack:
            raise SemanticError("Duplicate symbols in parameter list.", function)
        else:
            stack.add(name)
            
    for (name, value) in function.bindings:
        if name in stack:
            raise SemanticError("Duplicate symbols in let-form.", function)
        else:
            stack.add(name)
            code += compileExpression(dispatch, heap, stack, 0, value)
            
    code += compileExpression(dispatch, heap, stack, 0, function.body)
    
    numLocals = len(function.parameters) + len(function.bindings)
    
    code += [slide(numLocals),
             ret()]
    
    return code
    
def compileExpression(dispatch, heap, stack, offset, ex):
    
    if isinstance(ex, ast.IntegerLiteralEx):
        return compileIntegerLiteralEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, ast.StringLiteralEx):
        return compileStringLiteralEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, ast.SymbolEx):
        return compileSymbolEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, ast.IfEx):
        return compileIfEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, ast.DoEx):
        return compileDoEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, ast.CallEx):
        return compileCallEx(dispatch, heap, stack, offset, ex)
    else:
        raise SemanticError("Unknown expression type.", ex)
    
def compileIntegerLiteralEx(dispatch, heap, stack, offset, ex):
    code = [push(ex.value)]
    return code
    
def compileStringLiteralEx(dispatch, heap, stack, offset, ex):
    
    name = newid()
    data = stringToArray(ex.string)
    size = len(data)
    heap.add(name, size, data)
    
    (address, size, data) = heap[name]
    
    code = [push(address)]
    
    return code

def compileSymbolEx(dispatch, heap, stack, offset, ex):
    
    if ex.name in stack:
        index = stack[ex.name]
        address = index + offset
        code = [copy(address)]
    elif ex.name in heap:
        (address, size, data) = heap[ex.name]
        code = [push(address)]
    else:
        raise SemanticError("Symbol not defined.", ex)
        
    return code
        
def compileIfEx(dispatch, heap, stack, offset, ex):
    
    elseLabel = newid()
    endLabel = newid()
    
    code = compileExpression(dispatch, heap, stack, offset, ex.condition)
    code += [jz(elseLabel)]
    code += compileExpression(dispatch, heap, stack, offset+1, ex.trueValue)
    code += [jump(endLabel),
             label(elseLabel)]
    code += compileExpression(dispatch, heap, stack, offset+2, ex.falseValue)
    code += [label(endLabel)]
    
    return code
        
def compileDoEx(dispatch, heap, stack, offset, ex):
    code = compileExpressionList(dispatch, heap, stack, offset, ex.expressions)
    code += [slide(len(ex.expressions)-1)]
    return code

def compileCallEx(dispatch, heap, stack, offset, ex):

    signature = ex.name + '~' + str(len(ex.arguments))

    if not signature in dispatch:
        raise SemanticError("Routine '" + signature + "' not defined.", ex)

    routine = dispatch[signature]
    
    code = compileExpressionList(dispatch, heap, stack, offset, ex.arguments)
    
    if isinstance(routine, ast.Macro):
        code += map(compileInstruction, routine.body)
    elif isinstance(routine, ast.Function):
        code += [call(signature)]
        
    return code
        
def compileExpressionList(dispatch, heap, stack, offset, expressions):
    
    code = []
    expOffset = offset
    
    for exp in expressions:
        code += compileExpression(dispatch, heap, stack, expOffset, exp)
        expOffset += 1
        
    return code    
    
def compileInstruction(ins):
    if ins.operand == None:
        return (ins.command, )
    else:
        return (ins.command, ins.operand)

        
def push(n):  return ("push"  , n)
def dup():    return ("dup"   ,  )
def copy(n):  return ("copy"  , n)
def swap():   return ("swap"  ,  )
def pop():    return ("pop"   ,  )
def slide(n): return ("slide" , n)
    
def add(): return ("add" ,  )
def sub(): return ("sub" ,  )
def mul(): return ("mul" ,  )
def div(): return ("div" ,  )
def mod(): return ("mod" ,  )

def store(): return ("store" ,  )
def load():  return ("load"  ,  )

def label(x): return ("label" , x)
def call(x):  return ("call"  , x)
def jump(x):  return ("jump"  , x)
def jz(x):    return ("jz"    , x)
def jn(x):    return ("jn"    , x)
def ret():    return ("ret"   ,  )
def end():    return ("end"   ,  )

def pc(): return ("pc" ,  )
def pn(): return ("pn" ,  )
def rc(): return ("rc" ,  )
def rn(): return ("rn" ,  )
