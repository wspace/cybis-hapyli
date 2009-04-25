from DispatchTable import DispatchTable
from Heap import Heap
from Stack import Stack
from itertools import imap, count
from ast import *

class SemanticError(Exception):
    
    def __init__(self, token, message):
        
        if token == None:
            fullMessage = message
        else:
            fullMessage = str(token) + '\n' + message
        
        Exception.__init__(self, fullMessage)
        
        
newid = imap(lambda id: '~' + str(id) + '~', count()).next
        
def compileProgram(program):
    
    heap = buildProgramHeap(program)
    dispatch = buildProgramDispatchTable(program)
    
    nonInlineFunctions = filter(lambda f: not f.inline, program.functions)
    
    codeSegment = compileCodeSegment(dispatch, heap, nonInlineFunctions)
    bootRecord = compileBootRecord(dispatch, heap)
    dataSegment = compileDataSegment(heap)

    return dataSegment + bootRecord + codeSegment
            
def buildProgramHeap(program):

    heap = Heap(1)
    
    for v in program.variables:
        if v.name not in heap:
            heap.reserve(v.name, v.size, v.data)
        else:
            raise SemanticError(v.token, "Variable '" + v.name + "' already defined.")

    return heap
            
def buildProgramDispatchTable(program):

    dispatch = DispatchTable()
    
    for f in program.functions:
        if f.signature not in dispatch:
            dispatch.addFunction(f)
        else:
            raise SemanticError(f.token, "Function '" + f.signature + "' already defined.")
            
    return dispatch
    
def compileCodeSegment(dispatch, heap, functions):
    
    codeSegment = []
    
    for f in functions:
        assert not f.inline, "Inline functions cannot be compiled directly into the code segment."
        functionCode = compileFunction(dispatch, heap, f)
        codeSegment.extend(functionCode)
        
    return codeSegment
        
def compileDataSegment(heap):
    
    dataSegment = []
    
    for entry in heap:
        address = entry.address
        data = entry.data
        for item in data:
            dataSegment.extend([push(address), push(item), store()])
            address += 1
            
    dataSegment.extend([push(heap.heapPtr), push(0), store()])
    dataSegment.extend([push(0), push(heap.heapPtr), store()])
    
    return dataSegment
    
def compileBootRecord(dispatch, heap):
    
    if "main~0" in dispatch:
        mainFunction = dispatch["main~0"].function
    else:
        raise SemanticError(None, "Function 'main~0' not defined.")
        
    if mainFunction.inline:
        bootRecord = compileFunction(dispatch, heap, mainFunction)
    else:
        bootRecord = [call("main~0")]
        
    bootRecord.extend([pop(), end()])
        
    return bootRecord
    
def compileFunction(dispatch, heap, function):
    
    signature = function.signature
    assert signature in dispatch, "Dispatch entry for function '" + signature + "' not found."
    dispatch[signature].compiling = True

    stack = buildFunctionStack(function)
    bindingsCode = compileFunctionBindings(dispatch, heap, stack, function)
    bodyCode = compileFunctionBody(dispatch, heap, stack, function)

    functionCode = bindingsCode + bodyCode
    
    if not function.inline:
        functionCode = [label(signature)] + functionCode + [ret()]
    
    dispatch[signature].compiling = False
    return functionCode 
    
def buildFunctionStack(function):
    
    stack = Stack()
    
    for p in function.parameters:
        if p not in stack:
            stack.push(p)
        else:
            raise SemanticError(function.token, "Found multiple declarations of parameter '" + p + "'.")
    
    return stack
    
def compileFunctionBindings(dispatch, heap, stack, function):
    
    bindingsCode = []
    
    for (name, value) in function.bindings:
        if name not in stack:
            valueCode = compileExpression(dispatch, heap, stack, 0, value)
            bindingsCode.extend(valueCode)
            stack.push(name)
        else:
            raise SemanticError(function.token, "Found multiple declarations of symbol '" + name + "'.")
        
    return bindingsCode
        
def compileFunctionBody(dispatch, heap, stack, function):
    
    body = function.body
    
    if isinstance(body, Expression):
        bodyCode = compileExpression(dispatch, heap, stack, 0, body)
        nLocals = len(function.parameters) + len(function.bindings)
        if nLocals > 0:
            bodyCode.append(slide(nLocals))
    elif isinstance(body, AssemblyBlock):
        bodyCode = map(compileAstInstruction, body.instructions)
    else:
        assert False, "Unkown function body type."
        
    return bodyCode
        
def compileExpression(dispatch, heap, stack, offset, ex):
    if isinstance(ex, IntegerLiteralEx):
        return compileIntegerLiteralEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, StringLiteralEx):
        return compileStringLiteralEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, SymbolEx):
        return compileSymbolEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, IfEx):
        return compileIfEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, DoEx):
        return compileDoEx(dispatch, heap, stack, offset, ex)
    elif isinstance(ex, CallEx):
        return compileCallEx(dispatch, heap, stack, offset, ex)
    else:
        assert False, "Unknown expression type."
    
def compileIntegerLiteralEx(dispatch, heap, stack, offset, ex):
    return [push(ex.value)]
    
def compileStringLiteralEx(dispatch, heap, stack, offset, ex):
    var = StringVariable(ex.token, newid(), ex.string)
    heap.reserve(var.name, var.size, var.data)
    symbol = SymbolEx(ex.token, var.name)
    return compileSymbolEx(dispatch, heap, stack, offset, symbol)
    
def compileSymbolEx(dispatch, heap, stack, offset, ex):
    if ex.name in stack:
        address = stack.index(ex.name) + offset
        if address == 0:
            return [dup()]
        else:
            return [copy(address)]
    elif ex.name in heap:
        address = heap[ex.name].address
        return [push(address)]
    else:
        raise SemanticError(ex.token, "Symbol '" + ex.name + "' not defined.")
    
def compileIfEx(dispatch, heap, stack, offset, ex):
    
    conditionCode = compileExpression(dispatch, heap, stack, offset, ex.condition)
    trueValueCode = compileExpression(dispatch, heap, stack, offset, ex.trueValue)
    falseValueCode = compileExpression(dispatch, heap, stack, offset, ex.falseValue)
    
    elseLabel = newid()
    endLabel = newid()
    
    return (
        conditionCode + 
        [jz(elseLabel)] + 
        trueValueCode +
        [jump(endLabel),
         label(elseLabel)] +
        falseValueCode + 
        [label(endLabel)] )
    
def compileDoEx(dispatch, heap, stack, offset, ex):
    argumentListCode = compileArguments(dispatch, heap, stack, offset, ex.expressions)
    nArgs = len(ex.expressions)
    return argumentListCode + [slide(nArgs-1)]
    
def compileCallEx(dispatch, heap, stack, offset, ex):
    
    if not ex.signature in dispatch:
        raise SemanticError(ex.token, "Function '" + ex.signature + "' not defined.")
        
    argumentListCode = compileArguments(dispatch, heap, stack, offset, ex.arguments)
    dispatchEntry = dispatch[ex.signature]
    callingCode = dispatchEntry.call
    
    if callingCode == None:
    
        functionToCall = dispatchEntry.function
        
        if functionToCall.inline:
            if dispatchEntry.compiling:
                raise SemanticError(ex.token, "Function '" + ex.signature + "' cannot be both inline and recursive.")
            else:
                callingCode = compileFunction(dispatch, heap, functionToCall)
        else:
            callingCode = [call(ex.signature)]
        
        dispatchEntry.call = callingCode
        
    return argumentListCode + callingCode
    
def compileArguments(dispatch, heap, stack, offset, arguments):
    
    argumentListCode = []
    argumentOffset = offset
    
    for arg in arguments:
        argumentCode = compileExpression(dispatch, heap, stack, argumentOffset, arg)
        argumentListCode.extend(argumentCode)
        argumentOffset += 1
        
    return argumentListCode
    
def compileAstInstruction(ins):
    command = ins.command.lower()
    operand = ins.operand
    if operand == None:
        return (command, )
    else:
        return (command, operand)

def push(n): return ("push", n)
def dup(): return ("dup", )
def copy(n): return ("copy", n)
def swap(): return ("swap", )
def pop(): return ("pop", )
def slide(n): return ("slide", n)

def add(): return ("add", )
def sub(): return ("sub", )
def mul(): return ("mul", )
def div(): return ("div", )
def mod(): return ("mod", )

def store(): return ("store", )
def load(): return ("load", )

def label(x): return ("label", x)
def call(x): return ("call", x)
def jump(x): return ("jump", x)
def jz(x): return ("jz", x)
def jn(x): return ("jn", x)
def ret(): return ("ret", )
def end(): return ("end", )

def pc(): return ("pc", )
def pn(): return ("pn", )
def rc(): return ("rc", )
def rn(): return ("rn", )