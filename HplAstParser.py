from HplLexemeParser import HplLexemeParser
from TokenKinds import *
from ast import *

class HplAstParser(HplLexemeParser):

    def module(self):
        
        elements = self.many(self.element)
        
        self.whiteSpace()
        
        if self.hasMoreTokens():
            self.element()
        
        imports = filter(lambda e: isinstance(e, Import), elements)
        variables = filter(lambda e: isinstance(e, Variable), elements)
        functions = filter(lambda e: isinstance(e, Function), elements)
        
        return Module(imports, variables, functions)

    def element(self):
        return self.either(
            self.moduleImport,
            self.variable,
            self.function)

    def moduleImport(self):
        self.reserved("import")
        token = self.current()
        file = self.string()
        return Import(token, file)
    
    def variable(self):
        return self.either(
            self.integerVariable,
            self.arrayVariable,
            self.stringVariable,
            self.uninitializedVariable)
        
    def integerVariable(self):
        self.reserved("var")
        token = self.current()
        name = self.identifier()
        self.lexeme(string='=')
        value = self.number()
        return IntegerVariable(token, name, value)

    def arrayVariable(self):
        self.reserved("var")
        token = self.current()
        name = self.identifier()
        self.lexeme(string='=')
        self.lexeme(string='(')
        values = self.many1(self.number)
        self.lexeme(string=')')
        return ArrayVariable(token, name, values)

    def stringVariable(self):
        self.reserved("var")
        token = self.current()
        name = self.identifier()
        self.lexeme(string='=')
        string = self.string()
        return StringVariable(token, name, string)

    def uninitializedVariable(self):
        self.reserved("var")
        token = self.current()
        name = self.identifier()
        self.lexeme(string='(')
        size = self.number()
        self.lexeme(string=')')
        return UninitializedVariable(token, name, size)

    def function(self):
        
        functionType = self.either(lambda: self.reserved("def"),
                                   lambda: self.reserved("asm"))
                                   
        token = self.current()
        inline = self.optional(lambda: self.reserved("inline")) == "inline"
        name = self.identifier()
        self.lexeme(string='(')
        parameters = self.many(self.identifier)
        self.lexeme(string=')')
        self.lexeme(string='=')
        
        bindings = self.optional(self.letForm)
        if bindings == None:
            bindings = []
        
        if functionType == "def":
            body = self.expression()
        else:
            body = self.assemblyBlock()
        
        return Function(token, inline, name, parameters, bindings, body)
    
    def letForm(self):
        self.reserved("let")
        bindings = self.many(self.binding)
        self.reserved("in")
        return bindings
        
    def binding(self):
        name = self.identifier()
        self.lexeme(string='=')
        value = self.expression()
        return (name, value)
    
    def assemblyBlock(self):
        self.lexeme(string='(')
        token = self.current()
        instructions = self.many(self.instruction)
        self.lexeme(string=')')
        return AssemblyBlock(token, instructions)
    
    def expression(self):
        return self.either(
            self.integerLiteralEx,
            self.stringLiteralEx,
            self.symbolEx,
            self.ifEx,
            self.doEx,
            self.callEx)
    
    def integerLiteralEx(self):
        value = self.number()
        token = self.current()
        return IntegerLiteralEx(token, value)
    
    def stringLiteralEx(self):
        string = self.string()
        token = self.current()
        return StringLiteralEx(token, string)

    def symbolEx(self):
        name = self.identifier()
        token = self.current()
        return SymbolEx(token, name)

    def ifEx(self):
        self.lexeme(string='(')
        token = self.current()
        self.reserved("if")
        condition = self.expression()
        trueValue = self.expression()
        falseValue = self.expression()
        self.lexeme(string=')')
        return IfEx(token, condition, trueValue, falseValue)

    def doEx(self):
        self.lexeme(string='(')
        token = self.current()
        self.reserved("do")
        args = self.many1(self.expression)
        self.lexeme(string=')')
        return DoEx(token, args)

    def callEx(self):
        self.lexeme(string='(')
        token = self.current()
        name = self.identifier()
        args = self.many(self.expression)
        self.lexeme(string=')')
        return CallEx(token, name, args)

    def instruction(self):
        return self.either(
            self.push  , self.dup  , self.copy , self.swap , self.pop , self.slide,
            self.add   , self.sub  , self.mul  , self.div  , self.mod , 
            self.store , self.load ,
            self.label , self.call , self.jump , self.jz   , self.jn  ,
            self.ret   , self.end  ,
            self.pc    , self.pn   , self.rc   , self.rn   )

    def __atomicInstruction(self, command):
        self.lexeme(string=command)
        token = self.current()
        return Instruction(token, command)
        
    def __numericInstruction(self, command):
        self.lexeme(string=command)
        token = self.current()
        operand = self.number()
        return Instruction(token, command, operand)
        
    def __labelInstruction(self, command):
        self.lexeme(string=command)
        token = self.current()
        operand = self.identifier()
        return Instruction(token, command, operand)
    
    def push(self):  return self.__numericInstruction("push")
    def dup(self):   return self.__atomicInstruction("dup")
    def copy(self):  return self.__numericInstruction("copy")
    def swap(self):  return self.__atomicInstruction("swap")
    def pop(self):   return self.__atomicInstruction("pop")
    def slide(self): return self.__numericInstruction("slide")
        
    def add(self): return self.__atomicInstruction("add")
    def sub(self): return self.__atomicInstruction("sub")
    def mul(self): return self.__atomicInstruction("mul")
    def div(self): return self.__atomicInstruction("div")
    def mod(self): return self.__atomicInstruction("mod")
    
    def store(self): return self.__atomicInstruction("store")
    def load(self):  return self.__atomicInstruction("load")
    
    def label(self): return self.__labelInstruction("label")
    def call(self):  return self.__labelInstruction("call")
    def jump(self):  return self.__labelInstruction("jump")
    def jz(self):    return self.__labelInstruction("jz")
    def jn(self):    return self.__labelInstruction("jn")
    def ret(self):   return self.__atomicInstruction("ret")
    def end(self):   return self.__atomicInstruction("end")
    
    def pc(self): return self.__atomicInstruction("pc")
    def pn(self): return self.__atomicInstruction("pn")
    def rc(self): return self.__atomicInstruction("rc")
    def rn(self): return self.__atomicInstruction("rn")
    