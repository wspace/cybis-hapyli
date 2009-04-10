from HplLexemeParser import HplLexemeParser
from TokenKinds import *
from ast import *

class HplAstParser(HplLexemeParser):

    # variables, modules, program

    def function(self):
        self.reserved("def")
        token = self.current()
        name = self.identifier()
        params = self.parameterList()
        self.lexeme(string='=')
        bindings = self.optional(self.letForm)
        if bindings == None:
            bindings = []
        body = self.expression()
        return Function(token, name, params, bindings, body)
        
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
    
    def macro(self):
        self.reserved("asm")
        token = self.current()
        name = self.identifier()
        params = self.parameterList()
        self.lexeme(string='=')
        self.lexeme(string='(')
        body = self.many(self.instruction)
        self.lexeme(string=')')
        return Macro(token, name, params, body)
        
    def parameterList(self):
        self.lexeme(string='(')
        params = self.many(self.identifier)
        self.lexeme(string=')')
        return params
    
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
    
    def push(self):  self.__numericInstruction("push")
    def dup(self):   self.__atomicInstruction("dup")
    def copy(self):  self.__numericInstruction("copy")
    def swap(self):  self.__atomicInstruction("swap")
    def pop(self):   self.__atomicInstruction("pop")
    def slide(self): self.__numericInstruction("slide")
        
    def add(self): self.__atomicInstruction("add")
    def sub(self): self.__atomicInstruction("sub")
    def mul(self): self.__atomicInstruction("mul")
    def div(self): self.__atomicInstruction("div")
    def mod(self): self.__atomicInstruction("mod")
    
    def store(self): self.__atomicInstruction("store")
    def load(self):  self.__atomicInstruction("load")
    
    def label(self): self.__labelInstruction("label")
    def call(self):  self.__labelInstruction("call")
    def jump(self):  self.__labelInstruction("jump")
    def jz(self):    self.__labelInstruction("jz")
    def jn(self):    self.__labelInstruction("jn")
    def ret(self):   self.__atomicInstruction("ret")
    def end(self):   self.__atomicInstruction("end")
    
    def pc(self): self.__atomicInstruction("pc")
    def pn(self): self.__atomicInstruction("pn")
    def rc(self): self.__atomicInstruction("rc")
    def rn(self): self.__atomicInstruction("rn")
    