from escape import escape

class Program:

    def __init__(self, variables, functions):
        self.variables = variables
        self.functions = functions
        
    def __repr__(self):
        return ('\n'.join(map(repr, self.variables)) + '\n\n' +
                '\n'.join(map(repr, self.functions)))
        
class Module:

    def __init__(self, imports, variables, functions):
        self.imports = imports
        self.variables = variables
        self.functions = functions
        
    def __repr__(self):
        return ('\n'.join(map(repr, self.imports)) + '\n\n' +
                '\n'.join(map(repr, self.variables)) + '\n\n' +
                '\n'.join(map(repr, self.functions)))
        
class Import:

    def __init__(self, token, file):
        self.token = token
        self.file = file
        
    def __repr__(self):
        return 'import "' + self.file + '"'
        
class Variable:
    pass
    
class IntegerVariable(Variable):

    def __init__(self, token, name, value):
        self.token = token
        self.name = name
        self.value = value
        
    def __repr__(self):
        return 'var ' + self.name + ' = ' + str(self.value)
        
class ArrayVariable(Variable):
    
    def __init__(self, token, name, values):
        self.token = token
        self.name = name
        self.values = values
        
    def __repr__(self):
        return 'var ' + self.name + ' = (' + ' '.join(map(str, self.values)) + ')'
        
class StringVariable(Variable):

    def __init__(self, token, name, string):
        self.token = token
        self.name = name
        self.string = string
        
    def __repr__(self):
        return 'var ' + self.name + ' = "' + escape(self.string) + '"'
        
class UninitializedVariable(Variable):

    def __init__(self, token, name, size):
        self.token = token
        self.name = name
        self.size = size

    def __repr__(self):
        return 'var ' + self.name + '(' + str(self.size) + ')'
        
class Function:
    
    def __init__(self, token, inline, name, parameters, bindings, body):
        self.token = token
        self.inline = inline
        self.name = name
        self.parameters = parameters
        self.bindings = bindings 
        self.body = body
        
    def makeRepr(self, strFuncType, strBody):
        
        strFuncType = strFuncType + " "
        strBody = strBody + "\n"
        
        if self.inline:
            strInline = "inline "
        else:
            strInline = ""
            
        strSignature = self.name + " (" + ' '.join(self.parameters) + ") = "
        
        if self.bindings == []:
            strLetForm = ''
        else:
            strLetForm = ("let\n" +
                          '\n'.join([name + ' = ' + str(value) 
                                     for (name, value) in self.bindings]) + '\n' +
                          "in ")
        
        return strFuncType + strInline + strSignature + strLetForm + strBody
    
class HplFunction(Function):
    
    def __repr__(self):
        return self.makeRepr("def", repr(self.body))
        
class AsmFunction(Function):

    def __repr__(self):
        strBody = "(\n" + '\n'.join([str(ins) for ins in self.body]) + "\n)"
        return self.makeRepr("asm", strBody)
        
class Expression:
    pass
    
class IntegerLiteralEx(Expression):
    
    def __init__(self, token, value):
        self.token = token
        self.value = value
        
    def __repr__(self):
        return str(self.value)
        
class StringLiteralEx(Expression):

    def __init__(self, token, string):
        self.token = token
        self.string = string
        
    def __repr__(self):
        return '"' + escape(self.string) + '"'
        
class SymbolEx(Expression):

    def __init__(self, token, name):
        self.token = token
        self.name = name
        
    def __repr__(self):
        return self.name
        
class IfEx(Expression):

    def __init__(self, token, condition, trueValue, falseValue):
        self.token = token
        self.condition = condition
        self.trueValue = trueValue
        self.falseValue = falseValue
        
    def __repr__(self):
        return ('(if ' + repr(self.condition) + '\n' +
                         repr(self.trueValue) + '\n' + 
                         repr(self.falseValue) + ')')
        
class DoEx(Expression):

    def __init__(self, token, expressions):
        self.token = token
        self.expressions = expressions

    def __repr__(self):
        return ('(do ' + '\n'.join(map(repr, self.expressions)) + ')')
        
class CallEx(Expression):
    
    def __init__(self, token, name, arguments):
        self.token = token
        self.name = name
        self.arguments = arguments

    def __repr__(self):
        return '(' + self.name + ' ' + ' '.join(map(repr, self.arguments)) + ')'
        
class Instruction:
    
    def __init__(self, token, command, operand=None):
        self.token = token
        self.command = command
        self.operand = operand

    def __repr__(self):
        if self.operand == None:
            return self.command
        else:
            return self.command + ' ' + str(self.operand)