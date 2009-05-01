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
    
    def __init__(self, token, name, size, data):
        self.token = token
        self.name = name
        self.size = size
        self.data = data
        
    def __repr__(self):
        raise NotImplementedError()
    
class IntegerVariable(Variable):

    def __init__(self, token, name, value):
        self.__value = value
        Variable.__init__(self, token, name, 1, [value])
        
    def __repr__(self):
        return 'var ' + self.name + ' = ' + str(self.__value)
        
class ArrayVariable(Variable):
    
    def __init__(self, token, name, values):
        Variable.__init__(self, token, name, len(values), values)
        
    def __repr__(self):
        return 'var ' + self.name + ' = {' + ' '.join(map(str, self.data)) + '}'
        
class StringVariable(Variable):

    def __init__(self, token, name, string):
        self.__string = string
        size = len(string) + 1
        data = map(ord, string) + [0]
        Variable.__init__(self, token, name, size, data)
        
    def __repr__(self):
        return 'var ' + self.name + ' = "' + escape(self.__string) + '"'
        
class UninitializedVariable(Variable):

    def __init__(self, token, name, size):
        Variable.__init__(self, token, name, size, [])

    def __repr__(self):
        return 'var ' + self.name + '[' + str(self.size) + ']'
        
class Function:
    
    def __init__(self, token, inline, name, parameters, bindings, body):
        self.token = token
        self.inline = inline
        self.name = name
        self.parameters = parameters
        self.bindings = bindings 
        self.body = body
        self.signature = name + '~' + str(len(parameters))
                
    def __repr__(self):

        if self.inline:
            strInline = "inline "
        else:
            strInline = ''

        name = self.name + ' '
        strParams = '(' + ' '.join(self.parameters) + ')'
            
        if self.bindings:
            strLetForm = ("let\n" +
                          '\n'.join([name + ' = ' + str(value) 
                                     for (name, value) in self.bindings]) + '\n' +
                          "in ")
        else:
            strLetForm = ''

        strBody = repr(self.body) + '\n'
            
        return "def " + strInline + name + strParams + " = " + strLetForm + strBody

class AssemblyBlock:
    
    def __init__(self, token, instructions):
        self.token = token
        self.instructions = instructions
        
    def __repr__(self):
        return '{\n' + '\n'.join(map(repr, self.instructions)) + '\n}'
        
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
        self.signature = name + '~' + str(len(arguments))

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