from escape import escape

class Program:

    def __init__(self, token, variables, routines):
        self.token = token
        self.variables = []
        self.routines = []
        
    def __repr__(self):
        return ('\n'.join(map(repr, self.variables)) +
                '\n'.join(map(repr, self.routines)))
        
class Module:

    def __init__(self, token, imports, variables, routines):
        self.token = token
        self.imports = imports
        self.variables = variables
        self.routines = routines
        
    def __repr__(self):
        return ('\n'.join(map(repr, self.imports)) +
                '\n'.join(map(repr, self.variables)) +
                '\n'.join(map(repr, self.routines)))
        
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
        return 'var ' + self.name + '[' + str(self.size) + ']'
        
class Routine:
    pass
    
class Function(Routine):
    
    def __init__(self, token, name, parameters, bindings, body):
        self.token = token
        self.name = name
        self.parameters = parameters
        self.bindings = bindings
        self.body = body

    def __repr__(self):
        
        header = 'def ' + self.name + ' (' + ' '.join(self.parameters) + ') = '

        if self.bindings == []:
            letForm = ''
        else:
            letForm = ('let\n' + 
                       '\n'.join([name + ' = ' + repr(value) 
                                 for (name, value) in self.bindings]) + '\n' +
                       'in ')
        
        body = repr(self.body)
        
        return header + letForm + body + '\n'
    
        
class Macro(Routine):

    def __init__(self, token, name, parameters, body):
        self.token = token
        self.name = name
        self.parameters = parameters
        self.body = body
        
    def __repr__(self):
        header = 'asm ' + self.name + ' (' + ' '.join(self.parameters) + ') = '
        body = '(\n' + '\n'.join(map(repr, self.body)) + ')'
        return header + body + '\n'
        
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