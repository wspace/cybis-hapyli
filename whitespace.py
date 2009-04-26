
class WhitespaceError(Exception):
    pass

s = ' '
t = '\t'
n = '\n'

stack = s
arith = t + s
heap  = t + t
flow  = n
io    = t + n

COMMAND_CODES = dict(
    push  = stack + s,
    dup   = stack + n + s,
    copy  = stack + t + s,
    swap  = stack + n + t,
    pop   = stack + n + n,
    slide = stack + t + n,
    
    add = arith + s + s,
    sub = arith + s + t,
    mul = arith + s + n,
    div = arith + t + s,
    mod = arith + t + t,
    
    store = heap + s,
    load  = heap + t,
    
    label = flow + s + s,
    call  = flow + s + t,
    jump  = flow + s + n,
    jz    = flow + t + s,
    jn    = flow + t + t,
    ret   = flow + t + n,
    end   = flow + n + n,
    
    pc = io + s + s,
    pn = io + s + t,
    rc = io + t + s,
    rn = io + t + t,
)    

def translateNumber(value):
    binValue = bin(value)
    wsValue = binValue.replace('0', s).replace('1', t) + n
    return wsValue
    
def bin(value):
    
    if value < 0:
        sign = 1
    else:
        sign = 0
        
    value = abs(value)
    
    if value == 0:
        bits = [0]
    else:
        bits = []
        while value != 0:
            bits.append(value & 0x01)
            value = value >> 1
            
    bits.append(sign)
    bits.reverse()
    
    return ''.join(map(str, bits))

    
def translateAssembly(instructions):
    
    translatedInstructions = []
    labelCodes = buildLabelCodes(instructions)
    
    for (command, operand) in instructions:
        
        commandCode = COMMAND_CODES[command]
        
        if command in ["push", "copy", "slide"]:
            operandCode = translateNumber(operand)
        elif command in ["label", "call", "jump", "jz", "jn"]:
            if operand in labelCodes:
                operandCode = labelCodes[operand]
            else:
                raise WhitespaceError("Label '" + operand + "' not defined.")
        else:
            operandCode = ''
            
        translatedInstructions.append(commandCode + operandCode)
        
    whitespaceProgram = ''.join(translatedInstructions)
    
    return whitespaceProgram

    
def buildLabelCodes(instructions):

    autonumber = 0
    labelCodes = {}

    for (command, operand) in instructions:
        if command == "label":
            if operand in labelCodes:
                raise WhitespaceError("Label '" + operand + "' already defined.")
            else:
                labelCodes[operand] = translateNumber(autonumber)
                autonumber += 1
                
    return labelCodes
