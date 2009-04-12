
class AssemblerError(Exception):
    pass

s = ' '
t = '\t'
n = '\n'

stack = s
arith = t + s
heap  = t + t
flow  = n
io    = t + n

OPCODES = dict(
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

def numberToWhitespace(value):
    
    if value < 0:
        sign = 1
    else:
        sign = 0
        
    value = abs(value)
    
    if value == 0:
        revbits = [0]
    else:
        revbits = []
        while value != 0:
            bit = value & 0x01
            revbits.append(bit)
            value = value >> 1
    
    revbits.append(sign)
    
    bits = reversed(revbits)
    binstr = ''.join(map(str, bits))
    
    wsstr = binstr.replace('0', s).replace('1', t) + '\n'
        
    return wsstr
    
def translateToWhitespace(assembly):
    
    assembledOpcodes = []
    labelsDefined = []
    
    for (command, operand) in assembly:
        if command == "label":
            if operand in labelsDefined:
                raise AssemblerError("Multiple definitions of label " + operand + " found.")
            else:
                labelsDefined.append(operand)
                
    for (command, operand) in assembly:
        
        opcode = OPCODES[command]
        
        if command in ["push", "copy", "slide"]:
            opcode += numberToWhitespace(operand)
        elif command in ["label", "call", "jump", "jz", "jn"]:
            if operand in labelsDefined:
                opcode += numberToWhitespace(labelsDefined.index(operand))
            else:
                raise AssemblerError("Jump to undefined label " + operand + " found.")
            
        assembledOpcodes.append(opcode)
    
    whitespace = ''.join(assembledOpcodes)
    return whitespace
    
