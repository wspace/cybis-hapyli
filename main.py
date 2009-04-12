from hplio import loadProgram, writeCompiledAssembly, writeWhitespace
from compiler import compileProgram
from assembler import translateToWhitespace
from sys import argv

def printUsage():

    message = '\n'
    message += "             HaPyLi -> Whitespace Compiler             \n"
    message += "                   By Kevin Gundlach                   \n"               
    message += "Usage: python main.py [-asm] <input file> <output file>\n"
    print message
    
    
if __name__ == "__main__":
    
    if len(argv) == 3:
        inputFile = argv[1]
        outputFile = argv[2]
        writeAsAssembly = False
    elif len(argv) == 4:
        option = argv[1]
        inputFile = argv[2]
        outputFile = argv[3]
        writeAsAssembly = (option == "-asm")
    else:
        printUsage()
        exit()
        
    program = loadProgram(inputFile)
    assembly = compileProgram(program)
    
    if writeAsAssembly:
        writeCompiledAssembly(assembly, outputFile)
    else:
        whitespace = translateToWhitespace(assembly)
        writeWhitespace(whitespace, outputFile)
        
    print "Success!"
