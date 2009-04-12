from HplLexer import HplLexer
from HplAstParser import HplAstParser
from ast import Program

def loadProgram(mainFile):

    knownFiles = [mainFile]
    loadedModules = []
    index = 0
    
    while index < len(knownFiles):

        nextFile = knownFiles[index]
        nextModule = loadModule(nextFile)
        importedFiles = [i.file for i in nextModule.imports]
        newFiles = filter(lambda f: not f in knownFiles, importedFiles)
        knownFiles.extend(newFiles)

        loadedModules.append(nextModule)
        index += 1

    variables = []
    routines = []
    
    for m in loadedModules:
        variables.extend(m.variables)
        routines.extend(m.routines)

    return Program(variables, routines)

def loadModule(file):
    lex = HplLexer()
    tokens = lex.tokenizeFile(file)
    parser = HplAstParser(tokens)
    module = parser.module()
    return module

def writeCompiledAssembly(assembly, file):
    
    stream = open(file, "wb")
    
    for (command, operand) in assembly:
        stream.write(command + ' ' + str(operand) + '\n')

    stream.close()

def writeWhitespace(text, file):
    stream = open(file, "wb")
    stream.write(text)
    stream.close()
