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
        
    variables = reduce(lambda x,y: x+y, [m.variables for m in loadedModules])
    routines = reduce(lambda x,y: x+y, [m.routines for m in loadedModules])

    return Program(variables, routines)

def loadModule(file):
    lex = HplLexer()
    tokens = lex.tokenizeFile(file)
    parser = HplAstParser(tokens)
    module = parser.module()
    return module

