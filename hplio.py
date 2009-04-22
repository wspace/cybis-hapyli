from HplLexer import HplLexer
from HplAstParser import HplAstParser
from ast import Program
from os.path import normcase, realpath, dirname, join

def loadProgram(mainFile):

    modules = loadAllModules(mainFile)
    
    variables = []
    functions = []
    
    for m in modules:
        variables.extend(m.variables)
        functions.extend(m.functions)
        
    return Program(variables, functions)

def loadAllModules(fileName):
    
    fullPath = normalize(fileName)
    knownFiles = [fullPath]
    loadedModules = []
    index = 0
    
    while index < len(knownFiles):
        
        nextFile = knownFiles[index]
        nextModule = loadModule(nextFile)
        loadedModules.append(nextModule)
    
        curdir = dirname(nextFile)
    
        imports = [i.file for i in nextModule.imports]
        imports = map(lambda i: join(curdir, i), imports)
        imports = map(normalize, imports)
        imports = filter(lambda i: i not in knownFiles, imports)
        
        knownFiles.extend(imports)
        index += 1
        
    return loadedModules

def normalize(path):
    return normcase(realpath(path))
    
def loadModule(fileName):
    lex = HplLexer()
    tokens = lex.tokenizeFile(fileName)
    parser = HplAstParser(tokens)
    module = parser.module()
    return module

