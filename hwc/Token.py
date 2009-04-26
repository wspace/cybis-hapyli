
class Token:

    def __init__(self):
        self.file = ''
        self.line = ''
        self.lineNumber = 0
        self.index = 0
        self.kind = ''
        self.string = ''

    def __repr__(self):
        fileInfo = "In: " + self.file + '\n'
        tokenInfo = str(self.kind) + ": " + self.string + '\n'
        lineInfo = str(self.lineNumber) + ": " + self.line + '\n'
        caretOffset = len(str(self.lineNumber)) + 2 + self.index
        indexInfo = ' '*caretOffset + '^'
        return fileInfo + tokenInfo + lineInfo + indexInfo
        
