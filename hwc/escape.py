
def escape(str):

    str = str.replace('\\', '\\\\')
    str = str.replace('\t', '\\t')
    str = str.replace('\r', '\\r')
    str = str.replace('\n', '\\n')
    str = str.replace('\0', '\\0')
    str = str.replace("'", "\\'")
    str = str.replace('"', '\\"')
    
    return str
    

def unescape(str):
    
    chars = []
    escaped = False
    escapeLookup = {'s': ' ',
                    't': '\t',
                    'r': '\r',
                    'n': '\n',
                    '0': '\0'}
    
    for c in str:
        if escaped:
            if c in escapeLookup:
                chars.append(escapeLookup[c])
            else:
                chars.append(c)
            escaped = False
        else:
            if c == '\\':
                escaped = True
            else:
                chars.append(c)
    
    return ''.join(chars)