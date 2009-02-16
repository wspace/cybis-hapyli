module Lexer (
    identifier,
    reserved,
    operator,
    reservedOp,
    charLiteral,
    stringLiteral,
    natural,
    integer,
    float,
    naturalOrFloat,
    decimal,
    hexadecimal,
    octal,
    symbol,
    lexeme,
    whiteSpace,
    parens,
    braces,
    angles,
    brackets,
    squares,
    semi,
    comma,
    colon,
    dot,
    semiSep,
    semiSep1,
    commaSep,
    commaSep1
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

hapyliDef = P.LanguageDef { P.commentStart = "",
                            P.commentEnd = "",
                            P.commentLine = ";",
                            P.nestedComments = False,
                            P.identStart = letter <|> oneOf "!@#$%^&*-_=+\\|:,<.>/?",
                            P.identLetter = alphaNum <|> oneOf "!@#$%^&*-_=+\\|:,<.>/?",
                            P.opStart = undefined,
                            P.opLetter = undefined,
                            P.reservedOpNames = [],
                            P.reservedNames = ["import", "var", "def", "asm", "let", "in", "if", "do"],
                            P.caseSensitive = True }
                          
lexer = P.makeTokenParser hapyliDef

identifier      = P.identifier lexer
reserved        = P.reserved lexer
operator        = P.operator lexer
reservedOp      = P.reservedOp lexer
charLiteral     = P.charLiteral lexer
stringLiteral   = P.stringLiteral lexer
natural         = P.natural lexer
integer         = P.integer lexer
float           = P.float lexer
naturalOrFloat  = P.naturalOrFloat lexer
decimal         = P.decimal lexer
hexadecimal     = P.hexadecimal lexer
octal           = P.octal lexer
symbol          = P.symbol lexer
lexeme          = P.lexeme lexer
whiteSpace      = P.whiteSpace lexer
parens          = P.parens lexer
braces          = P.braces lexer
angles          = P.angles lexer
brackets        = P.brackets lexer
squares         = P.squares lexer
semi            = P.semi lexer
comma           = P.comma lexer
colon           = P.colon lexer
dot             = P.dot lexer
semiSep         = P.semiSep lexer
semiSep1        = P.semiSep1 lexer
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
