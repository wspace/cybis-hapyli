module Parser (parseModule, parseAssembly) where

import Lexer
import Ast
import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import Data.Char (ord)

parseModule :: FilePath -> String -> Module
parseModule location input = unwrap $ parse programModule location input
                           
parseAssembly :: FilePath -> String -> [Instruction]
parseAssembly location input = unwrap $ parse (many instruction) location input
                                 
unwrap :: Either ParseError a -> a
unwrap (Right result) = result
unwrap (Left err) = error $ show err                                
                                 
programModule :: Parser Module
programModule = do
    whiteSpace
    elements <- many $ lexeme element
    eof
    return $ fromElements elements

element :: Parser Element
element = (moduleImport  >>= \i -> return $ Import i) <|>
          (variable      >>= \v -> return $ Var v)   <|>
          (assemblyMacro >>= \a -> return $ Asm a)   <|>
          (function      >>= \f -> return $ Def f)   <?>
          "import, var, asm, def"
    
moduleImport :: Parser FilePath
moduleImport = do
    symbol "import"
    file <- stringLiteral
    return file

variable :: Parser Variable
variable = do
    symbol "var"
    name <- identifier
    symbol "="
    ((number        >>= \n   -> return $ IntegerVariable name n)   <|>
     (array         >>= \ns  -> return $ ArrayVariable name ns)    <|>
     (stringLiteral >>= \str -> return $ StringVariable name str))
        
array :: Parser [Integer]
array = parens $ many1 number    

function :: Parser Function
function = do
    symbol "def"
    name <- identifier
    params <- parens $ many identifier
    symbol "="
    body <- expression
    return $ Function name params body
    
assemblyMacro :: Parser AssemblyMacro
assemblyMacro = do
    symbol "asm"
    name <- identifier
    params <- parens $ many identifier
    symbol "="
    body <- parens $ many instruction
    return $ AssemblyMacro name params body
    
expression :: Parser Expression
expression =   literalExpression
           <|> symbolExpression
           <|> ifExpression
           <|> functionCall
           <?> "expression"
           
literalExpression :: Parser Expression
literalExpression = do
    n <- number
    return $ Literal n

symbolExpression :: Parser Expression
symbolExpression = do
    name <- identifier
    return $ Symbol name
        
ifExpression :: Parser Expression    
ifExpression = do
    (try $ symbol "(" >> 
           symbol "if")
    condition <- expression
    trueValue <- expression
    falseValue <- expression
    symbol ")"
    return $ If condition trueValue falseValue
           
functionCall :: Parser Expression
functionCall = parens $ do
    name <- identifier
    args <- many expression
    return $ FunctionCall name args
    
instruction :: Parser Instruction         
instruction =   withOperand "push" number Push
            <|> atomic "dup" Dup
            <|> withOperand "copy" number Copy
            <|> atomic "swap" Swap
            <|> atomic "pop" Pop
            <|> withOperand "slide" number Slide
            <|> atomic "add" Add
            <|> atomic "sub" Sub
            <|> atomic "mul" Mul
            <|> atomic "div" Div
            <|> atomic "mod" Mod
            <|> atomic "store" Store
            <|> atomic "load" Load
            <|> withOperand "label" identifier Label
            <|> withOperand "call" identifier Call
            <|> withOperand "jump" identifier Jump
            <|> withOperand "jz" identifier Jz
            <|> withOperand "jn" identifier Jn
            <|> atomic "ret" Ret
            <|> atomic "end" End
            <|> atomic "pc" Pc
            <|> atomic "pn" Pn
            <|> atomic "rc" Rc
            <|> atomic "rn" Rn
            <?> "instruction"

atomic :: String -> Instruction -> Parser Instruction
atomic name constructor = try $ do
    symbol name
    return constructor

withOperand :: String -> Parser a -> (a -> Instruction) -> Parser Instruction
withOperand name operand constructor = try $ do
    symbol name
    n <- operand
    return $ constructor n
    
number :: Parser Integer
number = integer <|> (toInteger . ord) `liftM` charLiteral
