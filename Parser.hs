module Parser (parseModule) where

import Lexer
import Ast
import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import Data.Char (ord)

parseModule :: String -> String -> Module
parseModule file text = unwrap $ parse programModule file text
                  where unwrap (Right result) = result
                        unwrap (Left err) = error $ (show err)

programModule :: Parser Module
programModule = do
    elements <- many element
    let m = fromElements elements
    return m

element :: Parser Element
element =   importElement 
        <|> variableElement
        <|> callableElement
        <?> "element"
        
importElement :: Parser Element
importElement = do
    reserved "import"
    file <- stringLiteral
    return $ ImportElement file
    
variableElement :: Parser Element
variableElement = do
    v <- variable
    return $ VariableElement v
    
callableElement :: Parser Element
callableElement = do
    c <- callable
    return $ CallableElement c

variable :: Parser Variable
variable = 
    do reserved "var"
       name <- identifier
       (uninitializedVariable name <|> 
        initializedVariable name)

uninitializedVariable :: String -> Parser Variable
uninitializedVariable name = squares $ do
    length <- number
    return $ UninitializedVariable name length    
            
initializedVariable :: String -> Parser Variable
initializedVariable name = do
    symbol "="
    (integerVariable name <|>
     arrayVariable name <|>
     stringVariable name)
          
integerVariable :: String -> Parser Variable
integerVariable name = do
    n <- number
    return $ IntegerVariable name n

arrayVariable :: String -> Parser Variable
arrayVariable name = do
    ns <- many number
    return $ ArrayVariable name ns
    
stringVariable :: String -> Parser Variable    
stringVariable name = do
    str <- stringLiteral
    return $ StringVariable name str
            
callable :: Parser Callable
callable =   function
         <|> macro
         <?> "function or macro"
         
function :: Parser Callable
function = do
    reserved "def"
    name <- identifier
    params <- parens (many identifier)
    symbol "="
    bindings <- (letForm <|> return [])
    body <- expression
    return $ Function name params bindings body

letForm :: Parser [Binding]
letForm = do reserved "let"
             bindings <- many binding
             reserved "in"
             return bindings
  where binding = do
          name <- identifier
          symbol "="
          value <- expression
          return (name, value)
    
macro :: Parser Callable
macro = do
    reserved "asm"
    name <- identifier
    params <- parens (many identifier)
    symbol "="
    instructions <- parens (many instruction)
    return $ Macro name params instructions
    
expression :: Parser Expression
expression =  integerLiteralEx
          <|> stringLiteralEx
          <|> symbolEx
          <|> ifEx
          <|> doEx
          <|> callEx
          <?> "expression"
          
integerLiteralEx :: Parser Expression
integerLiteralEx = number >>= \n -> 
                   return $ IntegerLiteral n

stringLiteralEx :: Parser Expression
stringLiteralEx = stringLiteral >>= \str -> 
                  return $ StringLiteral str
          
symbolEx :: Parser Expression
symbolEx = identifier >>= \name -> 
           return $ Symbol name
           
ifEx :: Parser Expression
ifEx = do
    (try $ symbol "(" >> reserved "if")
    condition <- expression
    trueValue <- expression
    falseValue <- expression
    symbol ")"
    return $ If condition trueValue falseValue
    
doEx :: Parser Expression
doEx = do
    (try $ symbol "(" >> reserved "do")
    expressions <- many1 expression
    symbol ")"
    return $ Do expressions
          
callEx :: Parser Expression
callEx = do
    symbol "("
    name <- identifier
    args <- many expression
    symbol ")"
    return $ CallEx name args

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

array :: Parser [Integer]
array = parens $ many1 number 

