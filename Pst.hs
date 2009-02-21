module Pst where

import qualified Ast as Ast

type Name = Ast.Name
type Address = Integer
type Size = Integer

data Program = Program [Array] [Function]

data Array = InitializedArray Address [Integer]
           | UninitializedArray Address Size

data Function = Function Name [Expression]

data Expression = IntegerLiteral Integer
                | HeapSymbol Address
                | StackSymbol Address
                | FunctionCall Name [Expression]
                | Macro [Expression] [Ast.Instruction]
                | If Expression Expression Expression
                | Do [Expression]
                
instance Show Program where
  
    show (Program variables functions) =
        (unlines $ map show variables) ++ "\n" ++
        (unlines $ map show functions)
        
instance Show Array where

    show (InitializedArray addr xs) =
        "var #:" ++ (show addr) ++ " = (" ++ (unwords $ map show xs) ++ ")"
        
    show (UninitializedArray addr len) =
        "var #:" ++ (show addr) ++ "[" ++ (show len) ++ "]"
        
instance Show Function where

    show (Function name body) = 
        "def " ++ name ++ " = \n" ++ (unlines $ map show body) ++ "\n"
        
instance Show Expression where

    show (IntegerLiteral i) = (show i)
    show (HeapSymbol addr) = "#heap:" ++ (show addr)
    show (StackSymbol addr) = "#stack:" ++ (show addr)
    
    show (FunctionCall name args) = 
        "(" ++ name ++ " " ++ (unwords $ map show args) ++ ")"
        
    show (Macro args instructions) = 
        "(# " ++ (show $ Do args) ++ 
        (unlines $ map show instructions) ++ ")\n"
        
    show (If c t f) = 
        "(if " ++ (show c) ++ "\n" ++ (show t) ++ "\n" ++ (show f) ++ ")\n"
        
    show (Do exps) = 
        "(do " ++ (unlines $ map show exps) ++ ")\n"
        
