module Overloader (overloadProgram) where
import Ast

overloadProgram :: Program -> Program
overloadProgram (Program vs cs) = 
    Program vs (map overload cs)
    
overload :: Callable -> Callable
overload (Function name params bindings body) =
    let (letSymbols, letExprs) = unzip bindings
    in  Function (name ++ "~" ++ (show . length) params)
                 (params)
                 (zip letSymbols $ map overloadExpression letExprs)
                 (overloadExpression body)
                 
overload (Macro name params body) =
    Macro (name ++ "~" ++ (show . length) params) 
          params
          body
                 
overloadExpression :: Expression -> Expression
overloadExpression (IntegerLiteral i) = IntegerLiteral i
overloadExpression (StringLiteral str) = StringLiteral str
overloadExpression (Symbol name) = Symbol name

overloadExpression (CallEx name args) = 
    CallEx (name ++ "~" ++ (show . length) args) args
    
overloadExpression (If c t f) = 
    If (overloadExpression c)
       (overloadExpression t)
       (overloadExpression f)
       
overloadExpression (Do exps) =
    Do (map overloadExpression exps)
