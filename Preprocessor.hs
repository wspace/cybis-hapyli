module Preprocessor (preprocess) where
import Ast
import Data.Char (ord)


preprocess :: Program -> Program
preprocess (Program vars macros funcs) = 
    Program (map toArrayVariable vars)
            (map overloadMacro macros) 
            (map overloadFunction funcs)

            
toArrayVariable :: Variable -> Variable
toArrayVariable (ArrayVariable name is) = ArrayVariable name is
toArrayVariable (IntegerVariable name i) = ArrayVariable name [i]
toArrayVariable (StringVariable name str) = 
    ArrayVariable name $ strToIntegers str
    where strToIntegers = foldr (\c is -> (toInteger $ ord c):is) [0]



overloadFunction :: Function -> Function
overloadFunction (Function name params body) = 
    Function (overloadName name params)
              params
              (overloadExpression body)

              
overloadMacro :: AssemblyMacro -> AssemblyMacro
overloadMacro (AssemblyMacro name params body) =
    AssemblyMacro (overloadName name params)
                  params
                  body

                  
overloadExpression :: Expression -> Expression

overloadExpression (Literal i) = Literal i
overloadExpression (Symbol str) = Symbol str

overloadExpression (CallEx name args) = 
    CallEx (overloadName name args)
           (map overloadExpression args)
    
overloadExpression (If condition trueValue falseValue) =
    If (overloadExpression condition)
       (overloadExpression trueValue)
       (overloadExpression falseValue)

       
overloadName :: String -> [a] -> String
overloadName name args = name ++ "~" ++ (show $ length args)

