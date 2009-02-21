module APTranslator (translateProgram) where

import qualified Ast as Ast
import Ast (variableName, variableLength, callableName)
import qualified Pst as Pst
import Data.List (elemIndex)
import Data.Char (ord)
import Data.Maybe (fromJust)

type StackTable = [String]
type HeapTable = [(String, Integer)]
type CallTable = [(String, Ast.Callable)]

data Environment = Environment {  source :: String
                                , heap :: HeapTable
                                , stack :: StackTable
                                , callTable :: CallTable }

                                
translateProgram :: Ast.Program -> Pst.Program
translateProgram (Ast.Program variables callables) = 
    let callTable = buildCallTable callables
        heapTable = buildHeapTable variables
        translatedVars = translateVariables heapTable variables
        functions = [f | f@(Ast.Function _ _ _ _) <- callables]
        translatedFuncs = translateFunctions heapTable callTable functions
    in Pst.Program translatedVars translatedFuncs
                                
                                
buildHeapTable :: [Ast.Variable] -> HeapTable
buildHeapTable = recurse [] 0 0
    where recurse :: HeapTable -> Integer -> Integer -> [Ast.Variable] -> HeapTable
          recurse heap _ _ [] = heap
          recurse heap addr len (v:vs) = 
            recurse ((variableName v, addr + len):heap) (addr + len) (variableLength v) vs

            
buildCallTable :: [Ast.Callable] -> CallTable
buildCallTable cs = zip (map callableName cs) cs
            
                                
translateVariables :: HeapTable -> [Ast.Variable] -> [Pst.Array]
translateVariables ht = map (translateVariable ht)
                                
                                
translateVariable :: HeapTable -> Ast.Variable -> Pst.Array

translateVariable ht (Ast.IntegerVariable name i) = 
    Pst.InitializedArray (addressOf ht name) [i]

translateVariable ht (Ast.ArrayVariable name is) = 
    Pst.InitializedArray (addressOf ht name) is

translateVariable ht (Ast.StringVariable name str) = 
    Pst.InitializedArray (addressOf ht name) $ 
                         map (toInteger . ord) str ++ [0]
    
translateVariable ht (Ast.UninitializedVariable name size) = 
    Pst.UninitializedArray (addressOf ht name) size


addressOf :: HeapTable -> String -> Pst.Address
addressOf ht name = fromJust $ lookup name ht
    
    
translateFunctions :: HeapTable -> CallTable -> [Ast.Callable] -> [Pst.Function]
translateFunctions heap callTable = map (translateFunction heap callTable)

                                
translateFunction :: HeapTable ->
                   CallTable ->
                   Ast.Callable ->
                   Pst.Function
                   
translateFunction heap callTable (Ast.Function name params bindings body) = 
    let letEnv = Environment name heap (reverse params) callTable
        (bodyEnv, translatedLetExpressions) = translateBindings letEnv bindings
        translatedBody = translateExpression bodyEnv 0 body
    in  Pst.Function name (translatedLetExpressions ++ [translatedBody])
        
translateFunction _ _ (Ast.Macro name _ _) = 
    error $ "Can't translate macro definition '" ++ name ++ "'."

    
translateBindings :: Environment -> [Ast.Binding] -> (Environment, [Pst.Expression])
translateBindings env [] = (env, [])
translateBindings env ((name, value):bindings) = 
    let prex = translateExpression env 0 value
        env' = env { stack = name:(stack env) }
        (env'', prexs) = translateBindings env' bindings
    in (env'', prex:prexs)

    
translateExpressions :: Environment -> Integer -> [Ast.Expression] -> [Pst.Expression]
translateExpressions env stackOffset [] = []
translateExpressions env stackOffset (e:es) = (translateExpression env stackOffset e) :
                                            (translateExpressions env (stackOffset + 1) es)
    
    
translateExpression :: Environment ->
                     Integer ->
                     Ast.Expression ->
                     Pst.Expression
                     
translateExpression _ _ (Ast.IntegerLiteral i) = Pst.IntegerLiteral i

translateExpression env _ (Ast.StringLiteral str) = 
    error $ "In '" ++ (source env) ++ "', can't translate inline string."
    
translateExpression env stackOffset (Ast.Symbol name) = 
    case elemIndex name (stack env) of
        Just index -> Pst.StackSymbol (toInteger index + stackOffset)
        Nothing ->
            case lookup name (heap env) of
                Just address -> Pst.HeapSymbol address
                Nothing -> 
                    error $ "In '" ++ (source env) ++ "', undefined symbol '" ++ name ++ "'."
                
translateExpression env stackOffset (Ast.CallEx name args) = 
    let translatedArgs = translateExpressions env stackOffset args
    in  case lookup name (callTable env) of
        Just (Ast.Function _ _ _ _) -> 
            Pst.FunctionCall name translatedArgs
        Just (Ast.Macro _ _ body) ->
            Pst.Macro translatedArgs body
        Nothing ->
            error $ "In '" ++ (source env) ++ "', undefined function or macro '" ++ name ++ "'."
            
translateExpression env stackOffset (Ast.If condition trueValue falseValue) =
    Pst.If (translateExpression env stackOffset condition)
           (translateExpression env (stackOffset + 1) trueValue)
           (translateExpression env (stackOffset + 2) falseValue)
           
translateExpression env stackOffset (Ast.Do exps) = 
    Pst.Do (translateExpressions env stackOffset exps)

    
                                            