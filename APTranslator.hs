module APTranslator where

import qualified Ast as Ast
import Ast (variableName, variableLength, callableName)
import qualified Pst as Pst
import Data.List (elemIndex)
import Data.Char (ord)

type StackTable = [String]
type HeapTable = [(String, Integer)]
type CallTable = [(String, Ast.Callable)]

data Environment = Environment {  source :: String
                                , heap :: HeapTable
                                , stack :: StackTable
                                , callTable :: CallTable }

                                
processProgram :: Ast.Program -> Pst.Program
processProgram (Ast.Program variables callables) = 
    let callTable = buildCallTable callables
        heapTable = buildHeapTable variables
        processedVars = processVariables variables
        processedFuncs = processCallables heapTable callTable callables
    in Pst.Program processedVars processedFuncs
                                
                                
buildHeapTable :: [Ast.Variable] -> HeapTable
buildHeapTable = recurse [] 0 0
    where recurse :: HeapTable -> Integer -> Integer -> [Ast.Variable] -> HeapTable
          recurse heap _ _ [] = heap
          recurse heap addr len (v:vs) = 
            recurse ((variableName v, addr + len):heap) (addr + len) (variableLength v) vs

            
buildCallTable :: [Ast.Callable] -> CallTable
buildCallTable cs = zip (map callableName cs) cs
            
                                
processVariables :: [Ast.Variable] -> [Pst.Array]
processVariables = map processVariable
                                
                                
processVariable :: Ast.Variable -> Pst.Array
processVariable (Ast.IntegerVariable _ i) = Pst.InitializedArray [i]
processVariable (Ast.ArrayVariable _ is) = Pst.InitializedArray is
processVariable (Ast.StringVariable _ str) = Pst.InitializedArray $ map (toInteger . ord) str ++ [0]
processVariable (Ast.UninitializedVariable _ size) = Pst.UninitializedArray size


processCallables :: HeapTable -> CallTable -> [Ast.Callable] -> [Pst.Function]
processCallables heap callTable = map (processCallable heap callTable)

                                
processCallable :: HeapTable ->
                   CallTable ->
                   Ast.Callable ->
                   Pst.Function
                   
processCallable heap callTable (Ast.Function name params bindings body) = 
    let letEnv = Environment name heap (reverse params) callTable
        (bodyEnv, processedLetExpressions) = processBindings letEnv bindings
        processedBody = processExpression bodyEnv 0 body
    in  Pst.Function name (processedLetExpressions ++ [processedBody])
        
processCallable _ _ (Ast.Macro name _ _) = 
    error $ "Can't process macro definition '" ++ name ++ "'."

    
processBindings :: Environment -> [Ast.Binding] -> (Environment, [Pst.Expression])
processBindings env [] = (env, [])
processBindings env ((name, value):bindings) = 
    let prex = processExpression env 0 value
        env' = env { stack = name:(stack env) }
        (env'', prexs) = processBindings env' bindings
    in (env'', prex:prexs)

    
processExpressions :: Environment -> Integer -> [Ast.Expression] -> [Pst.Expression]
processExpressions env stackOffset [] = []
processExpressions env stackOffset (e:es) = (processExpression env stackOffset e) :
                                            (processExpressions env (stackOffset + 1) es)
    
    
processExpression :: Environment ->
                     Integer ->
                     Ast.Expression ->
                     Pst.Expression
                     
processExpression _ _ (Ast.IntegerLiteral i) = Pst.IntegerLiteral i

processExpression env _ (Ast.StringLiteral str) = 
    error $ "In '" ++ (source env) ++ "', can't process inline string."
    
processExpression env stackOffset (Ast.Symbol name) = 
    case elemIndex name (stack env) of
        Just index -> Pst.StackSymbol (toInteger index + stackOffset)
        Nothing ->
            case lookup name (heap env) of
                Just address -> Pst.HeapSymbol address
                Nothing -> 
                    error $ "In '" ++ (source env) ++ "', undefined symbol '" ++ name ++ "'."
                
processExpression env stackOffset (Ast.CallEx name args) = 
    let processedArgs = processExpressions env stackOffset args
    in  case lookup name (callTable env) of
        Just (Ast.Function _ _ _ _) -> 
            Pst.FunctionCall name processedArgs
        Just (Ast.Macro _ _ body) ->
            Pst.Macro processedArgs body
        Nothing ->
            error $ "In '" ++ (source env) ++ "', undefined function or macro '" ++ name ++ "'."
            
processExpression env stackOffset (Ast.If condition trueValue falseValue) =
    Pst.If (processExpression env stackOffset condition)
           (processExpression env (stackOffset + 1) trueValue)
           (processExpression env (stackOffset + 2) falseValue)
           
processExpression env stackOffset (Ast.Do exps) = 
    Pst.Do (processExpressions env stackOffset exps)

    
                                            