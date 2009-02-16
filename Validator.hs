module Validator (getValidationErrors) where

import Ast
import Control.Monad.State
import Data.List (intersperse, sort)


getValidationErrors :: Program -> [String]
getValidationErrors p = errors $ execState (verifyProgram p) (initialState p)



data ValidatorState = ValidatorState { functions :: [String],
                                       assemblyMacros :: [String],
                                       variables :: [String],
                                       errors :: [String] }

type Validator a = State ValidatorState a
                                       
initialState :: Program -> ValidatorState
initialState p = ValidatorState { functions = map functionName $ programFunctions p,
                                  assemblyMacros = map macroName $ programMacros p,
                                  variables = map variableName $ programVariables p,
                                  errors = [] }


verifyProgram :: Program -> Validator ()
verifyProgram p = do
    assertNoVariablesRedefined
    assertNoCallablesRedefined
    mapM_ verifyFunction $ programFunctions p
    assertMainFunctionDefined                        
    
assertNoVariablesRedefined :: Validator ()
assertNoVariablesRedefined = do
    vs <- getVariables
    assertNoDuplicates vs "Duplicate variable definitions: "    
    
assertNoCallablesRedefined :: Validator ()
assertNoCallablesRedefined = do
    fs <- getFunctions
    as <- getAssemblyMacros
    let cs = fs ++ as
    assertNoDuplicates cs "Duplicate function / assembly macro definitions: "    
    
verifyFunction :: Function -> Validator ()
verifyFunction (Function name params body) = do
    assertNoDuplicates params $ "Duplicate parameter in definition of " ++ name ++ ": "
    verifyExpression name params body    
    
assertMainFunctionDefined :: Validator ()
assertMainFunctionDefined = do
    fs <- getFunctions
    if "main~0" `elem` fs
        then return ()
        else insertError $ "Function 'main~0' not defined."
    
verifyExpression :: String -> [String] -> Expression -> Validator ()

verifyExpression _ _ (Literal i) = return ()

verifyExpression sourceFunction sourceParams (Symbol s) = do
    vs <- getVariables
    let definedSymbols = sourceParams ++ vs
    if s `elem` definedSymbols
        then return ()
        else insertError $ "Undefined symbol '" ++ s ++ "' in function '" ++ sourceFunction ++ "'."

verifyExpression sourceFunction sourceParams (CallEx name args) = do
    fs <- getFunctions
    as <- getAssemblyMacros
    let cs = fs ++ as
    if name `elem` cs
        then mapM_ (verifyExpression sourceFunction sourceParams) args
        else insertError $ "Unresolvable function or macro call '" ++ name ++ "' in function '" ++ sourceFunction ++ "'."
        
verifyExpression sourceFunction sourceParams (If condition trueValue falseValue) = do
    verifyExpression sourceFunction sourceParams condition
    verifyExpression sourceFunction sourceParams trueValue
    verifyExpression sourceFunction sourceParams falseValue
        
        
assertNoDuplicates :: [String] -> String -> Validator ()
assertNoDuplicates lst errMsg = do
    let duplicates = findDuplicates lst
    if duplicates == []
        then return ()
        else insertError $ errMsg ++ (concat $ intersperse ", " duplicates)

        
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = recurse (sort xs) []
              where recurse      []  ds = ds
                    recurse   (x:[]) ds = ds
                    recurse (x:y:xs) ds | x == y    = recurse xs (x:ds)
                                        | otherwise = recurse (y:xs) ds
                                        
        
insertError :: String -> Validator ()
insertError msg = do
    state <- get
    put $ state { errors = msg:(errors state) }

getVariables :: Validator [String]
getVariables = do
    state <- get
    return $ variables state
    
getFunctions :: Validator [String]
getFunctions = do
    state <- get
    return $ functions state
    
getAssemblyMacros :: Validator [String]
getAssemblyMacros = do
    state <- get
    return $ assemblyMacros state