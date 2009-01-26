module Validator (getValidationErrors) where

import Ast
import Control.Monad.State
import Data.List (intersperse, sort)


getValidationErrors :: Program -> [String]
getValidationErrors p = errors $ execState (verifyProgram p) (initialState p)



data ValidatorState = ValidatorState { currentFunction :: String,
                                       functions :: [String],
                                       assemblyMacros :: [String],
                                       variables :: [String],
                                       parameters :: [String],
                                       errors :: [String] }

type Validator a = State ValidatorState a
                                       
initialState :: Program -> ValidatorState
initialState p = ValidatorState { currentFunction = "",
                                  functions = getFunctions p,
                                  assemblyMacros = getAssemblyMacros p,
                                  variables = getVariables p,
                                  parameters = [],
                                  errors = [] }

getFunctions :: Program -> [String]
getFunctions (Program _ _ funcs) = map getName funcs
    where getName (Function name _ _) = name

getAssemblyMacros :: Program -> [String]
getAssemblyMacros (Program _ macros _) = map getName macros
    where getName (AssemblyMacro name _ _) = name
    
getVariables :: Program -> [String]
getVariables (Program vars _ _) = map getName vars
    where getName (ArrayVariable name _) = name



verifyProgram :: Program -> Validator ()
verifyProgram p@(Program _ _ funcs) = do
    assertNoVariablesRedefined
    assertNoCallablesRedefined
    mapM_ verifyFunction funcs
    assertMainFunctionDefined                        
    
assertNoVariablesRedefined :: Validator ()
assertNoVariablesRedefined = do
    state <- get
    let vs = variables state
    assertNoDuplicates vs "Duplicate variable definitions: "    

assertNoCallablesRedefined :: Validator ()
assertNoCallablesRedefined = do
    state <- get
    let fs = functions state
    let as = assemblyMacros state
    let cs = fs ++ as
    assertNoDuplicates cs "Duplicate function / assembly macro definitions: "    
    
verifyFunction :: Function -> Validator ()
verifyFunction (Function name params body) = do
    setCurrentFunction name params
    assertNoParametersRedefined
    verifyExpression body    
    
assertMainFunctionDefined :: Validator ()
assertMainFunctionDefined = do
    state <- get
    if "main~0" `elem` (functions state)
        then return ()
        else insertError $ "Function 'main~0' not defined"
    
setCurrentFunction :: String -> [String] -> Validator ()
setCurrentFunction name params = do
    state <- get
    put state { currentFunction = name,
                parameters = params }    

assertNoParametersRedefined :: Validator ()
assertNoParametersRedefined = do
    state <- get
    let f = currentFunction state
    let ps = parameters state
    assertNoDuplicates ps $ "Duplicate parameter in definition of " ++ f ++ ": "

    
verifyExpression :: Expression -> Validator ()

verifyExpression (Literal i) = return ()

verifyExpression (Symbol s) = do
    state <- get
    let vs = variables state
    let ps = parameters state
    let symbols = vs ++ ps
    if s `elem` symbols
        then return ()
        else insertError $ "Undefined symbol: " ++ s

verifyExpression (FunctionCall name args) = do
    state <- get
    let fs = functions state
    let as = assemblyMacros state
    let callables = fs ++ as
    if name `elem` callables
        then mapM_ verifyExpression args
        else insertError $ "Undefined function or assembly macro: " ++ name
        
verifyExpression (If condition trueValue falseValue) = do
    verifyExpression condition
    verifyExpression trueValue
    verifyExpression falseValue
        
        
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

