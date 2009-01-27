module Compiler (compile) where

import Ast
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (mapAccumR, elemIndex)

compile :: Program -> [Instruction]
compile p = reverse $ code $ execState (compileProgram p) (initialState p)

data CompilerState = CompilerState { heapTable    :: [(String, Integer)],
                                     macroTable   :: [(String, [Instruction])],
                                     labelCounter :: Integer,
                                     code         :: [Instruction] }

type Compiler a = State CompilerState a

initialState :: Program -> CompilerState
initialState p = CompilerState { heapTable = buildHeapTable p,
                                 macroTable = buildMacroTable p,
                                 labelCounter = 0,
                                 code = [] }

buildMacroTable :: Program -> [(String, [Instruction])]
buildMacroTable p = map makeEntry (programMacros p)
                  where makeEntry (AssemblyMacro name _ body) = (name, body)
                              
buildHeapTable :: Program -> [(String, Integer)]
buildHeapTable = snd . mapAccumR assignAddress 1 . programVariables
    where   assignAddress address (ArrayVariable name values) = 
                (address + (toInteger $ length values), (name, address))

compileProgram :: Program -> Compiler ()
compileProgram p = do
    compileHeap p
    write (Call "main~0")
    write (Pop)
    write (End)
    mapM_ compileFunction (programFunctions p)
    
compileHeap :: Program -> Compiler ()
compileHeap p = do
    let variables = programVariables p
    let items = concat (map arrayValue variables)
    nextFreeAddress <- foldM pushHeapItem 1 items
    write (Push 0)
    write (Push $ nextFreeAddress)
    write (Store)
    
pushHeapItem :: Integer -> Integer -> Compiler Integer
pushHeapItem address value = do
    write (Push address)
    write (Push value)
    write (Store)
    return (address + 1)

compileFunction :: Function -> Compiler ()
compileFunction (Function name params body) = do
    write (Label name)
    compileExpression params 0 body
    write (Ret)

    
compileExpression :: [String] -> Integer -> Expression -> Compiler ()

compileExpression _ _ (Literal i) = write (Push i)

compileExpression parameters stackOffset (Symbol symbol) = do
    if symbol `elem` parameters
        then compileStackLookup parameters stackOffset symbol
        else compileHeapLookup symbol
        
compileExpression parameters stackOffset (CallEx name args) = do
    foldM_ (compileArgument parameters) stackOffset args
    macroTable <- getMacroTable
    case lookup name macroTable of
        Just code -> mapM_ write code
        Nothing -> write (Call name)
    write (Slide $ toInteger $ length args)
        
compileExpression parameters stackOffset (If condition trueValue falseValue) = do
    elseLabel <- genLabel
    endifLabel <- genLabel
    compileExpression parameters stackOffset condition
    write (Jz elseLabel)
    compileExpression parameters stackOffset trueValue
    write (Jump endifLabel)
    write (Label elseLabel)
    compileExpression parameters stackOffset falseValue
    write (Label endifLabel)

compileArgument :: [String] -> Integer -> Expression -> Compiler Integer
compileArgument parameters stackOffset expression = do
    compileExpression parameters stackOffset expression
    return (stackOffset + 1)
    
compileStackLookup :: [String] -> Integer -> String -> Compiler ()
compileStackLookup parameters stackOffset symbol = do
    let stackAddress = (toInteger $ fromJust $ elemIndex symbol $ reverse parameters) + stackOffset
    write (Copy stackAddress)
    
compileHeapLookup :: String -> Compiler ()
compileHeapLookup symbol = do
    heapTable <- getHeapTable
    let heapAddress = fromJust (lookup symbol heapTable)
    write (Push heapAddress)
    write (Load)
    
getHeapTable :: Compiler [(String, Integer)]
getHeapTable = do
    state <- get
    return $ heapTable state
    
getMacroTable :: Compiler [(String, [Instruction])]
getMacroTable = do
    state <- get
    return $ macroTable state
    
genLabel :: Compiler String
genLabel = do
    state <- get
    let counter = labelCounter state
    let label = "~" ++ (show counter)
    put (state { labelCounter = counter + 1 })
    return label

write :: Instruction -> Compiler ()
write i = do
    state <- get
    put $ state { code = i:(code state) }


