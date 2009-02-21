module Compiler (compile) where
import Pst
import Ast(Instruction(..))
import Control.Monad.State
import Control.Monad

data CompilerState = CompilerState { heapPtr :: Integer
                                   , labelCounter :: Integer
                                   , code :: [Instruction] }
                                   
type Compiler = State CompilerState



initState :: CompilerState
initState = CompilerState 0 0 []

compile :: Program -> [Instruction]
compile prg = let (_, s) = runState (compileProgram prg) initState
              in (reverse $ code s)

compileProgram :: Program -> Compiler ()
compileProgram (Program heap functions) = do
    compileHeap heap
    mapM_ compileFunction functions

compileHeap :: [Array] -> Compiler ()
compileHeap vars = do
    setHeapPtr 1
    mapM_ compileVariable vars
    heapPtr <- getHeapPtr
    write (Push 0)
    write (Push heapPtr)
    write (Store)
    
compileVariable :: Array -> Compiler ()
compileVariable (InitializedArray _ xs) =
    mapM_ compileHeapItem xs
    
compileHeapItem :: Integer -> Compiler ()
compileHeapItem i = do
    heapPtr <- getHeapPtr
    write (Push heapPtr)
    write (Push i)
    write (Store)
    setHeapPtr (heapPtr + 1)
    
compileFunction :: Function -> Compiler ()
compileFunction (Function name body) = do
    write (Label name)
    mapM_ compileExpression body
    write (Slide $ (toInteger $ length body) - 1)
    write (Ret)

    
    
compileExpression :: Expression -> Compiler ()

compileExpression (IntegerLiteral i) =
    write (Push i)
    
compileExpression (HeapSymbol addr) = do
    write (Push addr)
    write (Load)
    
compileExpression (StackSymbol addr) =
    write (Copy addr)
    
compileExpression (FunctionCall name args) = do
    mapM_ compileExpression args
    write (Call name)
    
compileExpression (Macro args body) = do
    mapM_ compileExpression args
    mapM_ write body
    
compileExpression (If condition trueValue falseValue) = do
    elseLabel <- genLabel
    endLabel <- genLabel
    compileExpression condition
    write (Jz elseLabel)
    compileExpression trueValue
    write (Jump endLabel)
    write (Label elseLabel)
    compileExpression falseValue
    write (Label endLabel)
    
compileExpression (Do body) = do
    mapM_ compileExpression body
    write (Slide $ (toInteger $ length body) - 1)
    
    
    
getHeapPtr :: Compiler Integer
getHeapPtr = do
    state <- get
    return $ heapPtr state
    
setHeapPtr :: Integer -> Compiler ()
setHeapPtr i = do
    state <- get
    put (state {heapPtr = i})
    
genLabel :: Compiler String
genLabel = do
    state <- get
    let id = labelCounter state
    let label = "~label" ++ (show id)
    put (state { labelCounter = id + 1 })
    return label
    
write :: Instruction -> Compiler ()
write i = do
    state <- get
    let is = (code state)
    put $ state { code = (i:is) }

