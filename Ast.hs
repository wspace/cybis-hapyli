module Ast where

type Name = String
type Parameter = String
type Binding = (String, Expression)
type Length = Integer

data Program = Program [Variable] [Callable]

data Module = Module [FilePath] [Variable] [Callable]

data Element = ImportElement FilePath
             | VariableElement Variable
             | CallableElement Callable

data Variable = IntegerVariable       { variableName :: Name 
                                      , integerValue :: Integer }
              | ArrayVariable         { variableName :: Name 
                                      , arrayValue :: [Integer] }
              | StringVariable        { variableName :: Name 
                                      , stringValue :: String }
              | UninitializedVariable { variableName :: Name 
                                      , uninitializedLength :: Length }

variableLength :: Variable -> Integer
variableLength (IntegerVariable _ _)         = 1
variableLength (ArrayVariable _ xs)          = toInteger (length xs)
variableLength (StringVariable _ str)        = toInteger $ (length str) + 1
variableLength (UninitializedVariable _ len) = len
                                      
data Callable = Function { callableName     :: Name
                         , callableParams   :: [Parameter]
                         , functionBindings :: [Binding]
                         , functionBody     :: Expression }
              | Macro    { callableName   :: Name
                         , callableParams :: [Parameter]
                         , macroBody      :: [Instruction] }

data Expression = IntegerLiteral Integer
                | StringLiteral String
                | Symbol Name
                | CallEx Name [Expression]
                | If Expression Expression Expression
                | Do [Expression]

data Instruction = Push Integer
                 | Dup
                 | Copy Integer
                 | Swap
                 | Pop
                 | Slide Integer
                 | Add
                 | Sub
                 | Mul
                 | Div
                 | Mod
                 | Store
                 | Load
                 | Label String
                 | Call String
                 | Jump String
                 | Jz String
                 | Jn String
                 | Ret
                 | End
                 | Pc
                 | Pn
                 | Rc
                 | Rn

fromElements :: [Element] -> Module
fromElements elements = Module imports variables callables 
    where imports = [i | ImportElement i <- elements]
          variables = [v | VariableElement v <- elements]
          callables = [c | CallableElement c <- elements]
        
fromModules :: [Module] -> Program
fromModules modules = Program variables callables
    where variables = concat $ [v | Module _ v _ <- modules]
          callables = concat $ [c | Module _ _ c <- modules]
          

instance Show Program where
    show (Program variables callables) = 
        (unlines $ map show variables) ++ "\n" ++
        (unlines $ map show callables)
        
instance Show Variable where
    
    show (IntegerVariable name i) = 
        "var " ++ name ++ " = " ++ (show i)
        
    show (ArrayVariable name is) = 
        "var " ++ name ++ " = (" ++ (unwords $ map show is) ++ ")"
        
    show (StringVariable name str) = 
        "var " ++ name ++ " = " ++ (show str)
        
    show (UninitializedVariable name len) = 
        "var " ++ name ++ "[" ++ (show len) ++ "]"
        
instance Show Callable where

    show (Function name params bindings body) = 
        "def " ++ name ++ " (" ++ (unwords params) ++ ") = " ++
        (showBindings bindings) ++
        (show body) ++ "\n"
      where showBindings [] = ""
            showBindings bs = "let " ++ (unlines $ map showBinding bs) ++ " in "
            showBinding (name, value) = name ++ " = " ++ (show value)
            
    show (Macro name params instructions) = 
        "asm " ++ name ++ " (" ++ (unwords params)  ++ ") = (\n" ++
        (unlines $ map show instructions) ++ ")\n"
   
instance Show Expression where

    show (IntegerLiteral i) = (show i)
    show (StringLiteral str) = (show str)
    show (Symbol name) = name
    
    show (CallEx name args) = 
        "(" ++ name ++ " " ++ (unwords $ map show args) ++ ")"

    show (If c t f) = 
        "(if " ++ (show c) ++ "\n" ++ (show t) ++ "\n" ++ (show f) ++ ")\n"
        
    show (Do exps) = 
        "(do " ++ (unlines $ map show exps) ++ ")\n"

instance Show Instruction where
    show (Push i)    = "push " ++ (show i)
    show (Dup)       = "dup"
    show (Copy i)    = "copy " ++ (show i)
    show (Swap)      = "swap"
    show (Pop)       = "pop"
    show (Slide i)   = "slide " ++ (show i)
    show (Add)       = "add"
    show (Sub)       = "sub"
    show (Mul)       = "mul"
    show (Div)       = "div"
    show (Mod)       = "mod"
    show (Store)     = "store"
    show (Load)      = "load"
    show (Label lbl) = "label " ++ lbl
    show (Call lbl)  = "call " ++ lbl
    show (Jump lbl)  = "jump " ++ lbl
    show (Jz lbl)    = "jz " ++ lbl
    show (Jn lbl)    = "jn " ++ lbl
    show (Ret)       = "ret"
    show (End)       = "end"
    show (Pc)        = "pc"
    show (Pn)        = "pn"
    show (Rc)        = "rc"
    show (Rn)        = "rn"
