module Ast where

data Module = Module [FilePath] [Variable] [AssemblyMacro] [Function]

data Element = Import FilePath
             | Var Variable
             | Asm AssemblyMacro
             | Def Function
             
data Variable = IntegerVariable String Integer
              | ArrayVariable String [Integer]
              | StringVariable String String
             
data AssemblyMacro = AssemblyMacro String [String] [Instruction]

data Function = Function String [String] Expression             
             
data Expression = Literal Integer
                | Symbol String
                | FunctionCall String [Expression]
                | If Expression Expression Expression
                
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

                 
instance Show Module where
    show (Module imports variables macros functions) = 
        (unlines $ map (("import " ++) . show) imports) ++ "\n" ++
        (unlines $ map show variables) ++ "\n" ++
        (unlines $ map show macros) ++ "\n" ++
        (unlines $ map show functions)

        
instance Show Element where
    show (Import file) = "import " ++ file
    show (Var v) = (show v)
    show (Asm a) = (show a)
    show (Def f) = (show f)

    
instance Show Variable where
    
    show (IntegerVariable name value) =
         "var " ++ name ++ " = " ++ (show value)
         
    show (ArrayVariable name values) =
         "var " ++ name ++ " = (" ++ (unwords $ map show values) ++ ")"
         
    show (StringVariable name str) = 
         "var " ++ name ++ " = " ++ (show str)

         
instance Show AssemblyMacro where

    show (AssemblyMacro name params instructions) =
        "asm " ++ name ++ " (" ++ (unwords params) ++ ") = (\n" ++ 
            (unlines $ map show instructions) ++ ")\n" 

            
instance Show Function where

    show (Function name params body) =
        "def " ++ name ++ " (" ++ (unwords params) ++ ") = " ++ (show body) ++ "\n"

        
instance Show Expression where
    show (Literal value) = (show value)
    show (Symbol name) = name
    show (FunctionCall name args) = "(" ++ name ++ " " ++ (unwords $ map show args) ++ ")"
    show (If condition trueValue falseValue) = "(if " ++ (show condition) ++ "\n"
                                                      ++ (show trueValue) ++ "\n"
                                                      ++ (show falseValue) ++ ")"

                                                      
instance Show Instruction where
    show (Push i) = "push " ++ (show i)
    show Dup = "dup"
    show (Copy i) = "copy " ++ (show i)
    show Swap = "swap"
    show Pop = "pop"
    show (Slide i) = "slide " ++ (show i)
    show Add = "add"
    show Sub = "sub"
    show Mul = "mul"
    show Div = "div"
    show Mod = "mod"
    show Store = "store"
    show Load = "load"
    show (Label lbl) = "label " ++ lbl
    show (Call lbl) = "call " ++ lbl
    show (Jump lbl) = "jump " ++ lbl
    show (Jz lbl) = "jz " ++ lbl
    show (Jn lbl) = "jn " ++ lbl
    show Ret = "ret"
    show End = "end"
    show Pc = "pc"
    show Pn = "pn"
    show Rc = "rc"
    show Rn = "rn"
    

fromElements :: [Element] -> Module
fromElements elements = recurse elements [] [] [] []
                  where recurse :: [Element] ->
                                   [FilePath] ->
                                   [Variable] ->
                                   [AssemblyMacro] ->
                                   [Function] ->
                                   Module
                        recurse             []  is vs as fs = Module is vs as fs
                        recurse ((Import i):es) is vs as fs = recurse es (i:is)    vs     as     fs
                        recurse    ((Var v):es) is vs as fs = recurse es    is  (v:vs)    as     fs
                        recurse    ((Asm a):es) is vs as fs = recurse es    is     vs  (a:as)    fs
                        recurse    ((Def f):es) is vs as fs = recurse es    is     vs     as  (f:fs)




