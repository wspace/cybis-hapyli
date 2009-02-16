module Ast where

data Program = Program { programVariables  :: [Variable], 
                         programMacros     :: [AssemblyMacro],
                         programFunctions  :: [Function] }

data Module = Module { moduleImports   :: [FilePath],
                       moduleVariables :: [Variable],
                       moduleMacros    :: [AssemblyMacro], 
                       moduleFunctions :: [Function] }

data Element = Import FilePath
             | Var Variable
             | Asm AssemblyMacro
             | Def Function
             
data Variable = IntegerVariable { variableName :: String, 
                                  integerValue :: Integer }
              | ArrayVariable   { variableName :: String,
                                  arrayValue :: [Integer] }
              | StringVariable  { variableName :: String,
                                  stringValue :: String }
             
data AssemblyMacro = AssemblyMacro { macroName :: String,
                                     macroParameters :: [String],
                                     macroBody :: [Instruction] }

data Function = Function { functionName :: String,
                           functionParameters :: [String], 
                           functionBody :: Expression }
             
data Expression = Literal Integer
                | Symbol String
                | CallEx String [Expression]
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


instance Show Program where
    show (Program variables macros functions) = 
        (unlines $ map show variables) ++ "\n" ++
        (unlines $ map show macros) ++ "\n" ++
        (unlines $ map show functions)

        
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
    show (CallEx name args) = "(" ++ name ++ " " ++ (unwords $ map show args) ++ ")"
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


fromModules :: [Module] -> Program
fromModules modules = recurse modules [] [] []
                where recurse :: [Module] ->
                                 [Variable] ->
                                 [AssemblyMacro] ->
                                 [Function] ->
                                 Program
                      recurse                   []  vs as fs = Program vs as fs
                      recurse ((Module _ v a f):ms) vs as fs = recurse ms (v++vs) (a++as) (f++fs)


