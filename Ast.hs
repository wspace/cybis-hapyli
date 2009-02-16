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
          

