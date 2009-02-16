module Pst where

import qualified Ast as Ast

type Name = Ast.Name
type Address = Integer
type Size = Integer

data Program = Program [Array] [Function]

data Array = InitializedArray [Integer]
           | UninitializedArray Size

data Function = Function Name [Expression]

data Expression = IntegerLiteral Integer
                | HeapSymbol Address
                | StackSymbol Address
                | FunctionCall Name [Expression]
                | Macro [Expression] [Instruction]
                | If Expression Expression Expression
                | Do [Expression]
                
type Instruction = Ast.Instruction
