module Instruction (SECDInstruction (..)) where

data SECDInstruction
 = Closure [SECDInstruction]
 | App
 | Access Int
 | Ret
 -- Arithmetic Instructions
 | Const Int
 | Add
 | Mul
 | LEq
 -- Boolean Instructions
 | True
 | False
 | IfThenElse
 | Cons
 | Nil
 | Case
 deriving (Show, Eq)
