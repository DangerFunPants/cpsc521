module Instruction (SECDInstruction (..)) where

data SECDInstruction
 = Closure  [SECDInstruction]
 | Fix      [SECDInstruction]
 | FixC     [SECDInstruction]
 | App
 | Access Int
 | Ret
 -- Arithmetic Instructions
 | Const Int
 | Add
 | Mul
 | LEq
 | Eq
 -- Boolean Instructions
 | True
 | False
 | IfThenElse
 -- List Instructions
 | Cons
 | Nil
 | Case
 deriving (Show, Eq)
