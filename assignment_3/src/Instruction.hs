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
 | Sub
 | Mul
 | Div
 | LEq
 | Eq
 -- Boolean Instructions
 | True
 | False
 | IfThenElse [SECDInstruction] [SECDInstruction]
 -- List Instructions
 | Cons
 | Nil
 | Case [SECDInstruction] [SECDInstruction]
 -- Let Bindings
 | LetBinding [SECDInstruction]
 | AppLet Int
 deriving (Show, Eq)
