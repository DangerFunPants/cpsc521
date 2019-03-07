module StackItem
    ( StackItem (..)
    ) where

import Instruction as I

data StackItem
    = IVal Int
    | BVal Bool
    | Closure     [I.SECDInstruction] [StackItem]
    | FixClosure  [I.SECDInstruction] [StackItem]
    | Cons StackItem StackItem
    | Nil
    deriving (Show, Eq)
