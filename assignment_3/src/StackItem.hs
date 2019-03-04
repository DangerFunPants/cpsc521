module StackItem
    ( StackItem (..)
    ) where

import Instruction as I

-- data StackList a
--     = Cons a (StackList a)
--     | Nil
--     deriving (Show, Eq)

data StackItem
    = IVal Int
    | BVal Bool
    | Closure     [I.SECDInstruction] [StackItem]
    | FixClosure  [I.SECDInstruction] [StackItem]
    | Cons StackItem StackItem
    | Nil
    deriving (Show, Eq)
