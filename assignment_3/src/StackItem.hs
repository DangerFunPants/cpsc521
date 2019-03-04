module StackItem
    ( StackItem (..)
    , StackList (..)
    ) where

import Instruction as I

data StackList a
    = Cons a (StackList a)
    | Nil
    deriving (Show, Eq)

data StackItem
    = IVal Int
    | BVal Bool
    | StackList
    | Closure     [I.SECDInstruction] [StackItem]
    | FixClosure  [I.SECDInstruction] [StackItem]
    deriving (Show, Eq)
