module LambdaAst 
    ( Term (..)
    , LitT (..)
    ) where

data Term
    = Var String
    | Literal LitT
    | Abstraction Term Term
    | App Term Term
    | Add Term Term
    deriving (Show, Eq)

data LitT
    = BLit Bool
    | ILit Int
    deriving (Show, Eq)

test = Abstraction (Var "a") (Abstraction (Var "b") (Add (Var "a") (Var "b")))


