{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module SymbolTable where

import qualified Data.Map as M

class SymbolTable (t :: *) where
    find :: String -> t -> Maybe String
    insert :: String -> String -> t -> t
    pushLevel :: t -> t 
    popLevel :: t -> t 
    mkEmpty :: t 

instance SymbolTable [M.Map String String] where
    mkEmpty = []
    insert k v (tl:rest) = (M.insert k v tl):rest
    pushLevel st = M.empty:st
    popLevel (st:rest) = rest
    find k (st:rest) = 
        case thisLevel of
            Nothing -> find k rest
            (Just v) -> (Just v)
        where
            thisLevel = M.lookup k st
    find _ [] = Nothing

type ST = [M.Map String String]
        
