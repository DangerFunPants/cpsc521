{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module SymbolTable ( SymbolTableST
                   , addLevel
                   , remLevel
                   , getFnLabel
                   , getVarLabel
                   , insertSym
                   , lookupSym
                   , empty
                   , mkInitState
                   ) where

import qualified Data.Map as M
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

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

data SymbolTableT = SymbolTableT
    { _symbolTable :: ST
    , _varLabel    :: Int
    , _funLabel    :: Int
    }

makeLenses ''SymbolTableT

type SymbolTableST = StateT SymbolTableT Identity

addLevel' :: SymbolTableST ()
addLevel' = StateT $ \s -> let new  = (over symbolTable pushLevel) s
                          in return ((), new)

addLevel :: ExceptT String SymbolTableST ()
addLevel = lift addLevel'

remLevel' :: SymbolTableST ()
remLevel' = StateT $ \s -> let new = (over symbolTable popLevel) s
                          in return ((), new)

remLevel :: ExceptT String SymbolTableST ()
remLevel = lift addLevel'

getFnLabel' :: SymbolTableST Int
getFnLabel' = StateT $ \s -> let new = (over funLabel succ) s
                                 l = (view funLabel) s
                             in return (l, new)

getFnLabel :: ExceptT String SymbolTableST Int
getFnLabel = lift getFnLabel'

getVarLabel' :: SymbolTableST Int
getVarLabel' = StateT $ \s -> let new = (over varLabel succ) s
                                  l = (view varLabel) s
                              in return (l, new)

getVarLabel :: ExceptT String SymbolTableST Int
getVarLabel = lift getVarLabel'

insertSym' :: String -> String -> SymbolTableST ()
insertSym' k v = StateT $ \s -> let new = (over symbolTable upFn) s
                                    upFn old = insert k v old
                                in return ((), new)

insertSym :: String -> String -> ExceptT String SymbolTableST ()
insertSym k v = lift $ insertSym' k v

lookupSym' :: String -> SymbolTableST (Maybe String)
lookupSym' k = StateT $ \s -> let v = find k (s^.symbolTable)
                              in return (v, s)

lookupSym :: String -> ExceptT String SymbolTableST String
lookupSym k = do
    s <- lift $ lookupSym' k
    case s of 
        (Just v) -> return v
        Nothing -> throwE $ "Failed to find symbol " ++ k

empty :: ST
empty = mkEmpty 

mkInitState :: SymbolTableT
mkInitState = SymbolTableT mkEmpty 0 0











