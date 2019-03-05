{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module SymbolTable 
    ( SymbolTableST
    , addLevel
    , remLevel
    , getFnLabel
    , getVarLabel
    , insertSym
    , lookupSym
    , empty
    , mkInitState
    , lookupDistance
    , MapList (..)
    , SymbolTable (..)
    ) where

import qualified Data.Map as M
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

class SymbolTable (t :: * -> * -> *) where
    find        :: (Ord k) => k -> t k v -> Maybe v
    insert      :: (Ord k) => k -> v -> t k v -> t k v
    pushLevel   :: (Ord k) => t k v -> t k v
    popLevel    :: (Ord k) => t k v -> t k v
    mkEmpty     :: (Ord k) => t k v
    distance    :: (Ord k) => k -> t k v -> Maybe Int

instance SymbolTable (MapList) where
    mkEmpty = (MapList [])

    insert k v (MapList (x:xs)) = MapList $ (M.insert k v x):xs

    pushLevel (MapList xs) = MapList $  (M.empty:xs)
    
    popLevel (MapList (x:xs)) = MapList xs
    popLevel (MapList []) = MapList []

    find k (MapList (x:xs)) = 
        case M.lookup k x of
            Nothing -> find k (MapList xs)
            (Just v) -> (Just v)
    find _ (MapList []) = Nothing

    distance k (MapList (x:xs)) =
        case M.lookup k x of
            Nothing -> do
                rec <- distance k (MapList xs)
                return $ 1 + rec
            Just _ -> return 0
    distance k (MapList []) = Nothing

newtype MapList k v = MapList [M.Map k v]
  deriving (Show, Eq)

type ST k v = MapList k v

data SymbolTableT k v = SymbolTableT 
    { _symbolTable:: ST k v
    , _varLabel    :: Int
    , _funLabel    :: Int
    }

makeLenses ''SymbolTableT

type SymbolTableST k v = StateT (SymbolTableT k v) Identity

addLevel' :: (Ord k) => (SymbolTableST k v) ()
addLevel' = StateT $ \s -> 
    let new = (over symbolTable pushLevel) s
    in return ((), new)

addLevel :: (Ord k) => ExceptT String (SymbolTableST k v) ()
addLevel = lift addLevel'

remLevel' :: (Ord k) => (SymbolTableST k v) ()
remLevel' = StateT $ \s ->
    let new = (over symbolTable popLevel) s
    in return ((), new)

remLevel :: (Ord k) => ExceptT String (SymbolTableST k v) ()
remLevel = lift remLevel'

getFnLabel' :: (Ord k) => (SymbolTableST k v) Int
getFnLabel' = StateT $ \s ->
    let l = s^.funLabel
        new = (over funLabel succ) s
    in return (l, new)

getFnLabel :: (Ord k) => ExceptT String (SymbolTableST k v) Int
getFnLabel = lift getFnLabel'

getVarLabel' :: (Ord k) => (SymbolTableST k v) Int
getVarLabel' = StateT $ \s ->
    let l = s^.varLabel
        new = (over varLabel succ) s
    in return (l, new)

getVarLabel :: (Ord k) => ExceptT String (SymbolTableST k v) Int
getVarLabel = lift getVarLabel'

insertSym' :: (Ord k) => k -> v -> (SymbolTableST k v) ()
insertSym' k v = StateT $ \s -> 
    let new = (over symbolTable (insert k v)) s
    in return ((), new)

insertSym :: (Ord k) => k -> v -> ExceptT String (SymbolTableST k v) ()
insertSym k v = lift $ insertSym' k v

lookupSym' :: (Ord k) => k -> (SymbolTableST k v) (Maybe v)
lookupSym' k = StateT $ \s ->
    let v = find k (s^.symbolTable)
    in return (v, s)

lookupSym :: (Show k, Ord k) => k -> ExceptT String (SymbolTableST k v) v
lookupSym k = do
    s <- lift $ lookupSym' k
    case s of
        (Just v) -> return v
        Nothing -> throwE $ "Failed to find symbol: " ++ (show k)

lookupDistance' :: (Ord k) => k -> (SymbolTableST k v) (Maybe Int)
lookupDistance' k = StateT $ \s ->
    let d = distance k (s^.symbolTable)
    in return (d, s)

lookupDistance :: (Show k, Ord k) => k -> ExceptT String (SymbolTableST k v) Int
lookupDistance k = do
    s <- lift $ lookupDistance' k
    case s of
        Just v -> return v
        Nothing -> throwE $ "Symbol: " ++ (show k) ++ " not present in table"

empty :: (Ord k) => ST k v
empty = mkEmpty

mkInitState :: (Show k, Ord k) => SymbolTableT k v
mkInitState = SymbolTableT mkEmpty 0 0








































    
    

