module ExprLifting where

import AST
import Data.Semigroup
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Except
import Control.Monad.Identity

cgProg :: Prog String String -> [(String, String)]
cgProg (Prog funList) = cgFunList funList

cgFunList :: [Fun String String] -> [(String, String)]
cgFunList (f:fs) = cgFun f <> cgFunList fs
cgFunList [] = []

cgFun :: Fun String String -> [(String, String)]
cgFun (Fun (name, args, exp)) = cgExp name exp

cgExp :: String -> Exp String String -> [(String, String)]
cgExp caller (Add le re) = cgExp caller le <> cgExp caller re
cgExp caller (Sub le re) = cgExp caller le <> cgExp caller re
cgExp caller (Mul le re) = cgExp caller le <> cgExp caller re
cgExp caller (Div le re) = cgExp caller le <> cgExp caller re
cgExp caller (Neg e) = cgExp caller e
cgExp caller (IVal _) = []
cgExp caller (Var _) = []
cgExp caller (Cond _ ifC elseC) = cgExp caller ifC <> cgExp caller elseC
cgExp caller (App fn argList) = [(caller, fn)]
cgExp caller (Let fnList exp) = (cgFunList fnList) <> (cgExp caller exp)

type MT = M.Map String (S.Set String, S.Set String)

initSetProg :: Prog String String -> MT
initSetProg (Prog funList) = initSetFunList (M.empty) funList 

initSetFunList :: MT -> [Fun String String] -> MT
initSetFunList d (f:fs) = newD
    where
        thisOne = initSetFun d f
        newD = initSetFunList thisOne fs
initSetFunList d [] = d

initSetFun d (Fun (name, args, exp)) = newD
    where
        withName = M.insert name (S.empty, S.empty) d
        wArgs = foldl (\d' v -> M.update (upFn v) name d') withName args
        newD = initSetExp wArgs name exp
        upFn v (as, fs) = Just (S.insert v as, fs)

initSetExp :: MT -> String -> Exp String String -> MT
initSetExp d name (Add le re) = initSetExp' d name le re
initSetExp d name (Sub le re) = initSetExp' d name le re
initSetExp d name (Mul le re) = initSetExp' d name le re
initSetExp d name (Div le re) = initSetExp' d name le re
initSetExp d name (Neg e) = initSetExp d name e
initSetExp d name (IVal _) = d
initSetExp d name (Var v) = M.update (\(as, fs) -> Just (as, S.insert v fs)) name d
initSetExp d name (Cond cond ifC elseC) = wElse
    where
        wCond = initSetBExp d name cond
        wIf = initSetExp wCond name ifC
        wElse = initSetExp wElse name elseC
initSetExp d name (App fn argList) = foldl (\d' v -> initSetExp d' name v) d argList
    where   
        upFn v (as, fs) = Just (as, S.insert v fs)

initSetExp d name (Let fnList exp) = newD
    where
        wList = initSetFunList d fnList
        newD = initSetExp wList name exp

initSetExp' :: MT -> String -> Exp String String -> Exp String String -> MT
initSetExp' d name e1 e2 = newD
    where
        w1 = initSetExp d name e1
        newD = initSetExp w1 name e2

initSetBExp :: MT -> String -> BExp String String -> MT
initSetBExp d name (Lt le re) = initSetExp' d name le re
initSetBExp d name (Gt le re) = initSetExp' d name le re 
initSetBExp d name (Eq le re) = initSetExp' d name le re
initSetBExp d name (And le re) = initSetBExp' d name le re
initSetBExp d name (Or le re) = initSetBExp' d name le re
initSetBExp d name (Not e) = initSetBExp d name e

initSetBExp' :: MT -> String -> BExp String String -> BExp String String -> MT
initSetBExp' d name e1 e2 = newD
    where
        w1 = initSetBExp d name e1
        newD = initSetBExp w1 name e2

liftFns :: MT -> [(String, String)] -> MT
liftFns d ((u, v):ls) = newD
    where
        thisOne = M.update (upFn v) u d
        upFn v (uAs, uFs) = Just $ (uAs, (uFs `S.union` (vFs `S.difference` vAs)))
            where
                Just (vAs, vFs) = M.lookup v d
        newD = liftFns thisOne ls
liftFns d [] = d

doFixedPoint :: MT -> [(String, String)] -> MT
doFixedPoint d ls = 
    if fstI == sndI 
        then sndI
        else doFixedPoint sndI ls
    where
        fstI = liftFns d ls
        sndI = liftFns fstI ls

type ErrM = ExceptT String Identity

tryLookup :: (Ord k) => k -> M.Map k v -> ErrM v
tryLookup k m = do
    case (M.lookup k m) of
        (Just v) -> return v
        Nothing -> throwE $ "Dictionary Lookup Failed"

liftProg :: MT -> Prog String String -> ErrM (Prog String String)
liftProg d (Prog funList) = do
    newList <- liftFunList d funList
    return $ Prog newList

liftFunList :: MT -> [Fun String String] -> ErrM [Fun String String]
liftFunList d (f:fs) = do
    thisOne <- liftFun d f
    rest <- liftFunList d fs
    return (thisOne++rest)
liftFunList d [] = return []

liftFun :: MT -> Fun String String -> ErrM [(Fun String String)]
liftFun d (Fun (name, argList, body)) = do
    (args, freeVars) <- tryLookup name d
    let liftedArgs = S.toList freeVars
    liftedBody <- liftExp d body
    case liftedBody of 
        (Let auxFuncs body) -> do
            return $ Fun (name, liftedArgs, body):auxFuncs
        otherwise -> do
            return $ (Fun (name, liftedArgs, liftedBody)):[]

liftExp :: MT -> Exp String String -> ErrM (Exp String String)
liftExp d (Add le re) = liftExp' d Add le re
liftExp d (Sub le re) = liftExp' d Sub le re
liftExp d (Mul le re) = liftExp' d Mul le re
liftExp d (Div le re) = liftExp' d Div le re
liftExp d (Neg e) = do
    newE <- liftExp d e
    return $ Neg newE
liftExp d iv@(IVal i) = return iv
liftExp d v@(Var b) = return v
liftExp d (Cond cond ifC elseC) = do
    condE <- liftBExp d cond
    ifE <- liftExp d ifC
    elseE <- liftExp d elseC
    return $ Cond condE ifE elseE

liftExp d (App fnName argList) = do
    (argVs, freeVs) <- tryLookup fnName d
    let toAdd = ((fmap Var) . S.toList) (freeVs `S.difference` argVs)
        newArgList = argList ++ toAdd
        
    return $ App fnName newArgList

liftExp d (Let funList body) = do
    auxFuncs <- liftFunList d funList
    liftedBody <- liftExp d body
    return $ Let auxFuncs liftedBody

liftExp' :: MT
         -> (Exp String String -> Exp String String -> a) 
         -> (Exp String String)
         -> (Exp String String)
         -> ErrM a
liftExp' d ctr e1 e2 = do
    e1' <- liftExp d e1
    e2' <- liftExp d e2
    return $ ctr e1' e2'

liftBExp :: MT -> BExp String String -> ErrM (BExp String String)
liftBExp d (Lt le re) = liftExp' d Lt le re
liftBExp d (Gt le re) = liftExp' d Lt le re
liftBExp d (Eq le re) = liftExp' d Eq le re
liftBExp d (And le re) = liftBExp' d And le re
liftBExp d (Or le re) = liftBExp' d Or le re
liftBExp d (Not e) = do
    newE <- liftBExp d e
    return $ Not newE

liftBExp' :: MT
          -> (BExp String String -> BExp String String -> a)
          -> (BExp String String)
          -> (BExp String String)
          -> ErrM a
liftBExp' d ctr e1 e2 = do
    e1' <- liftBExp d e1
    e2' <- liftBExp d e2
    return $ ctr e1 e2













































