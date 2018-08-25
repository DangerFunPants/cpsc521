module AST where

import qualified SymbolTable as S
import Control.Monad.State
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Text.Prettyprint.Doc

data Prog a b 
    = Prog [Fun a b]
    deriving (Show)

data Fun a b 
    = Fun (a, [b], Exp a b)
    deriving (Show)

data BExp a b 
    = Lt (Exp a b) (Exp a b)
    | Gt (Exp a b) (Exp a b)
    | Eq (Exp a b) (Exp a b)
    | And (BExp a b) (BExp a b)
    | Or (BExp a b) (BExp a b)
    | Not (BExp a b) 
    deriving (Show)

data Exp a b
    = Add (Exp a b) (Exp a b)
    | Sub (Exp a b) (Exp a b)
    | Mul (Exp a b) (Exp a b)
    | Div (Exp a b) (Exp a b)
    | Neg (Exp a b)
    | IVal Int
    | Var b
    | Cond (BExp a b) (Exp a b) (Exp a b)
    | App a [(Exp a b)]
    | Let [Fun a b] (Exp a b)
    deriving (Show)

type STState = ExceptT String S.SymbolTableST 

insertFuncDefs :: [Fun String String] -> STState ()
insertFuncDefs [] = return ()
insertFuncDefs ((Fun (name, _, _)):fs) = do
    newName <- getFnName
    S.insertSym name newName
    insertFuncDefs fs

convProg :: Prog String String -> STState (Prog String String)
convProg (Prog []) = return $ Prog []
convProg (Prog funcs@(f:fs)) = do
    S.addLevel    
    insertFuncDefs funcs
    alpha <- convFn f 
    (Prog rest) <- convProg (Prog fs)
    S.remLevel
    return $ Prog (alpha:rest)

convFn :: Fun String String -> STState (Fun String String)
convFn f@(Fun (name, args, exp)) = do
    S.addLevel
    newName <- S.lookupSym name
    newArgs <- convArgs args
    newExp <- convExp exp
    S.remLevel
    return $ Fun (newName, newArgs, newExp)

convExp :: Exp String String -> STState (Exp String String)
convExp (Add le re) = convExp' Add le re
convExp (Sub le re) = convExp' Sub le re
convExp (Mul le re) = convExp' Mul le re
convExp (Div le re) = convExp' Div le re
convExp (Neg e) = do
    newE <- convExp e
    return $ Neg e
convExp iv@(IVal i) = return iv
convExp (Var b) = do
    newName <- S.lookupSym b
    return $ Var newName
convExp (Cond cond ifClause elseClause) = do
    newCond <- convBExp cond
    newIf <- convExp ifClause
    newElse <- convExp elseClause
    return $ Cond newCond newIf newElse
convExp (App fName expList) = do
    newName <- S.lookupSym fName
    newExpList <- convExpList expList
    return $ App newName newExpList
convExp (Let funList e) = do
    S.addLevel
    insertFuncDefs funList
    newFunList <- convFnList funList
    newE <- convExp e
    S.remLevel
    return $ Let newFunList newE

convExp' tc le re = do
    newLe <- convExp le
    newRe <- convExp re
    return $ tc newLe newRe

convBExp' tc le re = do
    newLe <- convBExp le
    newRe <- convBExp re
    return $ tc le re

convFnList :: [Fun String String] -> STState ([Fun String String])
convFnList [] = return []
convFnList (f:fs) = do
    newFn <- convFn f
    rest <- convFnList fs
    return $ newFn:rest

convBExp :: BExp String String -> STState (BExp String String)
convBExp (Lt le re) = convExp' Lt le re
convBExp (Gt le re) = convExp' Gt le re
convBExp (Eq le re) = convExp' Eq le re
convBExp (And le re) = convBExp' And le re
convBExp (Or le re) = convBExp' Or le re
convBExp (Not e) = do
    newE <- convBExp e
    return $ Not newE

convExpList :: [Exp String String] -> STState ([Exp String String])
convExpList [] = return []
convExpList (e:es) = do
    newE <- convExp e
    rest <- convExpList es
    return $ newE:rest
    
convArgs :: [String] -> STState [String]
convArgs [] = return []
convArgs (a:as) = do
    newName <- getVarName
    S.insertSym a newName
    rest <- convArgs as
    return (newName:rest)

getVarName :: STState String
getVarName = do
   label <- S.getVarLabel
   return $ "var" ++ show label

getFnName :: STState String
getFnName = do
    label <- S.getFnLabel
    return $ "fun" ++ show label

doConv :: Prog String String -> Either String (Prog String String)
doConv p = (fst . runIdentity) stateTup
    where
        stM = convProg p
        stateM = runExceptT stM
        stateTup = runStateT stateM (S.mkInitState)

main :: IO ()
main = do
    let ts = Prog [ (Fun ("main"
                        , ["a", "b"] 
                        , (Add (Var "a") (Var "b"))))
                  ]
        -- res = runState (convProg ts) (StateType S.empty 0 0)
    putStrLn $ "Hola Mundas"




