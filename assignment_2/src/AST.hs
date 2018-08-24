module AST where

{-# LANGUAGE TemplateHaskell #-}

import qualified SymbolTable as S
import Control.Monad.State
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

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

testProg = Prog [ (Fun ("main", [], Add (IVal 1) (IVal 1))) ]

type STState a = State StateType a

data StateType = StateType
    { _symbolTable :: S.ST
    , _varLabel :: Int
    , _funLabel :: Int
    }
    deriving (Show)

makeLenses ''StateType

pushLevel :: STState ()
pushLevel = state $ \s -> let new = (over symbolTable S.pushLevel) s
                          in ((), new)

popLevel :: STState ()
popLevel = state $ \s -> let new = (over symbolTable S.popLevel) s
                         in ((), new)

getFnLabel :: STState Int
getFnLabel = state $ \s -> let new = (over funLabel succ) s
                           in (s^.funLabel, new)

getFnName :: STState String
getFnName = do
    fnLabel <- getFnLabel
    return $ "fn" ++ (show fnLabel)

getVarLabel :: STState Int
getVarLabel = state $ \s -> let new = (over varLabel succ) s
                            in (s^.varLabel, new)

getVarName :: STState String
getVarName = do
    varLabel <- getVarLabel
    return $ "var" ++ (show varLabel)

insertSym :: String -> String -> STState ()
insertSym k v = state $ \s -> let new = (over symbolTable upFn) s
                                  upFn old = S.insert k v old
                                  in ((), new)

lookupSym :: String -> STState (Maybe String)
lookupSym k = state $ \s -> let v = S.find k (s^.symbolTable)
                            in (v, s)

convProg :: Prog String String -> STState (Prog String String)
convProg (Prog []) = return $ Prog []
convProg (Prog (f:fs)) = do
    pushLevel    
    alpha <- convFn f 
    (Prog rest) <- convProg (Prog fs)
    popLevel
    return $ Prog (alpha:rest)

convFn :: Fun String String -> STState (Fun String String)
convFn f@(Fun (name, args, exp)) = do
    newName <- getFnName
    insertSym name newName
    pushLevel
    newArgs <- convArgs args
    newExp <- convExp exp
    popLevel
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
    newName <- lookupSym b
    let (Just nn) = newName
    return $ Var nn
convExp (Cond cond ifClause elseClause) = do
    newCond <- convBExp cond
    newIf <- convExp ifClause
    newElse <- convExp elseClause
    return $ Cond newCond newIf newElse
convExp (App fName expList) = do
    newName <- lookupSym fName
    let (Just nn) = newName
    newExpList <- convExpList expList
    return $ App nn newExpList
convExp (Let funList e) = do
    pushLevel
    newFunList <- convFnList funList
    newE <- convExp e
    popLevel
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
    insertSym a newName
    rest <- convArgs as
    return (newName:rest)

doConv :: Prog String String -> Prog String String
doConv p = fst $ runState (convProg p) (StateType S.mkEmpty 0 0)

main :: IO ()
main = do
    let ts = Prog [ (Fun ("main"
                        , ["a", "b"] 
                        , (Add (Var "a") (Var "b"))))
                  ]
        res = runState (convProg ts) (StateType S.mkEmpty 0 0)
    putStrLn $ show res




