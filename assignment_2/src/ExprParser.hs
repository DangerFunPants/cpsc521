module ExprParser where

import qualified AST as A
import AbsExprGrammar

traverseProg :: Prog -> A.Prog String String 
traverseProg (Prog_P0 funDef rest) = A.Prog (thisOne:recCall)
    where
        thisOne = traverseFun funDef
        (A.Prog recCall) = traverseProg rest

traverseProg (Prog_P1) = A.Prog []

traverseFun :: Fun -> A.Fun String String
traverseFun (Fun_P0 funName argList funExp) = A.Fun (name, args, expBody)
    where
        args = traverseArgList argList
        expBody = traverseExp funExp
        name = traverseId funName

traverseArgList :: ArgList -> [String]
traverseArgList (ArgList_P0 arg rest) = (traverseId arg):(traverseArgList rest)
traverseArgList (ArgList_P1) = []

traverseExp :: Exp -> A.Exp String String
traverseExp (Exp_P0 le re) = (A.Add lExp rExp)
    where
        lExp = traverseExp le
        rExp = traverseExp re
traverseExp (Exp_P1 le re) = (A.Sub lExp rExp)
    where   
        lExp = traverseExp le
        rExp = traverseExp re
traverseExp (Exp_P2 le re) = (A.Mul lExp rExp)
    where
        lExp = traverseExp le
        rExp = traverseExp re
traverseExp (Exp_P3 le re) = (A.Div lExp rExp)
    where
        lExp = traverseExp le
        rExp = traverseExp re
traverseExp (Exp_P4 e) = A.Neg exp
    where
        exp = traverseExp e
traverseExp (Exp_P5 intLit) = A.IVal i
    where
        i = traverseIVal intLit
traverseExp (Exp_P6 id) = A.Var (traverseId id)
traverseExp (Exp_P7 cond ifC elseC) = A.Cond bExp ifExp elseExp
    where
        bExp = traverseBExp cond
        ifExp = traverseExp ifC
        elseExp = traverseExp elseC
traverseExp (Exp_P8 fId a1 argList) = A.App name (arg1:expList)
    where
        expList = traverseParamList argList
        name = traverseId fId
        arg1 = traverseExp a1
traverseExp (Exp_P9 funList body) = A.Let funList' exp
    where
        funList' = traverseFunList funList
        exp = traverseExp body

traverseExpList :: [Exp] -> [A.Exp String String]
traverseExpList [] = []
traverseExpList (e:es) = (traverseExp e):(traverseExpList es)

traverseParamList :: ParamList -> [A.Exp String String]
traverseParamList (ParamList_P1) = []
traverseParamList (ParamList_P0 e rest) = exp:recCall
    where
        exp = traverseExp e
        recCall = traverseParamList rest

traverseFunList :: FunList -> [A.Fun String String]
traverseFunList (FunList_P1) = []
traverseFunList (FunList_P0 funDef rest) = thisOne:recCall
    where
        thisOne = traverseFun funDef
        recCall = traverseFunList rest

traverseId :: IDENT -> String
traverseId (IDENT id) = id

traverseIVal :: IVAL -> Int
traverseIVal (IVAL intLit) = (read intLit) :: Int

traverseBExp :: BExp -> A.BExp String String 
traverseBExp (BExp_P0 le re) = A.Lt lExp rExp
    where
        lExp = traverseExp le
        rExp = traverseExp re
traverseBExp (BExp_P1 le re) = A.Gt lExp rExp
    where
        lExp = traverseExp le 
        rExp = traverseExp re
traverseBExp (BExp_P2 le re) = A.Eq lExp rExp
    where
        lExp = traverseExp le
        rExp = traverseExp re
traverseBExp (BExp_P3 le re) = A.And lExp rExp
    where
        lExp = traverseBExp le
        rExp = traverseBExp re
traverseBExp (BExp_P4 le re) = A.Or lExp rExp
    where
        lExp = traverseBExp le
        rExp = traverseBExp re
traverseBExp (BExp_P5 e) = A.Not exp
    where
        exp = traverseBExp e







