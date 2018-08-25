module ExprPrinter where

import AST
import Data.Text.Prettyprint.Doc

instance (Pretty a, Pretty b) => Pretty (Prog a b) where
    pretty (Prog (f:fs)) = (pretty f) <> line <> (pretty (Prog fs))
    pretty (Prog []) = line

instance (Pretty a, Pretty b) => Pretty (Fun a b) where
    pretty (Fun (fnName, argList, body)) = pretty "fun" 
                                            <+> pretty fnName
                                            <+> prettyArgList
                                            <+> pretty "="
                                            <+> pretty body
        where
            prettyArgList = sep (fmap pretty argList)

instance (Pretty a, Pretty b) => Pretty (Exp a b) where
    pretty (Add le re) = prettyExp' (pretty "+") le re
    pretty (Sub le re) = prettyExp' (pretty "-") le re
    pretty (Mul le re) = prettyExp' (pretty "*") le re
    pretty (Div le re) = prettyExp' (pretty "/") le re
    pretty (Neg e) = (pretty "-") <> (pretty e)
    pretty (IVal i) = pretty i
    pretty (Var v) = pretty v
    pretty (Cond cond ifC elseC) = (align . sep) ps
        where
            ps = [ (pretty "if")
                 , (pretty cond)
                 , (pretty "then")
                 , (pretty ifC)
                 , (pretty "else")
                 , (pretty elseC)
                 ]
    pretty (App fnName argList) = (pretty fnName) <+> ((align . sep) (fmap pretty argList))
    pretty (Let fnList body) = (align . vsep) [ (pretty "let")
                                              , (indent 4 letFuns)
                                              , (pretty "in")
                                              , (indent 4 letBody) 
                                              ] 
        where
            -- ps = [ (pretty "let")
            --      , indent
            --      , (sep (fmap pretty fnList))
            --      , (pretty "in")
            --      , (pretty body)
            --      ]
            letFuns = sep (fmap pretty fnList)
            letBody = pretty body
instance (Pretty a, Pretty b) => Pretty (BExp a b) where
    pretty (Lt le re) = prettyExp' (pretty "<") le re
    pretty (Gt le re) = prettyExp' (pretty ">") le re
    pretty (Eq le re) = prettyExp' (pretty "==") le re
    pretty (And le re) = prettyBExp' (pretty "&&") le re
    pretty (Or le re) = prettyBExp' (pretty "||") le re
    pretty (Not e) = (pretty "not") <+> (pretty e)

prettyExp' opD le re = (pretty le) <+> opD <+> (pretty re)
prettyBExp' opD le re = (pretty le) <+> opD <+> (pretty re)
