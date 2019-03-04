module LambdaAst 
    ( Term (..)
    , LitT (..)
    , parseLambda
    ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Number
import Text.Show.Pretty

-- Grammar:
-- (\x. x + 2) 2
--
--
-- Term_p0 . Term ::= id ExprRest ;
-- Term_p1 . Term ::= ival ExprRest ;
-- Term_p2 . Term ::= true | false ;
-- Term_p3 . Term ::= \id. Term ; 
-- Term_p4 . Term ::= (Term Term) ;
-- Term_p5 . Term ::= (Term) ExprRest ;
-- Term_p5 . Term ::= ;
--
-- ExprRest_p0 . Expr ::= + Term ;
-- ExprRest_p1 . Expr ::= - Term ;
-- ExprRest_p2 . Expr ::= ;
-- 
-- Term_p0 . Term ::= IntExpr IntExprRest ;
-- Term_p1 . Term ::= BExpr ;
-- Term_p2 . Term ::= if BExpr then Term else Term ;
-- Term_p3 . Term ::= (Term Term)
-- Term_p4 . Term ::= \id. Term ;
-- Term_p5 . Term ::= IntExpr BExprRest ;
--  
-- BExpr_p0 . BExpr ::= true ;
-- BExpr_p1 . BExpr ::= false ;
-- BExpr_p2 . BExpr ::= id ;
--
-- IntExpr_p0 . IntExpr ::= int_lit ;
-- IntExpr_p1 . IntExpr ::= id ;
--
-- IntExprRest_p0 ::= + Term ;
-- IntExprRest_p1 ::= - Term ;
-- IntExprRest_p2 ::= ;
--
-- BExprRest_p0 . BExpr ::= < IntExpr ;
-- BExprRest_p1 . BExpr ::= ;
--
--
--
--
-- Term_P         . Term          ::= Abstraction Term
-- Term_P         . Term          ::= Abstraction 
-- Term_P         . Term          ::= Int ERest
--
-- ERest_P        . ERest         ::= + Int ERest
-- ERest_P        . ERest         ::= - Int ERest
-- ERest_P        . ERest         ::= * Int ERest
-- ERest_P        . ERest         ::= / Int ERest
--
--
-- Abstraction_P  . Abstraction   ::= \arg . Term
-- 
--
data Term
    = Var String
    | Literal LitT
    | Abstraction String Term
    | App Term Term
    | Add Term Term
    | Mul Term Term
    | LessThan Term Term
    | Equal Term Term 
    | Cond Term Term Term
    | Fix Term 
    deriving (Show, Eq)

data LitT
    = BLit Bool
    | ILit Int
    deriving (Show, Eq)

one_or_more_spaces = do
  space
  spaces
  return $ ()

parseTerm = foldl (\a p -> a <|> p) termP2 (fmap try [ termP6
                                                     , termP1
                                                     , termP0
                                                     , termP3
                                                     , termP4
                                                     , termP5
                                                     ])

termP0 = do
    e <- parseIntExpr
    rest <- parseIntExprRest
    case rest of
        Nothing -> return e
        Just v -> return $ v e

termP1 = parseBExpr

termP2 = do
    string "if"
    spaces
    cond <- parseTerm
    spaces
    string "then"
    spaces
    t1 <- parseTerm
    spaces
    string "else"
    spaces
    t2 <- parseTerm
    return $ Cond cond t1 t2

termP3 = do
    char '('
    t1 <- parseTerm
    spaces
    t2 <- parseTerm
    spaces
    char ')'
    return $ App t1 t2

termP4 = do
    char '\\'
    id <- (many1 letter)
    char '.'
    spaces
    t1 <- parseTerm
    return $ Abstraction id t1

termP5 = do
    char '('
    spaces
    t <- parseTerm
    spaces
    char ')'
    rest <- parseIntExprRest
    case rest of 
        Nothing -> return t
        Just v -> return $ v t

termP6 = do
  char '('
  spaces
  string "fix"
  one_or_more_spaces
  the_abstraction <- parseTerm
  spaces
  char ')'
  return $ Fix the_abstraction

parseBExpr = choice [ bExprP0, bExprP1 ]
bExprP0 = string "true" >> (return $ Literal (BLit True))
bExprP1 = string "false" >> (return $ Literal (BLit False))

parseIntExpr = spaces >> choice [ intExprP0, intExprP1 ]
intExprP0 = int >>= (\v -> return $ Literal (ILit v))
intExprP1 = (many1 letter) >>= (\v -> return $ Var v)

parseIntExprRest = spaces >> choice [ intExprRestP0
                                    , intExprRestP1
                                    , intExprRestP2
                                    , intExprRestP3 
                                    , intExprRestP4
                                    ]
intExprRestP0 = char '+' >> spaces >> parseTerm >>= (\v -> return $ Just (\t -> (Add v t)))
intExprRestP1 = char '*' >> spaces >> parseTerm >>= (\v -> return $ Just (\t -> (Mul v t)))
intExprRestP2 = char '<' >> spaces >> parseTerm >>= (\v -> return $ Just (\t -> (LessThan v t)))
intExprRestP3 = char '=' >> spaces >> parseTerm >>= (\v -> return $ Just (\t -> (Equal v t)))
intExprRestP4 = return Nothing

parseLambda :: String -> Either ParseError Term
parseLambda = runParser parseTerm () "Failed to parse lambda"

main :: IO ()
main = do
    let ts = "(\\x. (x 2) \\y. y + 4)"
        ts1 = "((\\x. if x then \\y. (y + 2) else \\y. y + 4 true) 2)"
        ts2 = "((\\x. if ((x + 5) * 10) < 2 then \\y. y + 2 else \\y. y * 4 5) 2)"
        res = parseLambda ts2
    putStrLn $ ppShow res
     

