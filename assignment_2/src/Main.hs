
import ExprParser
import ErrM
import LexExprGrammar
import AbsExprGrammar
import qualified AST as A
import ParExprGrammar
import ExprLifting
import ExprPrinter
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Text.Show.Pretty

lexSource :: String -> [Token]
lexSource s = myLexer s

parseToks :: [Token] -> Err Prog
parseToks = pProg

runCompiler :: String -> Either String (A.Prog String String)
runCompiler src = do
    let pt = parseToks ts
        ts = lexSource src
    case pt of
        Bad e -> Left e
        Ok toks -> do
            let ast = traverseProg toks
            return ast

performLift src = do
    ast <- runCompiler src
    convd <- A.doConv ast
    let callGraph = cgProg convd
        initSet = initSetProg convd
    lifted <- (runIdentity . runExceptT) $ doFixedPoint initSet callGraph
    convAst <- (runIdentity . runExceptT) $ liftProg lifted convd
    return convAst

main :: IO ()
main = do
    src <- getContents
    let lifted' = case performLift src of
                    Left e -> pretty e
                    Right v -> pretty v
        prettyAst = case runCompiler src of
                        Left e -> pretty e
                        Right v -> pretty v
        smartLayout = (renderString . (layoutSmart defaultLayoutOptions)) lifted'
    putStrLn $ "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn $ "                      Original Source                         "
    putStrLn $ ""
    putStrLn $ show prettyAst
    putStrLn $ "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn $ "                        Lifted Source                         "
    putStrLn $ ""
    putStrLn $ smartLayout
    putStrLn $ "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
