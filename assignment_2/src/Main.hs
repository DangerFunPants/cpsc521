
import ExprParser
import ErrM
import LexExprGrammar
import AbsExprGrammar
import qualified AST as A
import ParExprGrammar
import Text.Show.Pretty
import ExprLifting
import ExprPrinter
import Data.Text.Prettyprint.Doc
import Control.Monad.Trans.Except
import Control.Monad.Identity

lexSource :: String -> [Token]
lexSource s = myLexer s

parseToks :: [Token] -> Err Prog
parseToks = pProg

runCompiler :: [Token] -> (Err (A.Prog String String))
runCompiler ts = do
    pt <- parseToks ts
    let ast = traverseProg pt
    return ast

main :: IO ()
main = do
    src <- getContents
    let (Ok ast) = runCompiler (lexSource src)
        (Right convd) = A.doConv ast
        callGraph = cgProg ast
        initSets = initSetProg ast
        lifted = doFixedPoint initSets callGraph
        (Right lifted') = (runIdentity . runExceptT) $ liftProg lifted ast
        prettyAst = pretty ast
        prettyLifted = pretty lifted'
    putStrLn $ ppShow ast
    putStrLn $ ppShow convd
    putStrLn $ ppShow callGraph
    putStrLn $ ppShow initSets
    putStrLn $ ppShow lifted
    putStrLn $ ppShow lifted'
    putStrLn $ show prettyAst
    putStrLn $ show prettyLifted
