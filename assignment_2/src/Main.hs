
import ExprParser
import ErrM
import LexExprGrammar
import AbsExprGrammar
import qualified AST as A
import ParExprGrammar
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
        callGraph = cgProg convd
        initSet = initSetProg convd
        lifted = doFixedPoint initSet callGraph
        lifted' = 
            case (runIdentity . runExceptT) $ liftProg lifted convd of
                (Left err) -> pretty err
                (Right l) -> pretty l
        prettyAst = pretty ast
    putStrLn $ "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn $ "                      Original Source                         "
    putStrLn $ ""
    putStrLn $ show prettyAst
    putStrLn $ "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn $ "                        Lifted Source                         "
    putStrLn $ ""
    putStrLn $ show lifted'
    putStrLn $ "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
