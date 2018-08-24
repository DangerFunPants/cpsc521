



import ExprParser
import ErrM
import LexExprGrammar
import AbsExprGrammar
import qualified AST as A
import ParExprGrammar

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
        convd = A.doConv ast
    putStrLn $ show ast
    putStrLn $ show convd
