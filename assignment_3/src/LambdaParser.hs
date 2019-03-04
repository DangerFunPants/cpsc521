module LambdaParser where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Number
import Text.Parsec
import Text.Show.Pretty
import Control.Monad

-- There are no statements so to speak.
-- Three things that an expression can be
--  (1) A variable (x,y,z etc...)
--  (2) Abstraction (Func defn)
--  (3) Application (Func application)
--  (4) An integer literal
data Expr
  = Var String
  | Abstraction String Expr
  | Application Expr Expr
  | IntExpr IntExpr
  deriving (Show, Eq)

data IntExpr
  = IntLiteral Int
  | IntVar Expr 
  | IntAdd IntExpr IntExpr
  | IntSub IntExpr IntExpr
  | IntMul IntExpr IntExpr
  | IntDiv IntExpr IntExpr
  deriving (Show, Eq)

data BinaryOperator
  = BinaryMinus
  | BinaryPlus
  | BinaryStar
  | BinarySlash
  deriving (Show, Eq)

parse_lambda :: String -> Either String Expr
parse_lambda src = 
  case parse top_level_lambda_parser "Failed to parse lambda." src of
    Left parser_error -> Left $ show parser_error
    Right ast -> Right ast

top_level_lambda_parser = do
  expr <- choice $ fmap try [ bracketed_expr_parser_top_level
                            , abs_parser_top_level
                            , app_parser_top_level
                            , var_parser_top_level
                            , int_expr_parser_top_level
                            ]
  eof
  return expr

lambda_parser = do
  expr <- choice $ fmap try [ bracketed_expr_parser
                            , abs_parser
                            , app_parser
                            , var_parser
                            , int_expr_parser
                            ]
  return expr

bracketed_expr_parser_top_level = do
  expr <- bracketed_expr_parser
  eof
  return expr

bracketed_expr_parser = do
  opening_paren
  the_expr <- lambda_parser
  closing_paren
  rest <- continuation_parser -- could be more args or the rest of an int expr
  case rest of
    None -> return the_expr
    Just the_rest -> return $ the_rest the_expr

continuation_parser = do
  case optionalMaybe build_app_term of
    Nothing -> do
      -- IntExpr
    Just app_term ->
      

int_expr_parser_top_level = do
  int_expr <- try int_expr_parser
  eof
  return int_expr

bracketed_int_expr_parser_top_level = do
  bracketed_expr <- bracketed_int_expr_parser
  eof
  return bracketed_expr

-- bracketed_or_non_bracketed_int_expr_parser_top_level = do 
--   int_expr <- choice $ fmap try [ int_expr_parser_top_level
--                                 , bracketed_int_expr_parser_top_level
--                                 ]
--   return int_expr

abs_parser_top_level = do
  abstraction <- abs_parser 
  eof
  return abstraction

app_parser_top_level = do
  application <- app_parser
  eof
  return application

var_parser_top_level = do
  variable <- var_parser
  eof
  return variable

ident_parser = do 
  ident <- many1 letter
  return ident

var_parser = do
  ident <- ident_parser
  return $ Var ident

abs_parser = do
  char '\\'
  ident <- ident_parser
  char '.'
  spaces
  abs_expr <- lambda_parser
  return $ Abstraction ident abs_expr

continuation_term_parser = do 
  continuation_term <- optionMaybe $ choice $ fmap try [ abs_parser
                                                       , var_parser
                                                       , bracketed_int_expr_parser
                                                       , int_expr_parser
                                                       ]
  return continuation_term

app_parser = do 
  lh_term <- app_term_parser
  spaces 
  rh_term <- app_term_parser
  the_app_term <- build_app_term $ Application lh_term rh_term 
  return the_app_term
  where
    app_term_parser = choice $ fmap try [ bracketed_expr_parser
                                        , abs_parser
                                        , var_parser
                                        , bracketed_int_expr_parser
                                        , int_expr_parser
                                        ]

-- a (b c)  -> (a (b c))
-- a b c    -> ((a b) c)
-- (a b) c  -> ((a b) c)
build_app_term curr_term = do
  rest <- app_parser_rest
  case rest of
    Nothing -> return curr_term
    Just rest_expr -> do
      let new_curr_term = Application curr_term rest_expr
      the_term <- build_app_term new_curr_term
      return the_term

app_parser_rest = do
  spaces
  rest_term <- continuation_term_parser
  return rest_term

-- 1 + 1 + 1 + 1 -> (((1 + 1) + 1) + 1)
int_lit_parser = do
  spaces
  the_number <- int
  return $ IntLiteral the_number

int_var_parser = do
  spaces
  the_variable <- var_parser
  return $ IntVar the_variable

bracketed_int_expr_parser = do  
  opening_paren
  spaces
  bracketed_term <- int_expr_parser
  spaces
  closing_paren
  let unwrapped_bracketed_term = case bracketed_term of
                                  IntExpr the_expr -> the_expr
                                  otherwise -> error "Should have had in IntExpr."
  int_expr <- build_int_expr unwrapped_bracketed_term
  return $ IntExpr int_expr

int_expr_parser = do
  lh_term <- choice $ fmap try [int_var_parser, int_lit_parser]
  int_expr <- build_int_expr lh_term
  return $ IntExpr int_expr

int_binop_parser = do
  spaces
  the_operator <- optionMaybe $ choice [ binary_plus_parser
                                       , binary_minus_parser
                                       , binary_star_parser
                                       , binary_slash_parser
                                       ]
  return the_operator

binary_minus_parser = char '-' >> return BinaryMinus
binary_plus_parser = char '+' >> return BinaryPlus
binary_star_parser = char '*' >> return BinaryStar
binary_slash_parser = char '/' >> return BinarySlash
opening_paren = char '('
closing_paren = char ')'

int_expr_with_brackets_parser = do
  spaces
  the_expr <- bracketed_int_expr_parser
  case the_expr of
    IntExpr some_expr -> return some_expr
    otherwise -> error "Should have had an IntExpr"

build_int_expr current_expr = do
  spaces
  the_operator <- int_binop_parser
  case the_operator of 
    Nothing -> return current_expr
    Just op_val -> do
      rhs_lit <- choice $ fmap try [int_expr_with_brackets_parser, int_var_parser, int_lit_parser]
      -- rhs_lit <- int_expr_with_brackets_parser
      let constructor = case op_val of
                          BinaryPlus  -> IntAdd
                          BinaryMinus -> IntSub
                          BinaryStar  -> IntMul
                          BinarySlash -> IntDiv
      build_int_expr $ constructor current_expr rhs_lit

print_one_parsed_lambda :: String -> IO ()
print_one_parsed_lambda src = do
  case parse_lambda src of
    Left err -> putStrLn err
    Right ast -> putStrLn $ ppShow ast

main :: IO ()
main = do
  let src = [ "\\x. (1 + 2 * 3 / 4) 5"
            , "\\x. 1 + 2 * 3 / 4 5"
            , "\\x. 4 * (2 + 3)"
            , "\\x. 4 * 2 + 3"
            , "\\x. (2 + 3) * 4"
            , "\\x. (2 + 3) * (4 + 5)"
            , "(\\x. x) (\\x. x)"
            ]
  forM_ src (\src_i -> do
    putStrLn "Original Source: "
    putStrLn $ src_i ++ "\n"

    putStrLn $ "Parsed Source: "
    print_one_parsed_lambda src_i
    putStrLn "\n\n")

