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
--  (5) A binary operation (+, -, /, *)
data Expr
  = Var String
  | Abstraction String Expr
  | Application Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | IntLiteral Int
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
  -- These need to be ordered from more general to less general
  expr <- choice $ fmap try [ app_parser_top_level
                            , bracketed_expr_parser_top_level
                            , binary_expression_parser_top_level
                            , abs_parser_top_level
                            , var_parser_top_level
                            , lit_parser_top_level
                            ]
  eof
  return expr

lambda_parser = do
  expr <- choice $ fmap try [ app_parser
                            , binary_expression_parser
                            , bracketed_expr_parser
                            , abs_parser
                            , var_parser
                            , lit_parser
                            ]
  return expr

bracketed_expr_parser_top_level = do
  expr <- bracketed_expr_parser
  eof
  return expr

lit_parser_top_level = do
  the_literal <- lit_parser
  eof
  return the_literal

lit_parser = do
  identifier <- int_lit_parser
  return identifier

binary_expression_parser_top_level = do
  binary_operation <- binary_expression_parser
  eof
  return binary_operation

binary_expression_parser = do
  spaces
  lh_expr <- lhs_parser
  spaces
  op_constructor <- binary_operator_parser
  spaces
  rh_expr <- rhs_parser
  spaces
  let this_expression = lh_expr `op_constructor` rh_expr
  continuation_parser this_expression
  where
    lhs_parser = do
      expr <- choice $ fmap try [ bracketed_expr_parser
                                , abs_parser
                                , app_parser
                                , var_parser
                                , lit_parser
                                ]
      return expr
    rhs_parser = lhs_parser
    continuation_parser the_expr = do
      spaces
      operator <- optionMaybe binary_operator_parser
      case operator of 
        Nothing -> return the_expr
        (Just op_constructor) -> do
          rhs_expr <- rhs_parser
          continuation_parser (op_constructor the_expr rhs_expr)

binary_operator_parser = do
  operator <- choice $ fmap try [ binary_minus_parser
                                , binary_plus_parser
                                , binary_star_parser
                                , binary_slash_parser
                                ]
  let op_constructor = case operator of
                        BinaryPlus -> Add
                        BinaryMinus -> Sub
                        BinaryStar -> Mul
                        BinarySlash -> Div
  return op_constructor

bracketed_expr_parser = do
  opening_paren
  the_expr <- lambda_parser
  closing_paren
  return the_expr

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

app_parser = do 
  lh_term <- app_term_parser
  spaces 
  rh_term <- app_term_parser
  let the_app_term = Application lh_term rh_term
  rest <- continuation_parser the_app_term
  return rest
  where
    app_term_parser = choice $ fmap try [ bracketed_expr_parser
                                        , abs_parser
                                        , var_parser
                                        , lit_parser
                                        ]
    continuation_parser current_term = do  
      spaces
      next_app_term <- optionMaybe app_term_parser
      case next_app_term of
        Nothing -> return current_term
        Just the_next_term -> continuation_parser (Application current_term the_next_term)

int_lit_parser = do
  spaces
  the_number <- int
  return $ IntLiteral the_number

binary_minus_parser = char '-' >> return BinaryMinus
binary_plus_parser = char '+' >> return BinaryPlus
binary_star_parser = char '*' >> return BinaryStar
binary_slash_parser = char '/' >> return BinarySlash
opening_paren = char '('
closing_paren = char ')'

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
            , "1 + (2 * 3)"
            , "(((1 + 2) * (3)))"
            , "(\\x. (((1 + 2) * (3)))) 5"
            , "(\\f. \\x. f x) (\\x. x + 1) 1"
            , "(\\f. \\x. f + x) 11" 
            , "(\\f. \\x. f + x) 1 2" 
            , "(\\x. f + x) 1 2" 
            ]
  forM_ src (\src_i -> do
    putStrLn "Original Source: "
    putStrLn $ src_i ++ "\n"

    putStrLn $ "Parsed Source: "
    print_one_parsed_lambda src_i
    putStrLn "\n\n")

