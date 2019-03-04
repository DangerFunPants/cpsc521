module NewParser where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Number
import Text.Parsec
import Text.Show.Pretty
import Control.Monad
import Data.Functor.Identity (Identity)

data Lambda_Expr
  = Var String
  | Abstraction String Lambda_Expr
  | Application Lambda_Expr Lambda_Expr
  | Add Lambda_Expr Lambda_Expr
  | Sub Lambda_Expr Lambda_Expr
  | Mul Lambda_Expr Lambda_Expr
  | Div Lambda_Expr Lambda_Expr
  | Conditional Lambda_Expr Lambda_Expr Lambda_Expr
  | IntLiteral Int
  | BoolLiteral Bool
  deriving (Show, Eq)

data BinaryOperator
  = BinaryMinus
  | BinaryPlus
  | BinaryStar
  | BinarySlash
  deriving (Show, Eq)

type Parser a = ParsecT String () Identity a

-- ****************************************************************************
--                                Top Level Parser
-- ****************************************************************************

-- ParsecT s u m a
top_level_lambda_parser :: Parser Lambda_Expr
top_level_lambda_parser = do
  expr <- lambda_parser
  eof
  return expr

-- ****************************************************************************
--                                Expression Parser
-- ****************************************************************************
lambda_parser :: Parser Lambda_Expr
lambda_parser = do  
  expr <- choice $ fmap try [ app_parser
                            , binary_expression_parser
                            , bracketed_expression_parser
                            , abs_parser
                            , literal_parser
                            , var_parser
                            ]
  return expr

-- ****************************************************************************
--                              Sub Expression Parsers
-- ****************************************************************************
app_parser :: Parser Lambda_Expr
app_parser = do 
  lh_term <- app_term_parser
  spaces
  rh_term <- app_term_parser
  let the_app_term = Application lh_term rh_term
  continuation_parser the_app_term
  where
    app_term_parser = choice $ fmap try [ bracketed_expression_parser
                                        , abs_parser
                                        , literal_parser
                                        , var_parser
                                        ]
    continuation_parser current_term = do 
      spaces
      next_app_term <- optionMaybe app_term_parser
      case next_app_term of
        Nothing -> return current_term
        Just the_next_term -> continuation_parser (Application current_term the_next_term)

binary_expression_parser :: Parser Lambda_Expr
binary_expression_parser = do
  spaces
  lh_expr <- binary_expression_term_parser
  spaces
  op_constructor <- binary_operator_parser
  spaces
  rh_expr <- binary_expression_term_parser
  spaces
  let this_expression = op_constructor lh_expr rh_expr
  continuation_parser this_expression
  where
    binary_expression_term_parser = do  
      expr <- choice $ fmap try [ bracketed_expression_parser
                                , abs_parser
                                , app_parser
                                , literal_parser
                                , var_parser
                                ]
      return expr
    continuation_parser the_expr = do
      spaces
      operator <- optionMaybe binary_operator_parser
      spaces
      case operator of
        Nothing -> return the_expr
        (Just op_constructor) -> do
          rhs_expr <- binary_expression_term_parser
          continuation_parser (op_constructor the_expr rhs_expr)
    binary_operator_parser = do
      operator_token <- operator_token_parser 
      let binary_constructor = case operator_token of
                                  BinaryPlus -> Add
                                  BinaryMinus -> Sub
                                  BinaryStar -> Mul
                                  BinarySlash -> Div
      return binary_constructor
    operator_token_parser = do
      operator_token <- choice $ fmap try [ binary_minus_parser
                                          , binary_plus_parser
                                          , binary_star_operator
                                          , binary_slash_operator
                                          ]
      return operator_token
  
bracketed_expression_parser :: Parser Lambda_Expr
bracketed_expression_parser = do
  opening_paren
  the_expr <- lambda_parser
  closing_paren
  return the_expr

abs_parser :: Parser Lambda_Expr
abs_parser = do 
  char '\\'
  ident <- ident_parser
  char '.'
  spaces
  abs_expr <- lambda_parser
  return $ Abstraction ident abs_expr

var_parser :: Parser Lambda_Expr
var_parser = ident_parser >>= \v -> return $ Var v

literal_parser :: Parser Lambda_Expr
literal_parser = do
  the_literal <- choice $ fmap try [ int_literal_parser
                                   , bool_literal_parser
                                   ]
  return the_literal

-- ****************************************************************************
--                          Helpers and Leaves                        
-- ****************************************************************************
ident_parser :: Parser String
ident_parser = many1 letter

int_literal_parser :: Parser Lambda_Expr
int_literal_parser = int >>= \v -> return $ IntLiteral v

bool_literal_parser :: Parser Lambda_Expr
bool_literal_parser = do
  bool_literal <- choice $ fmap string ["true", "false"]
  case bool_literal of
    "true" -> return $ BoolLiteral True
    "false" -> return $ BoolLiteral False

binary_minus_parser :: Parser BinaryOperator
binary_minus_parser = char '-' >> return BinaryMinus

binary_plus_parser :: Parser BinaryOperator
binary_plus_parser = char '+' >> return BinaryPlus

binary_star_operator :: Parser BinaryOperator
binary_star_operator = char '*' >> return BinaryStar

binary_slash_operator :: Parser BinaryOperator
binary_slash_operator = char '/' >> return BinarySlash

opening_paren :: Parser ()
opening_paren = char '(' >> return ()

closing_paren :: Parser ()
closing_paren = char ')' >> return ()

-- ****************************************************************************
--                          Exported Functions
-- ****************************************************************************
parse_lambda :: String -> Either String Lambda_Expr
parse_lambda src =
  case parse top_level_lambda_parser "Failed to parse lambda." src of
    Left err -> Left $ show err
    Right lambda_ast -> Right lambda_ast

-- ****************************************************************************
--                          Adhoc Testing
-- ****************************************************************************

print_one_parsed_lambda :: String -> IO ()
print_one_parsed_lambda src = do
  case parse_lambda src of
    Left err -> putStrLn err
    Right ast -> putStrLn $ ppShow ast

main :: IO ()
-- main = do
--   let src = [ "\\x. (1 + 2 * 3 / 4) 5"
--             , "\\x. 1 + 2 * 3 / 4 5"
--             , "\\x. 4 * (2 + 3)"
--             , "\\x. 4 * 2 + 3"
--             , "\\x. (2 + 3) * 4"
--             , "\\x. (2 + 3) * (4 + 5)"
--             , "(\\x. x) (\\x. x)"
--             , "1 + (2 * 3)"
--             , "(((1 + 2) * (3)))"
--             , "(\\x. (((1 + 2) * (3)))) 5"
--             , "(\\f. \\x. f x) (\\x. x + 1) 1"
--             , "(\\f. \\x. f + x) 11" 
--             , "(\\f. \\x. f + x) 1 2" 
--             , "(\\x. f + x) 1 2" 
--             ]
--   forM_ src (\src_i -> do
--     putStrLn "Original Source: "
--     putStrLn $ src_i ++ "\n"
-- 
--     putStrLn $ "Parsed Source: "
--     print_one_parsed_lambda src_i
--     putStrLn "\n\n")

main = let src =  "true" in putStrLn src >> print_one_parsed_lambda src
















