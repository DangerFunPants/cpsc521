module LambdaParser 
  ( parse_lambda
  , parse_expression_binding
  , introduce_global_bindings
  , parse_lambda_file
  , Lambda_Expr (..)
  , Binding (..)
  ) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Number
import Text.Parsec
import Text.ParserCombinators.Parsec.Error

import Text.Show.Pretty
import Control.Monad
import Data.Functor.Identity (Identity)


-- ****************************************************************************
--                               Data Structures
-- ****************************************************************************
data Lambda_Expr
  = Var String
  | Abstraction String Lambda_Expr
  | Application Lambda_Expr Lambda_Expr
  | Add Lambda_Expr Lambda_Expr
  | Sub Lambda_Expr Lambda_Expr
  | Mul Lambda_Expr Lambda_Expr
  | Div Lambda_Expr Lambda_Expr
  | LessThan Lambda_Expr Lambda_Expr
  | Equal Lambda_Expr Lambda_Expr
  | Conditional Lambda_Expr Lambda_Expr Lambda_Expr
  | IntLiteral Int
  | BoolLiteral Bool
  -- Native fixed point combinator. 
  -- One day I'll understand what that means.
  -- maybe...
  | Fix Lambda_Expr 
  | Let [Binding] Lambda_Expr
  | Cons Lambda_Expr Lambda_Expr
  | Nil
  | Case Lambda_Expr Lambda_Expr Lambda_Expr
  deriving (Show, Eq)

data Binding
  = Binding Name Lambda_Expr
  deriving (Show, Eq)

data BinaryOperator
  = BinaryMinus
  | BinaryPlus
  | BinaryStar
  | BinarySlash
  | BinaryLeftAngle
  | BinaryEquality
  deriving (Show, Eq)

-- ParsecT s u m a
-- s: stream input type
-- u: user state
-- m: next monad in the stack
-- a: output type
type Parser a = ParsecT String () Identity a

-- ****************************************************************************
--                                Reserved Keywords
-- ****************************************************************************
-- None of the reserved keywords can be used in identifiers.
reserved_keywords :: [String]
reserved_keywords = [ "if"
                    , "then"
                    , "else"
                    , "true"
                    , "false"
                    , "let"
                    , "in"
                    , "cons"
                    , "nil"
                    , "case"
                    ]

-- ****************************************************************************
--                        Lambda Definition File Parser
-- ****************************************************************************
lambda_file_parser :: Parser (Maybe Binding)
lambda_file_parser = do
  comment_line <- optionMaybe comment_parser
  case comment_line of
    Nothing -> do
      the_binding <- bind_expression_parser
      return $ Just the_binding
    Just () -> return Nothing

comment_parser :: Parser ()
comment_parser = do
  comment_token_parser
  manyTill anyChar (try endOfLine)
  return ()

comment_token_parser :: Parser ()
comment_token_parser = char '#' >> return ()

parse_lambda_file :: [String] -> [Binding]
parse_lambda_file [] = []
parse_lambda_file (x:xs) = do
  case parse lambda_file_parser "" x of
    Left parse_error -> parse_lambda_file xs
    Right parse_result -> 
      case parse_result of  
        Nothing -> parse_lambda_file xs
        Just binding -> binding : (parse_lambda_file xs)

-- ****************************************************************************
--                            Repl Name Binding Parser
-- ****************************************************************************
bind_expression_parser :: Parser Binding
bind_expression_parser = do
  let_token_parser
  spaces
  lhs <- ident_parser
  spaces
  binary_equality_parser
  spaces
  rhs <- lambda_parser
  eof
  return $ Binding lhs rhs

parse_expression_binding :: String -> Either String Binding
parse_expression_binding src = 
  case parse bind_expression_parser "" src of
    Left err -> 
      let error_loc = show $ errorPos err
          error_msg = messageString $ head $ errorMessages err
      in Left $ "Parse error at " ++ show err
    Right the_expr -> Right the_expr

introduce_global_bindings :: [Binding] -> Lambda_Expr -> Lambda_Expr
introduce_global_bindings global_bindings top_level_expr = 
  Let global_bindings top_level_expr

-- ****************************************************************************
--                                Top Level Parser
-- ****************************************************************************
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
  expr <- choice $ fmap try [ case_expression_parser
                            , let_binding_parser
                            , fix_parser
                            , conditional_parser
                            , app_parser
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
case_expression_parser :: Parser Lambda_Expr
case_expression_parser = do 
  case_token_parser
  spaces
  predicate_expression <- case_branch_parser
  spaces
  nil_case <- case_branch_parser
  spaces
  cons_case <- case_branch_parser
  return $ Case predicate_expression nil_case cons_case
  where
    case_branch_parser = bracketed_expression_parser

let_binding_parser :: Parser Lambda_Expr
let_binding_parser = do
  let_token_parser
  spaces
  bindings <- binding_parser 
  spaces
  in_token_parser
  spaces
  expression <- lambda_parser
  return $ Let bindings expression
  where
    binding_parser = do
      name <- ident_parser
      spaces
      binary_equality_parser
      spaces
      expr_to_bind <- lambda_parser
      more_bindings <- more_bindings_parser
      return $ (Binding name expr_to_bind) : more_bindings
    more_bindings_parser = do
      maybe_separator <- optionMaybe let_binding_separator
      spaces
      case maybe_separator of
        Nothing -> return []
        (Just _) -> binding_parser

fix_parser :: Parser Lambda_Expr
fix_parser = do
  fix_token_parser
  spaces
  the_fixed_expr <- lambda_parser
  return $ Fix the_fixed_expr

conditional_parser :: Parser Lambda_Expr
conditional_parser = do
  if_token_parser
  spaces
  predicate <- lambda_parser
  spaces
  then_token_parser
  spaces
  then_clause <- lambda_parser
  spaces
  else_token_parser
  spaces
  else_clause <- lambda_parser
  spaces
  return $ Conditional predicate then_clause else_clause

app_parser :: Parser Lambda_Expr
app_parser = do 
  lh_term <- app_term_parser
  spaces
  rh_term <- app_term_parser
  let the_app_term = Application lh_term rh_term
  continuation_parser the_app_term
  where
    app_term_parser = choice $ fmap try [ let_binding_parser
                                        , fix_parser
                                        , conditional_parser
                                        , bracketed_expression_parser
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
      expr <- choice $ fmap try [ let_binding_parser
                                , fix_parser
                                , conditional_parser
                                , bracketed_expression_parser
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
                                  BinaryLeftAngle -> LessThan
                                  BinaryEquality -> Equal
      return binary_constructor
    operator_token_parser = do
      operator_token <- choice $ fmap try [ binary_minus_parser
                                          , binary_plus_parser
                                          , binary_star_parser
                                          , binary_slash_parser
                                          , binary_left_angle_parser
                                          , binary_equality_parser
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
                                   , list_literal_parser
                                   ]
  return the_literal

list_literal_parser = do
  list_literal <- choice $ fmap try [cons_parser, nil_parser]
  return list_literal

cons_parser :: Parser Lambda_Expr
cons_parser = do
  cons_token_parser
  spaces
  first_expr <- list_element_parser
  spaces
  second_expr <- list_element_parser
  return $ Cons first_expr second_expr
  where
    list_element_parser = do
      expr <- choice $ fmap try [ bracketed_expression_parser
                                , literal_parser
                                , var_parser
                                ]
      return expr

nil_parser :: Parser Lambda_Expr
nil_parser = nil_token_parser >> return Nil

-- ****************************************************************************
--                          Helpers and Leaves                        
-- ****************************************************************************
ident_parser :: Parser String
ident_parser = do
  the_ident <- many1 letter
  if not $ the_ident `elem` reserved_keywords
    then return the_ident
    else parserZero

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

binary_star_parser:: Parser BinaryOperator
binary_star_parser = char '*' >> return BinaryStar

binary_slash_parser :: Parser BinaryOperator
binary_slash_parser = char '/' >> return BinarySlash

binary_left_angle_parser :: Parser BinaryOperator
binary_left_angle_parser = char '<' >> return BinaryLeftAngle

binary_equality_parser :: Parser BinaryOperator
binary_equality_parser = char '=' >> return BinaryEquality

opening_paren :: Parser ()
opening_paren = char '(' >> return ()

closing_paren :: Parser ()
closing_paren = char ')' >> return ()

if_token_parser :: Parser ()
if_token_parser = string "if" >> return ()

then_token_parser :: Parser ()
then_token_parser = string "then" >> return ()

else_token_parser :: Parser ()
else_token_parser = string "else" >> return ()

fix_token_parser :: Parser ()
fix_token_parser = string "fix" >> return ()

let_token_parser :: Parser ()
let_token_parser = string "let" >> return ()

in_token_parser :: Parser ()
in_token_parser = string "in" >> return ()

let_binding_separator :: Parser ()
let_binding_separator = char ';' >> return ()

cons_token_parser :: Parser ()
cons_token_parser = string "cons" >> return ()

nil_token_parser :: Parser ()
nil_token_parser = string "nil" >> return ()

case_token_parser :: Parser ()
case_token_parser = string "case" >> return ()

-- ****************************************************************************
--                          Exported Functions
-- ****************************************************************************
parse_lambda :: String -> Either String Lambda_Expr
parse_lambda src =
  case parse top_level_lambda_parser "" src of
    Left err -> Left $ 
      let error_loc = show $ errorPos err
          error_msg = messageString $ head $ errorMessages err
      in "Parse error at " ++ show err
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
            , "if (\\x. true) 5 then 1 + 1 * 2 else (2 + 3)"
            , "let add = \\x. \\y. x + y in add 1 2"
            , "(\\x. \\y. let add = \\a. \\b. a + b in add x y) 1 2"
            , "let add = \\x. \\y. x + y; sub = \\x. \\y. x - y; a = 5; b = 2 in (add a a) - (add b b)"
            , "cons (0 + 1) (cons (0 + 2) (cons (0 + 3) nil))"
            , "cons (\\x. \\y. x + y) nil"
            , "\\x. let lst = cons 1 nil in lst"
            , "case (cons 5 nil) (\\x. 5) (\\y. 5)"
            , "let v = cons 5 nil in case (v) (\\x. head) (\\y. 5)"
            ]
  forM_ src (\src_i -> do
    putStrLn "Original Source: "
    putStrLn $ src_i ++ "\n"

    putStrLn $ "Parsed Source: "
    print_one_parsed_lambda src_i
    putStrLn "\n\n")

