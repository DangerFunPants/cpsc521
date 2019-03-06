module LambdaPrinter where

{-# LANGUAGE TemplateHaskell #-}

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

import Text.Show.Pretty
import Data.Char

import qualified Data.Map as M
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Identity

import qualified LambdaParser as L
import qualified Typer as T

type Printer_State a = StateT State_Type Identity a
data State_Type = State_Type
  { _label_map      :: M.Map Int Int
  , _current_label  :: Int
  }
makeLenses ''State_Type

-- ****************************************************************************
--                          Type Declaration Printer
-- ****************************************************************************
print_type_declaration :: L.Lambda_Expr -> T.Type -> String
print_type_declaration expr expr_type = show $ print_type_declaration_doc expr expr_type

print_type_declaration_doc :: L.Lambda_Expr -> T.Type -> Doc ann
print_type_declaration_doc expr expr_type = sep [expr_doc, (pretty "::"), type_doc]
  where
    expr_doc = print_lambda expr
    type_doc = run_printer_state expr_type

-- ****************************************************************************
--                                Type Printer
-- ****************************************************************************

mk_new_label :: Printer_State Int
mk_new_label = do
  st <- get
  let current = st^.current_label
  put $ over current_label (+1) st
  return current

get_label :: Int -> Printer_State Int
get_label label = do  
  st <- get
  let map = st^.label_map
  case M.lookup label map of
    Nothing -> do 
      new_label <- mk_new_label
      st <- get
      put $ over label_map (M.insert label new_label) st
      return new_label
    (Just new_label) -> return new_label

run_printer_state :: T.Type -> Doc ann
run_printer_state expr_type = exec_res
  where
    exec_res = runIdentity $  evalStateT (print_type_doc expr_type) init_state
    init_state = State_Type M.empty 0

print_type :: T.Type -> String
print_type expr_type = show $ run_printer_state expr_type

print_type_doc :: T.Type -> Printer_State (Doc ann)
print_type_doc (T.Type_Int) = return $ pretty "Int"
print_type_doc (T.Type_Bool) = return $ pretty "Bool" 
print_type_doc (T.Abstraction from_type to_type) = do
  from_type_doc <- print_type_doc from_type
  to_type_doc <- print_type_doc to_type
  let arrow = pretty "\x21A6"
  return $ sep [from_type_doc, arrow, to_type_doc]

print_type_doc (T.Type_Variable t_var) = do
  label_num <- get_label t_var
  return $ pretty $ lowercase_greek_letter_code_point label_num
  where 
    lowercase_greek_letter_code_point v = chr $ v + 0x03b1

-- ****************************************************************************
--                          Binding Pretty Printer
-- ****************************************************************************
-- let add = \x. \y. x + y
print_binding :: L.Binding -> String
print_binding binding = show $ print_binding_doc binding

print_binding_doc :: L.Binding -> Doc ann
print_binding_doc (L.Binding name lambda_expr) = sep [name_doc, pretty ":=", lambda_doc]
  where
    name_doc = pretty name
    lambda_doc = print_lambda lambda_expr

print_binding_doc (L.RecBinding name lambda_expr) = sep [name_doc, pretty ":=", lambda_doc]
  where
    name_doc = pretty name
    lambda_doc = print_lambda lambda_expr

print_bindings_doc :: [L.Binding] -> Doc ann
print_bindings_doc bindings = sep $ zipWith (<+>) (pretty "" : repeat (pretty ";")) pretty_bindings
  where
    pretty_bindings = fmap print_binding_doc bindings

print_bindings :: [L.Binding] -> String
print_bindings = show . print_bindings_doc


-- ****************************************************************************
--                          Lambda Pretty Printer
-- ****************************************************************************
print_lambda :: L.Lambda_Expr -> Doc ann
print_lambda (L.Var var_ident) = pretty var_ident

print_lambda (L.Application func arg) = parens $ sep [func_doc, arg_doc]
  where
    func_doc = print_lambda func
    arg_doc = print_lambda arg

print_lambda (L.Abstraction arg_ident expr_body) = sep [ pretty "\x03BB"
                                                       , arg_doc
                                                       , pretty "."
                                                       , expr_body_doc
                                                       ]
  where
    arg_doc = pretty arg_ident
    expr_body_doc = print_lambda expr_body

print_lambda (L.Sub lhs rhs) = print_binary_op "-" lhs rhs

print_lambda (L.Add lhs rhs) = print_binary_op "+" lhs rhs

print_lambda (L.Mul lhs rhs) = print_binary_op "*" lhs rhs

print_lambda (L.Div lhs rhs) = print_binary_op "/" lhs rhs

print_lambda (L.LessThan lhs rhs) = print_binary_op "<" lhs rhs

print_lambda (L.Equal lhs rhs) = print_binary_op "=" lhs rhs

print_lambda (L.Conditional predicate then_expr else_expr) =
  if_tok <+> (align (vsep [pred_doc, sep [then_tok, then_doc], sep [else_tok, else_doc]]))
  where
    if_tok    = pretty "if"
    then_tok  = pretty "then"
    else_tok  = pretty "else"
    pred_doc  = print_lambda predicate
    then_doc  = print_lambda then_expr
    else_doc  = print_lambda else_expr

print_lambda (L.IntLiteral v) = pretty v

print_lambda (L.BoolLiteral b) = pretty b

print_lambda (L.Fix expr) = undefined

print_lambda (L.Let bindings expr_body) = sep binding_list
  where
    let_doc = pretty "let"
    bindings_doc = print_bindings_doc bindings
    in_doc = pretty "in"
    expr_body_doc = print_lambda expr_body
    binding_list = [ let_doc
                   , bindings_doc
                   , in_doc
                   , expr_body_doc
                   ]

print_lambda (L.Cons first rest) = undefined

print_lambda (L.Nil) = undefined

print_lambda (L.Case pred_expr nil_expr cons_expr) = undefined

-- ****************************************************************************
--                                  Helpers
-- ****************************************************************************
print_binary_op :: String -> L.Lambda_Expr -> L.Lambda_Expr -> Doc ann
print_binary_op op_cygil lhs rhs = sep [lhs_doc, op_cygil_doc, rhs_doc]
  where
    lhs_doc = print_lambda lhs
    rhs_doc = print_lambda rhs
    op_cygil_doc = pretty op_cygil

main :: IO ()
main = putStrLn "Hola Mundas!"



