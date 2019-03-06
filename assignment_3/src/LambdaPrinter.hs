module LambdaPrinter where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

import Text.Show.Pretty

import qualified LambdaParser as L


-- ****************************************************************************
--                          Binding Pretty Printer
-- ****************************************************************************
-- let add = \x. \y. x + y
print_binding :: L.Binding -> String
print_binding (L.Binding name lambda_expr) = show $ sep [name_doc, pretty ":=", lambda_doc]
  where
    name_doc = pretty name
    lambda_doc = print_lambda lambda_expr

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
  -- sep [pretty "if", pred_doc, pretty "then", then_doc, pretty "else", else_doc]
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

print_lambda (L.Let bindings expr_body) = undefined

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



