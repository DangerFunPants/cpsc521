module Typer 
  ( type_expression
  , Type (..)
  , mk_initial_type_state
  , type_expression_with_initial_state
  ) where

{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.State.Strict
import Control.Monad.Identity
import Control.Monad.Except
import Control.Lens
import Text.Show.Pretty
import qualified Data.Map as M

import qualified LambdaParser as L

-- \x. x
--
-- Abstraction "x" (Var "x")
--
-- * From the appearance of the expression Var "x" in the body of the lambda, a new free type variable X is introducd. 
--   this also entails the type equation x :: X |- x :: Y <Y = X> which states "Given that x inhabits some type X, if
--   x also inhabits type Y then Y = X.
--
-- * Then from the abstraction it is deduced that (\x. x) :: Q, exists. X, Y. Q = X -> Y, Y = X
--
-- \x. x
-- E |- x::X -> 

data Type
  = Type_Int
  | Type_Bool
  | Abstraction Type Type
  | Type_Variable Int -- A free variable
  deriving (Show, Eq)

-- At each level of the AST it is possible to introduce one or more type variables and one or more type equations
-- which relate new type variables to existing type variables
data Type_Equation
  = Type_Introduction [Type] [Type_Equation]
  | Type_Equality Type Type
  deriving (Show, Eq)

data Type_State = Type_State
  { _label :: Int
  , _environment :: M.Map String Type
  }
  deriving (Show, Eq)

makeLenses ''Type_State

-- Monad m => StateT s m a
type Typer_State a = StateT Type_State (ExceptT String Identity) a

mk_empty_type_state :: Type_State
mk_empty_type_state = Type_State 1 M.empty

mk_initial_type_state :: [L.Binding] -> M.Map String Type
mk_initial_type_state (b:bs) = 
  let (binding_ident, binding_type) = process_one_binding b
      rec_call = mk_initial_type_state bs
  in M.insert binding_ident binding_type rec_call
  where
    process_one_binding :: L.Binding -> (String, Type)
    process_one_binding (L.Binding name lambda_expr) = (name, expr_type)
      where
        (Right expr_type) = type_expression lambda_expr
    process_one_binding (L.RecBinding name lambda_expr) = (name, expr_type)
      where
        (Right expr_type) = type_expression lambda_expr

runTyperState :: Typer_State a -> Either String a
runTyperState typer_state = 
  case exec_result of 
    Left err -> Left err
    Right a -> Right a
  where
    exec_result = runIdentity $ runExceptT $ evalStateT typer_state mk_empty_type_state
  

-- ****************************************************************************
--                          State Manipulation and Access
-- ****************************************************************************
create_type_variable :: Typer_State Type
create_type_variable = do
  current_state <- get
  let current_label_num = current_state^.label
  modify (set label (current_label_num + 1))
  return $ Type_Variable current_label_num

lookup_in_env :: String -> Typer_State Type
lookup_in_env ident = do
  current_state <- get
  let the_env = current_state^.environment
      (Just ident_type) = M.lookup ident the_env
  return ident_type

insert_into_env :: String -> Type -> Typer_State ()
insert_into_env var_ident var_type = do
  modify $ over environment (M.insert var_ident var_type)

remove_from_env :: String -> Typer_State ()
remove_from_env var_ident = do
  modify $ over environment (M.delete var_ident)

-- ****************************************************************************
--                          Constraint Generation
-- ****************************************************************************
collect_constraints :: Type -> L.Lambda_Expr -> Typer_State Type_Equation
-- introduce the contraint that the type at use U(x) is the same 
-- as the type of the definition D(x). To determine the type of 
-- the definition, consult G(x) 
collect_constraints use_type (L.Var var_ident) = do
  defined_type <- lookup_in_env var_ident
  let the_constraint = Type_Equality defined_type use_type
  return $ Type_Introduction [] [the_constraint]

collect_constraints use_type (L.Let bindings expr_body) = do
  Type_Introduction _ binding_constraints <- collect_constraints_from_bindings bindings
  body_type <- create_type_variable
  Type_Introduction _ body_constraints <- collect_constraints body_type expr_body
  let the_constraints = (Type_Equality use_type body_type) : (binding_constraints ++ body_constraints)
  return $ Type_Introduction [] the_constraints

collect_constraints use_type (L.Abstraction var_ident expr_body) = do
  from_type <- create_type_variable
  to_type <- create_type_variable
  let the_constraint = Type_Equality use_type (Abstraction from_type to_type)
  insert_into_env var_ident from_type
  Type_Introduction vars eqs <- collect_constraints to_type expr_body
  let new_vars = from_type : (to_type : vars)
      new_eqs = the_constraint : eqs
  return $ Type_Introduction new_vars new_eqs

collect_constraints use_type (L.Application func arg) = do
  func_type <- create_type_variable
  arg_type <- create_type_variable
  Type_Introduction _ func_constraints <- collect_constraints func_type func
  Type_Introduction _ arg_constraints <- collect_constraints arg_type arg
  let the_constraint = Type_Equality func_type (Abstraction arg_type use_type)
      constraint_list = the_constraint : (func_constraints ++ arg_constraints)
  return $ Type_Introduction [] constraint_list

collect_constraints use_type (L.IntLiteral _) = do
  let the_constraint = Type_Equality Type_Int use_type 
  return $ Type_Introduction [] [the_constraint]

collect_constraints use_type (L.BoolLiteral _) = do
  let the_constraint = Type_Equality Type_Bool use_type
  return $ Type_Introduction [] [the_constraint]

collect_constraints use_type (L.Add lhs_expr rhs_expr) = 
  collect_constraints_for_int_bin_op use_type lhs_expr rhs_expr

collect_constraints use_type (L.Sub lhs_expr rhs_expr) = 
  collect_constraints_for_int_bin_op use_type lhs_expr rhs_expr

collect_constraints use_type (L.Mul lhs_expr rhs_expr) = 
  collect_constraints_for_int_bin_op use_type lhs_expr rhs_expr

collect_constraints use_type (L.Div lhs_expr rhs_expr) = 
  collect_constraints_for_int_bin_op use_type lhs_expr rhs_expr

collect_constraints use_type (L.LessThan lhs_expr rhs_expr) =
  collect_constraints_for_bool_bin_op use_type lhs_expr rhs_expr

collect_constraints use_type (L.Equal lhs_expr rhs_expr) = 
  collect_constraints_for_bool_bin_op use_type lhs_expr rhs_expr

collect_constraints use_type (L.Conditional pred then_expr else_expr) = do
  Type_Introduction _ pred_constraints <- collect_constraints Type_Bool pred
  then_type <- create_type_variable
  else_type <- create_type_variable
  Type_Introduction _ then_constraints <- collect_constraints then_type then_expr
  Type_Introduction _ else_constraints <- collect_constraints else_type else_expr
  let the_constraints = [ Type_Equality then_type else_type
                        , Type_Equality use_type then_type
                        ]
                        ++ pred_constraints
                        ++ then_constraints
                        ++ else_constraints
  return $ Type_Introduction [] the_constraints

collect_constraints_for_int_bin_op 
  :: Type -> L.Lambda_Expr -> L.Lambda_Expr -> Typer_State Type_Equation
collect_constraints_for_int_bin_op use_type lhs_expr rhs_expr = do
  Type_Introduction _ lhs_constraints <- collect_constraints Type_Int lhs_expr
  Type_Introduction _ rhs_constraints <- collect_constraints Type_Int rhs_expr
  let the_constraint = Type_Equality use_type Type_Int
  return $ Type_Introduction [] (the_constraint : (lhs_constraints ++ rhs_constraints))

collect_constraints_for_bool_bin_op
  :: Type -> L.Lambda_Expr -> L.Lambda_Expr -> Typer_State Type_Equation
collect_constraints_for_bool_bin_op use_type lhs_expr rhs_expr= do
  lhs_type <- create_type_variable
  rhs_type <- create_type_variable
  Type_Introduction _ lhs_constraints <- collect_constraints lhs_type lhs_expr
  Type_Introduction _ rhs_constraints <- collect_constraints rhs_type rhs_expr
  let the_constraints = [ Type_Equality Type_Bool use_type
                        , Type_Equality lhs_type rhs_type
                        ]
  return $ Type_Introduction [] (the_constraints ++ (lhs_constraints ++ rhs_constraints))

collect_constraints_from_bindings :: [L.Binding] -> Typer_State Type_Equation
collect_constraints_from_bindings [] = return $ Type_Introduction [] []
collect_constraints_from_bindings (b:bs) = do
  Type_Introduction _ constraints <- collect_constraints_from_one_binding b
  Type_Introduction _ rec_call <- collect_constraints_from_bindings bs
  return $ Type_Introduction [] (constraints ++ rec_call)

collect_constraints_from_one_binding :: L.Binding -> Typer_State Type_Equation
collect_constraints_from_one_binding (L.Binding name expr) = do
  expr_type <- create_type_variable
  Type_Introduction _ expr_constraints <- collect_constraints expr_type expr
  insert_into_env name expr_type
  return $ Type_Introduction [] expr_constraints
collect_constraints_from_one_binding (L.RecBinding name expr) = do
  expr_type <- create_type_variable
  Type_Introduction _ expr_constraints <- collect_constraints expr_type expr
  insert_into_env name expr_type
  return $ Type_Introduction [] expr_constraints

-- ****************************************************************************
--                              Substitutions
-- ****************************************************************************
data Substitution 
  = EmptySub
  | Sub (Type -> Type)

compose_substitutions :: Substitution -> Substitution -> Substitution
compose_substitutions EmptySub s = s
compose_substitutions s EmptySub = s
compose_substitutions (Sub s1) (Sub s2) = Sub $ s2 . s1

perform_substitution :: Type -> Type -> Type -> Type
perform_substitution (Type_Variable x) type_to_sub (Type_Variable x') = 
  if x == x'
    then type_to_sub
    else (Type_Variable x')

perform_substitution tv@(Type_Variable x) type_to_sub (Abstraction arg_t body_t) = Abstraction arg_sub body_sub
  where
    arg_sub = (perform_substitution tv type_to_sub arg_t) 
    body_sub =(perform_substitution tv type_to_sub body_t)

perform_substitution (Type_Variable x) type_to_sub (Type_Int) = Type_Int
perform_substitution (Type_Variable x) type_to_sub (Type_Bool) = Type_Bool

perform_substitution t1 t2 t3 = error $ show $ fmap show [t1, t2, t3]

make_substitution :: Type -> Type -> Substitution
make_substitution type_to_sub t@(Type_Variable _) = Sub $ \expr -> perform_substitution t type_to_sub expr

apply_substitution :: Substitution -> Type -> Type
apply_substitution (Sub sub_fn) t = sub_fn t

substitute_over_constraints :: Substitution -> [Type_Equation] -> [Type_Equation]
substitute_over_constraints s cs = fmap map_fn cs
  where
    map_fn (Type_Equality lhs rhs) = Type_Equality (apply_substitution s lhs) (apply_substitution s rhs)

-- ****************************************************************************
--                          Constraint Unification
-- ****************************************************************************
unify_constraints :: [Type_Equation] -> Substitution
unify_constraints [] = EmptySub

unify_constraints ((Type_Equality (Type_Variable x) t'):xs) = 
  case occurs_check (Type_Variable x) t' of
    True -> error "occurs_check"
    False -> compose_substitutions the_substitution (unify_constraints new_constraints)
  where
    the_substitution = make_substitution t' (Type_Variable x)
    new_constraints = substitute_over_constraints the_substitution xs
 
unify_constraints ((Type_Equality t (Type_Variable x)):xs) = 
  case occurs_check (Type_Variable x) t of
    True -> error "occurs_check"
    False -> compose_substitutions the_substitution (unify_constraints new_constraints)
  where
    the_substitution = make_substitution t (Type_Variable x)
    new_constraints = substitute_over_constraints the_substitution xs

unify_constraints (
  (Type_Equality 
    (Abstraction from_type to_type) 
    (Abstraction from_type' to_type'))
    :cs) = unify_constraints augmented_constraint_set
  where
    new_constraints = [ Type_Equality from_type from_type'
                      , Type_Equality to_type to_type'
                      ]
    augmented_constraint_set = new_constraints ++ cs

unify_constraints ((Type_Equality t t'):cs) = 
  if t == t'
    then unify_constraints cs
    else error "Failed to type expression."


occurs_check :: Type -> Type -> Bool
occurs_check (Type_Variable t1) (Type_Variable t2) = t1 == t2
occurs_check t1@(Type_Variable _) (Abstraction arg_type body_type) = 
  (occurs_check t1 arg_type) || (occurs_check t1 body_type)
occurs_check (Type_Variable _) Type_Int = False
occurs_check (Type_Variable _) Type_Bool = False

-- ****************************************************************************
--                            Exposed Functions
-- ****************************************************************************
type_expression :: L.Lambda_Expr -> Either String Type
-- type_expression lambda_expr = expr_type
--   where
--     Type_Introduction vs generated_constraints = 
--       case typer_exec_result of
--         Left err_msg -> Left err_msg
--         Right (Type_Introduction vs generated_constraints) ->
--     unification = unify_constraints generated_constraints
--     unification_result constraints = substitute_over_constraints unification constraints
--     expr_type = apply_substitution unification (Type_Variable 0)
--     typer_exec_result = runTyperState (collect_constraints (Type_Variable 0) lambda_expr)

type_expression lambda_expr = 
  case typer_exec_result of
    Left err_msg -> Left err_msg
    Right (Type_Introduction vs generated_constraints) ->
      let unification = unify_constraints generated_constraints
          unification_result = substitute_over_constraints unification generated_constraints
          expr_type = apply_substitution unification (Type_Variable 0)
      in Right expr_type
  where
    typer_exec_result = runTyperState (collect_constraints (Type_Variable 0) lambda_expr)
      

type_expression_with_initial_state :: [L.Binding] -> L.Lambda_Expr -> Either String Type
type_expression_with_initial_state bindings expr = type_expression augmented_expr
  where
    augmented_expr = L.Let bindings expr
    
-- ****************************************************************************
--                              Adhoc Testing
-- ****************************************************************************
main :: IO ()
main = do
  let ast = L.Abstraction "y" $ L.Abstraction "x" (L.Var "y")
      simple = L.Abstraction "x" (L.Div (L.Var "x") (L.IntLiteral 5))
      with_int = L.Abstraction "x" $ L.Add (L.Var "x") (L.IntLiteral 5)
      with_application = L.Application (L.Abstraction "x" (L.Var "x")) (L.IntLiteral 5)
      expr_type = type_expression with_int
  putStrLn $ "The expression is of type: " ++ (show expr_type)










