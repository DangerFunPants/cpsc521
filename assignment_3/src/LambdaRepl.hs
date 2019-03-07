{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaRepl where

import System.Console.Repline

import Data.List (isPrefixOf)
import Control.Monad.State.Strict
import Control.Lens

import Text.Show.Pretty

import qualified LambdaParser as L
import qualified AbstractMachine as A
import qualified SymbolTable as S
import qualified LambdaPrinter as PP
import qualified Typer as T

type Repl a = HaskelineT IO a
type ReplState s a = HaskelineT (StateT s IO) a


-- ****************************************************************************
--                         Global State of the REPL
-- ****************************************************************************
-- @Cleanup: Thus has to be at the top of the file in order for the makeLenses
-- splice to be visible. Should move to another file or something.
data Repl_State = Repl_State
  { _global_bindings    :: [L.Binding]
  , _type_declarations  :: [L.Lambda_Declaration]
  } 
  deriving (Show, Eq)
makeLenses ''Repl_State

-- ****************************************************************************
--                                  Helpers
-- ****************************************************************************
get_lambda_from_args :: [String] -> Maybe String
get_lambda_from_args args = 
  if null args
    then Nothing
    else Just $ tail (foldl (\acc v -> acc ++ " " ++ v) "" args)

get_filepath_from_args :: [String] -> Maybe String
get_filepath_from_args = get_lambda_from_args

-- ****************************************************************************
--                            State Helpers
-- ****************************************************************************
add_global_binding :: L.Binding -> Lambda_Repl ()
add_global_binding binding = modify $ (over global_bindings (++ [binding]))

add_global_bindings :: [L.Binding] -> Lambda_Repl ()
add_global_bindings binding_list = modify $ (over global_bindings (binding_list ++))

get_global_bindings :: Lambda_Repl [L.Binding]
get_global_bindings = get >>= \st -> return (st^.global_bindings)

get_global_symbol_names :: Lambda_Repl [String]
get_global_symbol_names = 
  get >>= \st -> return $ bindings_to_names (st^.global_bindings)

bindings_to_names :: [L.Binding] -> [String]
bindings_to_names = fmap binding_to_name
binding_to_name :: L.Binding -> String
binding_to_name (L.Binding name expr) = name
binding_to_name (L.RecBinding name expr) = name

clear_all_global_bindings :: Lambda_Repl ()
clear_all_global_bindings = modify $ (set global_bindings [])

clear_global_binding :: String -> Lambda_Repl ()
clear_global_binding bound_expr_name = 
  modify $ (over global_bindings (delete_binding bound_expr_name))
  where
    delete_binding target_name (binding@(L.Binding name _):xs) = 
      if target_name == name
        then xs
        else binding : (delete_binding target_name xs)

parse_and_inject_binding :: String -> Lambda_Repl ()
parse_and_inject_binding src = 
  case get_lambda_from_args [src] of 
    Nothing -> return ()
    Just the_lambda ->
      case L.parse_expression_binding the_lambda of
        Left err -> liftIO $ putStrLn err
        Right (L.Binding name expr) -> do
          expr_type <- type_expression_with_global_bindings expr
          case expr_type of
            Left type_error -> liftIO $ putStrLn type_error
            Right _ -> do
              liftIO $ putStrLn $ "Type: " ++ (show expr_type)
              add_global_binding (L.Binding name expr)
              the_new_state <- get
              liftIO $ putStrLn $ ppShow the_new_state

type_expression_with_global_bindings :: L.Lambda_Expr -> Lambda_Repl (Either String T.Type)
type_expression_with_global_bindings lambda = do
  global_bindings <- get_global_bindings
  let expr_type = T.type_expression_with_initial_state global_bindings lambda
  return expr_type

-- ****************************************************************************
--             Command Definition and Autocompletion (Top Level)
-- ****************************************************************************
type Lambda_Repl a = ReplState Repl_State a
  
mk_init_state_for_lambda :: Repl_State
mk_init_state_for_lambda = Repl_State []

lambda_repl_init :: Lambda_Repl ()
lambda_repl_init = return ()

global_symbol_completer :: (MonadState Repl_State m) => CompletionFunc m
global_symbol_completer (s1, s2) = do
  st <- get
  let ns = bindings_to_names (st^.global_bindings)
  -- ns <- get_global_symbol_names
  -- Why does the above line not work? 
  --  st :: Repl_State
  listCompleter ns (s1, s2)

top_level_matcher :: (MonadIO m, MonadState Repl_State m) => [(String, CompletionFunc m)]
top_level_matcher = completer_list
  where
    completer_list = [ (":parse"   , listCompleter [])
                     , (":q"       , listCompleter [])
                     , (":exec"    , global_symbol_completer)
                     , (":compile" , listCompleter [])
                     , (":debug"   , global_symbol_completer)
                     , (":load"    , fileCompleter)
                     , (":print"   , global_symbol_completer)
                     , (":free"    , global_symbol_completer)
                     , (":type"    , global_symbol_completer)
                     , (":bindings", listCompleter [])
                     ]

top_level_prefix_completer :: (Monad m, MonadState Repl_State m) => WordCompleter m
top_level_prefix_completer n = do
  let names = [ ":parse"
              , ":q"
              , ":exec"
              , ":compile"
              , ":debug"
              , ":load"
              , ":print"
              , ":free"
              , ":type"
              , ":bindings"
              ]
  return $ filter (isPrefixOf n) names


top_level_opts :: [(String, [String] -> Lambda_Repl ())]
top_level_opts = [ ("parse"   , parse)
                 , ("q"       , quit)
                 , ("exec"    , exec)
                 , ("compile" , compile)
                 , ("debug"   , debug)
                 , ("load"    , load)
                 , ("print"   , print_bindings)
                 , ("free"    , free)
                 , ("type"    , print_expression_type)
                 , ("bindings", print_bindings_with_no_types)
                 ]

-- ****************************************************************************
--                          Command Implementation (Top Level)
-- ****************************************************************************
top_level_cmd :: String -> Lambda_Repl ()
top_level_cmd = parse_and_inject_binding

parse :: [String] -> Lambda_Repl ()
parse args = do
  case get_lambda_from_args args of
    Nothing -> do
      liftIO $ putStrLn "Must provide a lambda"
      return ()
    Just the_lambda -> liftIO $ do
      let parsed_lambda = L.parse_lambda the_lambda
      putStrLn "Original Source: "
      putStrLn $ (show the_lambda) ++ "\n"
      case parsed_lambda of 
        Left err -> do
          putStrLn err
        Right lambda_ast -> do
          putStrLn "Lambda Abstract SyntaxTree: "
          putStrLn $ (ppShow $ lambda_ast) ++ "\n"
          putStrLn "Formatted Lambda: "
          putStrLn $ show $ PP.print_lambda lambda_ast

quit :: [String] -> Lambda_Repl ()
quit args = abort

exec :: [String] -> Lambda_Repl ()
exec args = do
  case get_lambda_from_args args of
    Nothing -> liftIO $ putStrLn "Must provided a lambda" >> return ()
    Just the_lambda -> do
      global_bindings <- get_global_bindings
      let exec_state = A.execute_lambda_and_show_state the_lambda global_bindings False
      case exec_state of 
        (Left err_msg) -> liftIO $ do
          putStrLn $ "Lambda execution failed:"
          putStrLn err_msg
        (Right (ast, instrs, exec_state, info)) -> liftIO $ do
          show_exec_result exec_state
  where 
    show_exec_result exec_state =
      if null the_stack
        then putStrLn $ "Finished execution in an invalid state."
        else putStrLn $ show pretty_expr
      where
        the_stack = A._stack exec_state
        pretty_expr = head the_stack


compile :: [String] -> Lambda_Repl ()
compile args =
  case get_lambda_from_args args of
    Nothing -> liftIO $ putStrLn "Must provide a lambda." >> return ()
    Just the_lambda -> do
      global_bindings <- get_global_bindings
      let maybe_src = A.parse_and_compile the_lambda global_bindings
      case maybe_src of
        Left err -> liftIO $ putStrLn err
        Right code -> liftIO $ do
          putStrLn $ "********************************************************************************"
          putStrLn "Input Source: "
          putStrLn $ the_lambda ++ "\n"
          putStrLn $ "Instructions: "
          putStrLn $ (ppShow code) ++ "\n"
          putStrLn $ "********************************************************************************"

debug :: [String] -> Lambda_Repl ()
debug args = get_global_bindings >>= \v -> liftIO $ debug_repl args v

load :: [String] -> Lambda_Repl ()
load args = do
  case get_filepath_from_args args of
    Nothing -> liftIO $ putStrLn "Must provide a file path."
    Just file_path -> do
      file_contents <- liftIO $ readFile file_path
      let lambda_defns = lines file_contents
          binding_list = L.parse_lambda_file lambda_defns
      add_global_bindings binding_list

print_bindings :: [String] -> Lambda_Repl ()
print_bindings _ = do
  bindings <- get_global_bindings
  let pretty_bindings = fmap (decl_of_binding bindings) bindings
  liftIO $ mapM_ (\v -> putStrLn $ "    " ++ v) pretty_bindings
  where
    -- @Hack: Passing the var name as the expression. Should be more clear.
    decl_of_binding :: [L.Binding] -> L.Binding -> String
    decl_of_binding global_bindings (L.Binding name expr) = 
      case T.type_expression_with_initial_state global_bindings expr of
        Left err -> err
        Right expr_type -> PP.print_type_declaration (L.Var name) expr_type
    decl_of_binding global_bindings (L.RecBinding name expr) = 
      case T.type_expression_with_initial_state global_bindings expr of
        Left err -> err
        Right expr_type -> PP.print_type_declaration (L.Var name) expr_type

free :: [String] -> Lambda_Repl ()
free args = 
  if null args 
    then clear_all_global_bindings >> (liftIO $ putStrLn "Cleared all global bindings.")
    else 
      forM_ args (\name_to_free -> do
        clear_global_binding name_to_free)

print_expression_type :: [String] -> Lambda_Repl ()
print_expression_type args = 
  case get_lambda_from_args args of
    Nothing -> liftIO $ (putStrLn "Must provide a lambda.")
    Just the_lambda -> do
    case L.parse_lambda the_lambda of
      Left err -> liftIO $ putStrLn err
      Right ast -> do
        maybe_expr_type <- type_expression_with_global_bindings ast
        case maybe_expr_type of 
          Left err -> liftIO $ putStrLn $ "Type Error: " ++ err
          Right expr_type -> liftIO $ putStrLn $ ("   " ++ PP.print_type_declaration ast expr_type)

print_bindings_with_no_types :: [String] -> Lambda_Repl ()
print_bindings_with_no_types args = do
  bindings <- get_global_bindings
  liftIO $ putStrLn $ ppShow bindings

-- ****************************************************************************
--             Command Definition and Autocompletion (Debug Mode)
-- ****************************************************************************
type Debug_Repl a = ReplState A.StateType a

debug_repl_init :: Debug_Repl ()
debug_repl_init = return ()

debug_matcher :: MonadIO m => [(String, CompletionFunc m)]
debug_matcher = [ (":next"    , listCompleter [])
                , (":continue", listCompleter [])
                , (":q"       , listCompleter [])
                , (":list"    , listCompleter [])
                ]

debug_prefix_completer :: Monad m => WordCompleter m
debug_prefix_completer n = do
  let names = [ ":next"
              , ":continue"
              , ":q"
              , ":list"
              ]
  return $ filter (isPrefixOf n) names

debug_opts :: [(String, [String] -> Debug_Repl ())]
debug_opts = [ ("next"    , next)
             , ("continue", continue)
             , ("q"       , quit_debug)
             , ("list"    , list)
             ]

-- ****************************************************************************
--                      Command Implementation (Debug Mode)
-- ****************************************************************************
debug_cmd :: String -> Debug_Repl ()
debug_cmd "n" = next []
debug_cmd _ = liftIO $ putStrLn "Unrecognized debug command."

next :: [String] -> Debug_Repl ()
next args = do
  current_state <- get
  case A.exec_code_dbg current_state of
    Left (err_msg, err_state) -> do
      liftIO $ putStrLn err_msg
      abort
    Right new_state -> do
      modify $ \old -> new_state
      liftIO $ putStrLn $ ppShow new_state

list :: [String] -> Debug_Repl ()
list args = do
  current_state <- get
  liftIO $ putStrLn $ ppShow current_state

continue :: [String] -> Debug_Repl ()
continue args = do
  current_state <- get
  case A.exec_to_completion current_state of  
    Left (err_msg, err_state) -> do 
      liftIO $ putStrLn err_msg
      liftIO $ putStrLn $ ppShow err_state
      abort
    Right success_state -> do
      liftIO $ putStrLn $ ppShow success_state
      abort

quit_debug :: [String] -> Debug_Repl ()
quit_debug args = abort

-- ****************************************************************************
--                     REPL Initialization and Creation
-- ****************************************************************************
debug_repl :: [String] -> [L.Binding] -> IO ()
debug_repl init_state global_bindings = do
  case get_lambda_from_args init_state of
    Nothing -> do
      putStrLn "Must provide a lambda."
      return ()
    Just the_lambda -> do
      case A.parse_and_compile the_lambda global_bindings of
        Left err -> putStrLn err
        Right instrs -> do
          let start_state = A.mk_state_for_dbg_execution instrs
          exec_func start_state
  where
    prompt = pure " \x03BB debug> "
    command_prefix = (Just ':')
    prefix_completer = Prefix (wordCompleter debug_prefix_completer) debug_matcher
    initial_state = debug_repl_init
    eval_func = evalRepl prompt debug_cmd debug_opts command_prefix prefix_completer initial_state
    exec_func st = flip evalStateT st eval_func

lambda_repl :: IO ()
lambda_repl = exec_func mk_init_state_for_lambda
  where
    prompt = pure " \x03BB> "
    command_prefix = Just ':'
    prefix_completer = Prefix (wordCompleter top_level_prefix_completer) top_level_matcher
    initial_state = lambda_repl_init
    eval_func = evalRepl prompt top_level_cmd top_level_opts command_prefix prefix_completer initial_state
    exec_func st = flip evalStateT st eval_func

main :: IO ()
main = lambda_repl 
