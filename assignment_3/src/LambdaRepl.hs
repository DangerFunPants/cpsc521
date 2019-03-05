{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaRepl where

import System.Console.Repline

import Data.List (isPrefixOf)
import Control.Monad.State.Strict
import Text.Show.Pretty

import LambdaParser as L
import AbstractMachine as A



-- ****************************************************************************
--                                  Helpers
-- ****************************************************************************
get_lambda_from_args :: [String] -> Maybe String
get_lambda_from_args args = 
  if null args
    then Nothing
    else Just $ tail (foldl (\acc v -> acc ++ " " ++ v) "" args)


-- ****************************************************************************
--             Command Definition and Autocompletion (Top Level)
-- ****************************************************************************
type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ print input

lambda_repl_init :: Repl ()
lambda_repl_init = return ()

top_level_matcher :: MonadIO m => [(String, CompletionFunc m)]
top_level_matcher = [ (":parse"  , listCompleter [])
                 , (":q"      , listCompleter [])
                 , (":exec"   , listCompleter [])
                 , (":compile", listCompleter [])
                 , (":debug:" , listCompleter [])
                 ]

top_level_prefix_completer :: Monad m => WordCompleter m
top_level_prefix_completer n = do
  let names = [ ":parse"
              , ":q"
              , ":exec"
              , ":compile"
              , ":debug"
              ]
  return $ filter (isPrefixOf n) names


top_level_opts :: [(String, [String] -> Repl ())]
top_level_opts = [ ("parse"   , parse)
                 , ("q"       , quit)
                 , ("exec"    , exec)
                 , ("compile" , compile)
                 , ("debug"   , debug)
                 ]

-- ****************************************************************************
--                          Command Implementation (Top Level)
-- ****************************************************************************
parse :: [String] -> Repl ()
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
          putStrLn $ ppShow $ lambda_ast

quit :: [String] -> Repl ()
quit args = abort

exec :: [String] -> Repl ()
exec args = do
  case get_lambda_from_args args of
    Nothing -> liftIO $ putStrLn "Must provided a lambda" >> return ()
    Just the_lambda -> liftIO $ do
      let exec_state = A.execute_lambda_and_show_state the_lambda False
      case exec_state of 
        (Left err_msg) -> do
          putStrLn $ "Lambda execution failed:"
          putStrLn err_msg
        (Right (ast, instrs, exec_state, info)) -> do
          putStrLn $ "********************************************************************************"
          putStrLn "Input Source: "
          putStrLn $ the_lambda ++ "\n"
          putStrLn "Compiled Source: "
          putStrLn $ (ppShow ast) ++ "\n"
          putStrLn $ "Instructions: "
          putStrLn $ (ppShow instrs) ++ "\n"
          putStrLn $ "Execution State: " ++ info
          putStrLn $ (ppShow exec_state) ++ "\n"
          putStrLn $ "********************************************************************************"

compile :: [String] -> Repl ()
compile args = do
  case get_lambda_from_args args of
    Nothing -> liftIO $ putStrLn "Must provide a lambda." >> return ()
    Just the_lambda -> liftIO $ do
      let maybe_src = A.parse_and_compile the_lambda
      case maybe_src of
        Left err -> putStrLn err
        Right code -> do
          putStrLn $ "********************************************************************************"
          putStrLn "Input Source: "
          putStrLn $ the_lambda ++ "\n"
          putStrLn $ "Instructions: "
          putStrLn $ (ppShow code) ++ "\n"
          putStrLn $ "********************************************************************************"

debug :: [String] -> Repl ()
debug args = liftIO $ do
  debug_repl args

-- ****************************************************************************
--             Command Definition and Autocompletion (Debug Mode)
-- ****************************************************************************
type ReplState a = HaskelineT (StateT StateType IO) a
type Debug_Return = ReplState ()

debug_repl_init :: ReplState ()
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

debug_opts :: [(String, [String] -> Debug_Return)]
debug_opts = [ ("next"    , next)
             , ("continue", continue)
             , ("q"       , quit_debug)
             , ("list"    , list)
             ]

-- ****************************************************************************
--                      Command Implementation (Debug Mode)
-- ****************************************************************************
debug_cmd :: String -> ReplState ()
debug_cmd args = undefined

next :: [String] -> ReplState ()
next args = do
  current_state <- get
  case A.exec_code_dbg current_state of
    Left (err_msg, err_state) -> do
      liftIO $ putStrLn err_msg
      abort
    Right new_state -> do
      modify $ \old -> new_state
      liftIO $ putStrLn $ ppShow new_state

list :: [String] -> ReplState ()
list args = do
  current_state <- get
  liftIO $ putStrLn $ ppShow current_state

continue :: [String] -> Debug_Return 
continue args = do
  current_state <- get
  case exec_to_completion current_state of  
    Left (err_msg, err_state) -> liftIO $ do 
      putStrLn err_msg
      putStrLn $ ppShow err_state
    Right success_state -> do
      liftIO $ putStrLn $ ppShow success_state
  abort

quit_debug :: [String] -> Debug_Return 
quit_debug args = abort

-- ****************************************************************************
--                     REPL Initialization and Creation
-- ****************************************************************************
debug_repl :: [String] -> IO ()
debug_repl init_state = do
  case get_lambda_from_args init_state of
    Nothing -> do
      putStrLn "Must provide a lambda."
      return ()
    Just the_lambda -> do
      case A.parse_and_compile the_lambda of
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
lambda_repl = evalRepl prompt cmd top_level_opts command_prefix prefix_completer initial_state
  where
    prompt = pure " \x03BB> "
    command_prefix = Just ':'
    prefix_completer = Prefix (wordCompleter top_level_prefix_completer) top_level_matcher
    initial_state = lambda_repl_init

main :: IO ()
main = lambda_repl 


















