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

type Repl a = HaskelineT IO a

-- ****************************************************************************
--                                  Helpers
-- ****************************************************************************
get_lambda_from_args = tail . (foldl (\acc v -> acc ++ " " ++ v) "")

cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- ****************************************************************************
--             Command Definition and Autocompletion (Top Level)
-- ****************************************************************************
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
parse args = liftIO $ do
  let parsed_lambda = L.parse_lambda the_lambda
      the_lambda = get_lambda_from_args args
  putStrLn "Original Source: "
  putStrLn $ (show the_lambda) ++ "\n"
  case parsed_lambda of 
    Left err -> do
      putStrLn $ "Error: " ++ err
    Right lambda_ast -> do
      putStrLn "Lambda Abstract SyntaxTree: "
      putStrLn $ ppShow $ lambda_ast

quit :: [String] -> Repl ()
quit args = abort

exec :: [String] -> Repl ()
exec args = liftIO $ do
  let the_lambda = get_lambda_from_args args
      exec_state = A.execute_lambda_and_show_state the_lambda False
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
compile args = liftIO $ do
  let the_lambda = get_lambda_from_args args
      maybe_src = A.parse_and_compile the_lambda
  case maybe_src of
    Left err -> putStrLn "Error: " >> putStrLn err
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
debug_matcher :: MonadIO m => [(String, CompletionFunc m)]
debug_matcher = [ (":next"    , listCompleter [])
                , (":continue", listCompleter [])
                , (":q"       , listCompleter [])
                ]

debug_prefix_completer :: Monad m => WordCompleter m
debug_prefix_completer n = do
  let names = [ ":next"
              , ":continue"
              , ":q"
              ]
  return $ filter (isPrefixOf n) names

debug_opts :: [(String, [String] -> Repl ())]
debug_opts = [ ("next"    , next)
             , ("continue", continue)
             , ("q"       , quit_debug)
             ]

-- ****************************************************************************
--                      Command Implementation (Debug Mode)
-- ****************************************************************************
debug_repl_init :: [String] -> Repl ()
debug_repl_init init_state = liftIO $ do
  let the_lambda = get_lambda_from_args init_state
      code  = A.parse_and_compile the_lambda
  case code of
    Left err -> putStrLn "Compilation Failed: " >> putStrLn err
    Right instrs -> do
      let the_init_state = A.mk_state_for_dbg_execution instrs
      putStrLn $ ppShow the_init_state

-- next :: [String] -> Repl A.StateType
next :: [String] -> Repl ()
next = undefined

-- continue :: [String] -> Repl A.StateType
continue :: [String] -> Repl ()
continue = undefined

-- quit_debug :: [String] -> Repl A.StateType
quit_debug :: [String] -> Repl ()
quit_debug args = abort

-- ****************************************************************************
--                     REPL Initialization and Creation
-- ****************************************************************************
lambda_repl_init :: Repl ()
lambda_repl_init = return ()

debug_repl :: [String] -> IO ()
debug_repl init_state = evalRepl prompt cmd debug_opts command_prefix prefix_completer initial_state
  where
    prompt = pure " \x03BB debug> "
    command_prefix = (Just ':')
    prefix_completer = Prefix (wordCompleter debug_prefix_completer) debug_matcher
    initial_state = debug_repl_init init_state

lambda_repl :: IO ()
lambda_repl = evalRepl prompt cmd top_level_opts command_prefix prefix_completer initial_state
  where
    prompt = pure " \x03BB> "
    command_prefix = Just ':'
    prefix_completer = Prefix (wordCompleter top_level_prefix_completer) top_level_matcher
    initial_state = lambda_repl_init

main :: IO ()
main = lambda_repl 


















