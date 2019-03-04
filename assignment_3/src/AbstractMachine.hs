
{-# LANGUAGE TemplateHaskell #-}

module AbstractMachine where

import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans.Class (lift)
import Text.Show.Pretty

import qualified Instruction    as I
import qualified StackItem      as S
import qualified LambdaParser   as L
import qualified SymbolTable    as ST

data StateType = StateType
    { _code             :: [ I.SECDInstruction ]
    , _env              :: [ S.StackItem ]
    , _stack            :: [ S.StackItem ]
    , _dbg_history      :: [ StateType ]
    } 
    deriving (Show, Eq)

makeLenses ''StateType

type StackState = ExceptT String (StateT StateType Identity)

pushStackItem :: S.StackItem -> StackState ()
pushStackItem item = lift $ StateT $ \s -> 
    let new = (over stack (\st -> item:st)) s
    in return ((), new)

popStackItem' :: StackState ()
popStackItem' = lift $ StateT $ \s ->
    let v = head $ s^.stack
        new = (over stack tail) s
    in return ((), new)

add_to_debug_history :: StackState ()
add_to_debug_history = lift $ StateT $ \s ->
  let new = set dbg_history (with_empty_history:(s^.dbg_history)) s
      with_empty_history = (set dbg_history [] s)
  in return ((), new)

popStackItem :: StackState S.StackItem
popStackItem = do
    s <- getStack
    if null s
        then throwE "Attempt to pop empty stack"
        else popStackItem' >> (return $ head s)

getStack :: StackState [S.StackItem]
getStack = lift $ StateT $ \s ->
    let v = s^.stack
    in return (v, s)

getNextInstruction :: StackState I.SECDInstruction
getNextInstruction = do
    iList <- getInstructionList
    if null iList
        then throwE "Finished Execution"
        else do
            popIList
            return $ head iList

getInstructionList :: StackState [I.SECDInstruction]
getInstructionList = lift $ StateT $ \s ->
    let iList = s^.code
    in return (iList, s)

popIList :: StackState ()
popIList = lift $ StateT $ \s ->
    let new = (over code tail) s
        in return ((), new)

lookupEnv :: Int -> StackState S.StackItem
lookupEnv i = lift $ StateT $ \s ->
    let eVal = (s^.env) !! i
    in return (eVal, s)

pushToEnv :: S.StackItem -> StackState ()
pushToEnv e = lift $ StateT $ \s ->
    let new = (over env (e:)) s
    in return ((), new)

popEnv :: StackState ()
popEnv = lift $ StateT $ \s ->
    let new = (over env tail) s
    in return ((), new)

get_env :: StackState [S.StackItem]
get_env = lift $ StateT $ \s -> 
  let current_env = s^.env
  in return (current_env, s)

set_env :: [S.StackItem] -> StackState ()
set_env new_env = lift $ StateT $ \s -> 
  let new_state = set env new_env s
  in return ((), new_state)

get_code :: StackState [I.SECDInstruction]
get_code = lift $ StateT $ \s ->
  let current_code = s^.code
  in return (current_code, s)

set_code :: [I.SECDInstruction] -> StackState ()
set_code new_code = lift $ StateT $ \s -> 
  let new_state = set code new_code s
  in return ((), new_state)

pushToCode :: [I.SECDInstruction] -> StackState ()
pushToCode iList = lift $ StateT $ \s ->
    let new = (over code (iList++)) s
    in return ((), new)

stepMachine :: I.SECDInstruction -> StackState ()
-- Boolean Instructions
stepMachine (I.True) = pushStackItem (S.BVal True)
stepMachine (I.False) = pushStackItem (S.BVal False)
stepMachine (I.IfThenElse _ _) = do
    c <- popStackItem
    ifC <- popStackItem
    elseC <- popStackItem
    case c of
        (S.BVal t) -> do
            if t
                then pushStackItem ifC
                else pushStackItem elseC
        otherwise -> throwE $ "Expected boolean, got: " ++ (show c)

-- Arithmetic Instructions
stepMachine (I.Const i) = pushStackItem (S.IVal i)
stepMachine (I.Add) = do
    s1 <- popStackItem
    s2 <- popStackItem
    result <- arithmeticOp s1 s2 (+)
    pushStackItem result
stepMachine (I.Mul) = do
    s1 <- popStackItem
    s2 <- popStackItem
    result <- arithmeticOp s1 s2 (*)
    pushStackItem result
stepMachine (I.LEq) = do
    s1 <- popStackItem
    s2 <- popStackItem
    result <- intToBooleanOp s1 s2 (<=)
    pushStackItem result

-- Function Calls and Variable Access
stepMachine (I.App) = do
    closure <- popStackItem
    case closure of
        S.Closure iList env -> do
            arg <- popStackItem
            pushToEnv arg
            pushToCode iList
        S.FixClosure instr_list env -> do
          old_code <- get_code
          old_env <- get_env
          let new_closure = S.Closure old_code old_env
          arg <- popStackItem
          set_code instr_list
          set_env $ arg : ((S.FixClosure instr_list env) : env)
          pushStackItem new_closure
        otherwise -> throwE $ "Expected a closure and got: " ++ (show closure)
    
stepMachine (I.Access offset) = do
  if offset > 0
    then throwE $ "Offset was large: " ++ (show offset)
    else (lookupEnv offset) >>= pushStackItem
stepMachine (I.Ret) = popEnv
stepMachine (I.Closure iList) = do
    pushStackItem $ S.Closure iList []

arithmeticOp :: S.StackItem 
             -> S.StackItem 
             -> (Int -> Int -> Int) 
             -> StackState S.StackItem
arithmeticOp (S.IVal i) (S.IVal j) op = return $ S.IVal (j `op` i)
arithmeticOp i j _ = throwE $
    "Attempted to perform arithmetic operation "
    ++ "on non integer arguments: "
    ++ (show i)
    ++ " and "
    ++ (show j)

intToBooleanOp :: S.StackItem
               -> S.StackItem
               -> (Int -> Int -> Bool)
               -> StackState S.StackItem
intToBooleanOp (S.IVal i) (S.IVal j) op = return $ S.BVal (j `op` i)
intToBooleanOp i j _ = throwE $ 
    "Attempted to perform arithmetic operation "
    ++ "on non integer arguments: " ++ (show i)
    ++ " and "
    ++ (show j)
    
execute :: StackState ()
execute = do
    nextI <- getNextInstruction
    stepMachine nextI
    execute

execute_dbg :: StackState ()
execute_dbg = do
  next_instr <- getNextInstruction
  step_machine_dbg next_instr
  execute_dbg

execCode :: [I.SECDInstruction] -> (Either String (), StateType)
execCode code = (runIdentity . stateFun . runExceptT) execute
    where
        stateFun = (flip runStateT) (StateType code [] [] [])

exec_code_dbg :: [I.SECDInstruction] -> (Either String (), StateType)
exec_code_dbg code = (runIdentity . stateFun . runExceptT) execute_dbg
  where
    stateFun = (flip runStateT) (StateType code [] [] [StateType code [] [] []])

type DBState a = ExceptT String (ST.SymbolTableST String String) a

deBruijn :: L.Lambda_Expr -> DBState [I.SECDInstruction]
deBruijn (L.Var v) = do
    dist <- ST.lookupDistance v
    return $ [ I.Access dist ]

deBruijn (L.IntLiteral int_val)   = return $ [I.Const int_val]
deBruijn (L.BoolLiteral bool_val) = return $ [if bool_val then I.True else I.False]

deBruijn (L.Abstraction arg term) = do
    ST.addLevel
    ST.insertSym arg "var"
    body <- deBruijn term
    ST.remLevel
    return $ [I.Closure (body ++ [I.Ret])]

-- @Todo: I think this will only support two-argument recursive functions currently.
deBruijn (L.Fix instr_list) = do  
  case instr_list of
    L.Abstraction func_name code -> do
      ST.addLevel
      ST.insertSym func_name "var"
      case code of
        L.Abstraction arg_name func_code -> do
          ST.addLevel
          ST.insertSym arg_name "var"
          compiled_code <- deBruijn $ func_code
          ST.remLevel
          ST.remLevel
          return [I.Fix $ compiled_code ++ [I.Ret]]
        otherwise -> throwE $ "Expected a closure and got: " ++ (show code)
    otherwise -> throwE $ "Expected a closure and got: " ++ (show instr_list)

deBruijn (L.Application t1 t2) = do
    t1' <- deBruijn t1
    t2' <- deBruijn t2
    return $ t2' ++ t1' ++ [I.App]
deBruijn (L.Add t1 t2) = do
    t1' <- deBruijn t1
    t2' <- deBruijn t2
    return $ t1' ++ t2' ++ [I.Add]
deBruijn (L.Sub t1 t2) = do
  t1' <- deBruijn t1
  t2' <- deBruijn t2
  return $ t1' ++ t2' ++ [I.Sub]
deBruijn (L.Mul t1 t2) = do
    t1' <- deBruijn t1
    t2' <- deBruijn t2
    return $ t1' ++ t2' ++ [I.Mul]
deBruijn (L.Conditional cond ifC elseC) = do
    cond' <- deBruijn cond
    ifC'  <- deBruijn ifC
    elseC' <- deBruijn elseC
    return $ cond' ++ [I.IfThenElse (ifC'++[I.Ret]) (elseC'++[I.Ret])]
deBruijn (L.LessThan t1 t2) = do
    t1' <- deBruijn t1
    t2' <- deBruijn t2
    return $ t1' ++ t2' ++ [I.LEq]
deBruijn (L.Equal t1 t2) = do
  t1' <- deBruijn t1
  t2' <- deBruijn t2
  return $ t1' ++ t2' ++ [I.Eq]

step_machine_dbg :: I.SECDInstruction -> StackState ()
step_machine_dbg (I.True) = pushStackItem (S.BVal True) >> add_to_debug_history
step_machine_dbg (I.False) = pushStackItem (S.BVal False) >> add_to_debug_history

step_machine_dbg (I.IfThenElse then_code else_code) = do
  cond_value <- popStackItem
  case cond_value of
    (S.BVal t) -> do
      let code_to_execute = if t
                              then then_code
                              else else_code
      old_code <- get_code
      old_env <- get_env
      let new_closure = S.Closure old_code old_env
      set_code code_to_execute
      pushStackItem new_closure
    otherwise -> throwE $ "Expected boolean, got: " ++ (show cond_value)
  add_to_debug_history

step_machine_dbg (I.Const i) = pushStackItem (S.IVal i) >> add_to_debug_history
step_machine_dbg (I.Add) = do
  s1 <- popStackItem 
  s2 <- popStackItem
  result <- arithmeticOp s1 s2 (+)
  pushStackItem result
  add_to_debug_history

step_machine_dbg (I.Sub) = do
  s1 <- popStackItem
  s2 <- popStackItem
  result <- arithmeticOp s1 s2 (-)
  pushStackItem result
  add_to_debug_history

step_machine_dbg (I.Mul) = do
  s1 <- popStackItem 
  s2 <- popStackItem
  result <- arithmeticOp s1 s2 (*)
  pushStackItem result
  add_to_debug_history

step_machine_dbg (I.LEq) = do
  s1 <- popStackItem
  s2 <- popStackItem
  result <- intToBooleanOp s1 s2 (<)
  pushStackItem result
  add_to_debug_history

step_machine_dbg (I.Eq) = do
  s1 <- popStackItem
  s2 <- popStackItem
  result <- intToBooleanOp s1 s2 (==)
  pushStackItem result
  add_to_debug_history

-- Preconditions: 
--  (1) Stack shoudl be a closure followed by an argument
-- Postconditions
--  (1) Code pointer should be the code of the closure
--  (2) Environment should be the environment extracted from the closure plus the arg
--  (3) Push a new closure containing the old code pointer and teh old environemnt
--
--  New things
-- aset_code, get_code 
step_machine_dbg (I.App) = do
  closure <- popStackItem
  case closure of
    S.Closure i_list env -> do
      arg <- popStackItem
      old_env <- get_env
      old_code <- get_code
      let new_env = arg : env
          new_closure = S.Closure old_code old_env
      pushStackItem new_closure
      set_code i_list
      set_env new_env
    S.FixClosure instr_list env -> do
      old_code <- get_code
      old_env <- get_env
      let new_closure = S.Closure old_code old_env
      arg <- popStackItem
      set_code instr_list
      set_env $ arg : ((S.FixClosure instr_list env) : env)
      pushStackItem new_closure
    otherwise -> throwE $ "Expected a closure. Found: " ++ (show closure)
  add_to_debug_history

step_machine_dbg (I.Access offset) = (lookupEnv offset) >>= pushStackItem >> add_to_debug_history

-- Preconditions
--  (1) Stack contains a value (the return value followed by a closure.
-- Postconditions
--  (1) Code pointer is set to the code pointer extracted from the closure taken off the stack.
--  (2) Environemnt pointer is set to the environment pointer retrieved from the closure taken off the stack
--  (3) The return value sits ontop of the stack.
step_machine_dbg (I.Ret) = do 
  return_val <- popStackItem
  closure <- popStackItem
  case closure of
    (S.Closure next_code next_env) -> do
      set_code next_code 
      set_env next_env
      pushStackItem return_val
  add_to_debug_history

step_machine_dbg (I.Closure i_list) = do
  current_env <- get_env
  pushStackItem $ S.Closure i_list current_env
  add_to_debug_history

step_machine_dbg (I.Fix instr_list) = do
  current_env <- get_env
  let the_closure = S.FixClosure instr_list current_env
  pushStackItem the_closure
  add_to_debug_history

compile :: L.Lambda_Expr -> Either String [I.SECDInstruction]
compile ast = fst $ (runIdentity . ((flip runStateT) ST.mkInitState) . runExceptT) (deBruijn ast)

-- parseAndExecuteLambda :: String -> Either (String ()) StateType
-- You can't use two functions returning eithers parameterized on a different
-- "Left" type since the function would have multiple return types.
parseAndExecuteLambda source = do
    parseResult <- case L.parse_lambda source of
                        (Left err)   -> Left $ "Failed to parse Lambda.: " ++ (show err)
                        (Right code) -> (Right code)
    compilationResult <- case compile parseResult of
                            (Left err)         -> Left $ "Failed to compile code: " ++ (show err)
                            (Right compResult) -> Right compResult
    return $ execCode compilationResult

show_debug_information :: String -> IO ()
show_debug_information src = do 
  let Right parsed = L.parse_lambda src
      (either_inst) = compile parsed
  putStrLn $ "Parsed Source Code: "
  putStrLn $ ppShow parsed
  putStrLn $ "Compiled Source Code: "
  case either_inst of
    (Left _) -> putStrLn $ "Error: Compilation Failed."
    (Right instrs) -> do
      putStrLn $ ppShow instrs
      putStrLn $ "Execution Result: "
      putStrLn $ ppShow $ exec_code_dbg instrs

execute_lambda_and_show_state 
  :: String 
  -> Bool 
  -> Either String (L.Lambda_Expr, [I.SECDInstruction], StateType, String) 
execute_lambda_and_show_state src enable_dbg = do
  case L.parse_lambda src of
    (Left err_msg) -> Left $ show err_msg
    (Right ast) -> do
      case compile ast of
        ((Left err_msg)) -> Left $ show err_msg
        ((Right instrs)) -> do
          case exec_code_dbg instrs of
            ((Left error), exec_state) -> do
              if enable_dbg
                then return $ (ast, instrs, exec_state, error)
                else return $ (ast, instrs, (set dbg_history [] exec_state), error)
            ((Right ()), exec_state) -> do
              if enable_dbg
                then return $ (ast, instrs, exec_state, "No Errors.")
                else return $ (ast, instrs, (set dbg_history [] exec_state), "No Errors.")

read_lambdas_from_file :: String -> IO [String]
read_lambdas_from_file filePath = do
    lambdaFileContent <- readFile filePath
    let lambdas = filter (\line -> not ('#' `elem` line)) (lines lambdaFileContent)
    return lambdas

enable_dbg :: Bool
enable_dbg = False

execute_tests :: [String] -> IO ()
execute_tests lambdas = do 
  forM_ lambdas (\lambda -> do
    case execute_lambda_and_show_state lambda enable_dbg of
      (Left err_msg) -> putStrLn $ err_msg
      (Right (ast, instrs, exec_state, info)) -> do
        putStrLn $ "********************************************************************************"
        putStrLn "Input Source: "
        putStrLn $ lambda ++ "\n"
        putStrLn "Compiled Source: "
        putStrLn $ (ppShow ast) ++ "\n"
        putStrLn $ "Instructions: "
        putStrLn $ (ppShow instrs) ++ "\n"
        putStrLn $ "Execution State: " ++ info
        putStrLn $ (ppShow exec_state) ++ "\n"
        putStrLn $ "********************************************************************************")

parse_and_compile :: [String] -> IO ()
parse_and_compile lambdas = do
  forM_ lambdas (\lambda_i -> do
    case L.parse_lambda lambda_i of
      Left err -> error err
      Right ast -> do
        case compile ast of
          Left err -> error err
          Right compiled_code ->
            putStrLn $ ppShow compiled_code)

main :: IO ()
main = do
  let tests = [ "(\\x. (x + 2) = 20) if (\\x. false) 1 then 5 else 18"
              , "(\\add. \\x. \\y. add x y)  (\\a. \\b. a + b) 10 20"
              , "(\\add. \\x. \\y. \\z. 10*add x y z) (\\a. \\b. \\c. a + b + c) 3 2 1"
              , "(fix (\\fact. \\n. if n = 1 then 1 else n * (fact (n - 1)))) 5"
              , "(fix (\\fib. \\n. if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2)))) 8"
              ]
  execute_tests tests



















