
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans.Class (lift)

import qualified Instruction    as I
import qualified StackItem      as S

data StateType = StateType
    { _code     :: [ I.SECDInstruction ]
    , _env      :: [ S.StackItem ]
    , _stack    :: [ S.StackItem ]
    } 
    deriving (Show, Eq)

makeLenses ''StateType

type StackState = ExceptT String (StateT StateType Identity)

pushStackItem :: S.StackItem -> StackState ()
pushStackItem item = lift $ StateT $ \s -> 
    let new = (over stack (\st -> item:st)) s
    in return ((), new)

popStackItem :: StackState S.StackItem
popStackItem = lift $ StateT $ \s ->
    let v = head $ s^.stack
        new = (over stack tail) s
    in return (v, new)

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

pushToCode :: [I.SECDInstruction] -> StackState ()
pushToCode iList = lift $ StateT $ \s ->
    let new = (over code (++iList)) s
    in return ((), new)

stepMachine :: I.SECDInstruction -> StackState ()
stepMachine (I.Const i) = pushStackItem (S.IVal i)
stepMachine (I.True) = pushStackItem (S.BVal True)
stepMachine (I.False) = pushStackItem (S.BVal False)
stepMachine (I.Add) = do
    s1 <- popStackItem
    s2 <- popStackItem
    result <- addItems s1 s2 
    pushStackItem result

stepMachine (I.App) = do
    closure <- popStackItem
    case closure of
        S.Closure iList env -> do
            arg <- popStackItem
            pushToEnv arg
            pushToCode iList
        otherwise -> throwE $ "Expected a closure and got: " ++ (show closure)

stepMachine (I.Access offset) = (lookupEnv offset) >>= pushStackItem
stepMachine (I.Ret) = popEnv

stepMachine (I.Closure iList) = pushStackItem $ S.Closure iList []

addItems :: S.StackItem -> S.StackItem -> StackState S.StackItem
addItems (S.IVal i) (S.IVal j) = return (S.IVal (i + j))
addItems i j = throwE $ 
    "Attempted to add non integer arguments: " 
    ++ (show i) 
    ++ " and " 
    ++ (show j)
    
execute :: StackState ()
execute = do
    nextI <- getNextInstruction
    stepMachine nextI
    execute

execCode code = (runIdentity . stateFun . runExceptT) execute
    where
        stateFun = (flip runStateT) (StateType code [] [])

main :: IO ()
main = do
    let ts = [ I.Const 2
             , I.Const 2
             , I.Add
             , I.Const 3
             , I.Add
             ]
        ts1 = [ I.Const 2
              , I.Closure [ I.Const 1
                          , I.Access 0
                          , I.Add
                          , I.Ret
                          ]
              , I.App
              ]
        res = execCode ts1
    putStrLn $ show res







