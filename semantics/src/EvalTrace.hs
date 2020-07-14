module EvalTrace
  ( evalTrace
  , TValue(..)
  ) where

import           Control.Monad.Except
import qualified Control.Monad.State.Strict as State
import           Data.List                  as List hiding (insert)
import           Data.Map.Strict            as Map hiding (take)
import           Syntax

-- Addresses in the store
type Addr = Int

-- Trace values. Note that 'TVEnv' maps names to addresses on the
data TValue
  = TVConst Const
  | TVEnv (Map Id Addr)
  deriving (Show)

-- The store, environment, and next free address. Note that the environment
-- is not an environment in the traditional sense, since we have let statements.
-- Therefore, we cannot use a reader monad.
data Config =
  Config
    { env      :: Map Id Addr
    , store    :: Map Addr TValue
    , nextAddr :: Addr
    }

-- The exception monad transformer handles labels and breaks. The state monad
-- stores the configuration.
type Eval a = ExceptT (Label, TValue) (State.State Config) a

-- 'handleBreak l m' produces 'Left v' if 'm' returns normally with value 'v',
-- 'Right v' if 'm' runs 'break l v', and otherwise propagates the break.
handleBreak :: String -> Eval TValue -> Eval (Either TValue TValue)
handleBreak l m =
  catchError (fmap Left m) $ \(l', v) ->
    if l == l'
      then return (Right v)
      else throwError (l', v)

-- Runs 'break l a';
returnBreak :: String -> TValue -> Eval TValue
returnBreak l v = throwError (l, v)

alloc :: TValue -> Eval Addr
alloc v = do
  config@(Config {store, nextAddr}) <- lift State.get
  lift $
    State.put $
    config {store = Map.insert nextAddr v store, nextAddr = nextAddr + 1}
  return nextAddr

derefAddr :: Addr -> Eval TValue
derefAddr addr = do
  (Config {store}) <- lift State.get
  return (store ! addr)

setAddr :: Addr -> TValue -> Eval ()
setAddr addr v = do
  config@(Config {store}) <- lift State.get
  lift $ State.put $ config {store = Map.insert addr v store}

addressOfVar :: Id -> Eval Addr
addressOfVar x = do
  (Config {env}) <- lift State.get
  return (env ! x)

get :: Id -> Eval TValue
get x = do
  (Config {env, store}) <- lift State.get
  return (store ! (env ! x))

set :: Id -> TValue -> Eval ()
set x v = do
  config@(Config {env, store}) <- lift State.get
  lift $ State.put $ config {store = Map.insert (env ! x) v store}

let_ :: Id -> Addr -> Eval ()
let_ x addr = do
  config@(Config {env}) <- lift State.get
  lift $ State.put $ config {env = Map.insert x addr env}

block :: Eval m -> Eval m
block m = do
  Config {env} <- lift State.get
  r <- m
  config <- lift State.get
  lift $ State.put $ config {env = env}
  return r

evalOp :: Op2 -> Const -> Const -> Const
evalOp Add (CInt m) (CInt n) = CInt (m + n)
evalOp Sub (CInt m) (CInt n) = CInt (m - n)
evalOp Eq c1 c2              = CBool (c1 == c2)
evalOp OGT c1 c2             = CBool (c1 > c2)
evalOp _ _ _                 = error "invalid arguments to evalOp"

evalLValM :: Trace -> Eval Addr
evalLValM (TId x) = addressOfVar x
evalLValM (TFrom t x) = do
  tv <- evalTraceM t
  case tv of
    TVEnv env -> return (env ! x)
    _         -> error "expected environment value"

evalTraceM :: Trace -> Eval TValue
evalTraceM (TConst c) = return (TVConst c)
evalTraceM (TId x) = get x
evalTraceM (TFrom t x) = do
  v <- evalTraceM t
  case v of
    TVEnv env -> derefAddr (env ! x)
    _         -> error "expected environment value"
evalTraceM (TClos env) = do
  let (names, lvals) = unzip (Map.toList env)
  addrs <- mapM evalLValM lvals
  return (TVEnv (Map.fromList $ zip names addrs))
evalTraceM (TOp2 op2 t1 t2) = do
  tv1 <- evalTraceM t1
  tv2 <- evalTraceM t2
  case (tv1, tv2) of
    (TVConst c1, TVConst c2) -> return (TVConst (evalOp op2 c1 c2))
    _                        -> error "binary operator expects constants"
evalTraceM (TSeq ts) = do
  tvs <- block (mapM evalTraceM ts)
  return (List.last tvs)
evalTraceM (TIf t1 t2 t3) = do
  tv1 <- evalTraceM t1
  case tv1 of
    TVConst (CBool True)  -> evalTraceM t2
    TVConst (CBool False) -> evalTraceM t3
    _                     -> error "expected boolean"
evalTraceM (TWhile t1 t2) = do
  tv1 <- evalTraceM t1
  case tv1 of
    TVConst (CBool True) -> do
      evalTraceM t2
      evalTraceM (TWhile t1 t2)
    TVConst (CBool False) -> return (TVConst (CInt 0)) -- arbitrary value
    _ -> error "expected boolean"
evalTraceM (TLabel l s) = do
  result <- handleBreak l (evalTraceM s)
  case result of
    Left v  -> return v
    Right v -> return v
evalTraceM (TBreak l t) = do
  tv <- evalTraceM t
  returnBreak l tv
evalTraceM (TLet x t) = do
  tv <- evalTraceM t
  addr <- alloc tv
  let_ x addr
  return (TVConst (CInt 0)) -- arbitrary value
evalTraceM (TSet t1 t2) = do
  addr <- evalLValM t1
  tv <- evalTraceM t2
  setAddr addr tv
  return (TVConst (CInt 0)) -- arbitrary value
evalTraceM TUnknown = error "evaluated an unknown"

emptyConfig =
  Config
    { env = Map.fromList [("output", 0)]
    , store = Map.fromList [(0, TVConst (CInt 0))]
    , nextAddr = 1
    }

evalTrace :: Trace -> TValue
evalTrace t = (store config) ! 0
  where
    config = State.execState (runExceptT (evalTraceM t)) emptyConfig
