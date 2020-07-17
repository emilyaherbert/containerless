module Lib (Val(..), Env, Addr, Result(..), State(..), alloc, updateStore, extendEnv) where

import           Data.Map.Strict as Map hiding (take)
import           Syntax          hiding (Env)

data Val
  = VConst Const
  | VClosure Env [Id] Block
  | VTrace Trace
  deriving (Show)

type Env = Map Id Addr

type Addr = Int

data Result
  = RBreak Label
  | RReturn Val
  | RNothing

data State =
  State
    { nextAddr      :: Addr
    , store         :: Map Addr Val
    , current       :: Trace
    , traceContext  :: TraceContext
    , argsStack     :: [Trace]
    }
  deriving (Show)

alloc :: State -> Val -> (State, Addr)
alloc (State { nextAddr = nextAddr
             , store = store
             , current = current
             , traceContext = traceContext
             , argsStack = argsStack
             }) v =
  (State
      { nextAddr = nextAddr + 1
      , store = insert nextAddr v store
      , current = current
      , traceContext = traceContext
      , argsStack = argsStack
      }
  , nextAddr)

updateStore :: State -> Addr -> Val -> State
updateStore (State { nextAddr = nextAddr
                   , store = store
                   , current = current
                   , traceContext = traceContext
                   , argsStack = argsStack
                   }) addr v =
  (State
     { nextAddr = nextAddr
     , store = insert addr v store
     , current = current
     , traceContext = traceContext
     , argsStack = argsStack
     })

extendEnv :: State -> Env -> [Id] -> [Val] -> (State, Env)
extendEnv st env [] [] = (st, env)
extendEnv st env (x:xs) (v:vs) = extendEnv st' (insert x addr env) xs vs
  where
    (st', addr) = alloc st v