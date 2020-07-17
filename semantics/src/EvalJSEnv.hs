module EvalJSEnv (Val(..), Env, Addr, Result(..), State(..)) where

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