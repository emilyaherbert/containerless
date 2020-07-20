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

updateMetaState (tr, traceContext) st = st {current = tr, traceContext = traceContext}

evalTraceBinding :: [Trace] -> TraceBinding -> (Trace, [Trace])
evalTraceBinding args (TBTrace t)    = (t, args)
evalTraceBinding (a:args) (TBPopArg) = (a, args)

evalMeta :: State -> Env -> Meta -> State
evalMeta st env (MLet x trb) =
  st {current = current', traceContext = traceContext', argsStack = argsStack'}
  where
    (tr, argsStack') = evalTraceBinding (argsStack st) trb
    (current', traceContext') = traceLet (current st) (traceContext st) x tr
evalMeta st env (MSet lval trb) =
  st {current = current', traceContext = traceContext', argsStack = argsStack'}
  where
    (tr, argsStack') = evalTraceBinding (argsStack st) trb
    (current', traceContext') = traceSet (current st) (traceContext st) lval tr
evalMeta st env (MNamed x) =
  updateMetaState (traceNamed (current st) (traceContext st) x) st
evalMeta st env (MIfTrue cond) =
  updateMetaState (traceIfTrue (current st) (traceContext st) cond) st
  where

evalMeta st env (MIfFalse cond) =
  updateMetaState (traceIfFalse (current st) (traceContext st) cond) st
  where

evalMeta st env (MWhile cond) =
  updateMetaState (traceWhile (current st) (traceContext st) cond) st
  where

evalMeta st env (MLabel l) =
  updateMetaState (traceLabel (current st) (traceContext st) l) st
evalMeta st env (MBreak l tr) =
  updateMetaState (traceBreak (current st) (traceContext st) l tr) st
  where

evalMeta st env (MEnterSeq n) =
  updateMetaState (traceSeq (current st) (traceContext st) n) st
evalMeta st env MSeqNext =
  updateMetaState (traceSeqNext (current st) (traceContext st)) st
evalMeta st env MPop = updateMetaState (tracePop (current st) (traceContext st)) st
evalMeta st env (MPopTo l) =
  updateMetaState (tracePopTo (current st) (traceContext st) l) st
evalMeta st env (MPushArg arg) = st {argsStack = arg : prev}
  where
    prev = (argsStack st)

traceLet :: Trace -> TraceContext -> Id -> Trace -> (Trace, TraceContext)
traceLet TUnknown k x e = (TLet x e, k)
traceLet c k _ _        = (c, k) -- assumes correct

traceSet :: Trace -> TraceContext -> Trace -> Trace -> (Trace, TraceContext)
traceSet TUnknown k lval e = (TSet lval e, k)
traceSet c k _ _           = (c, k)

traceNamed :: Trace -> TraceContext -> Id -> (Trace, TraceContext)
traceNamed TUnknown k x   = (TUnknown, KNamed x k)
traceNamed (TLet x e) k _ = (e, KNamed x k)

traceIfTrue :: Trace -> TraceContext -> Trace -> (Trace, TraceContext)
traceIfTrue TUnknown k cond    = (TUnknown, KIfTrue cond TUnknown k)
traceIfTrue (TIf cond t f) k _ = (t, KIfTrue cond f k)

traceIfFalse :: Trace -> TraceContext -> Trace -> (Trace, TraceContext)
traceIfFalse TUnknown k cond    = (TUnknown, KIfFalse cond TUnknown k)
traceIfFalse (TIf cond t f) k _ = (f, KIfFalse cond t k)

traceWhile :: Trace -> TraceContext -> Trace -> (Trace, TraceContext)
traceWhile TUnknown k cond        = (TUnknown, KWhile cond k)
traceWhile (TWhile cond body) k _ = (body, KWhile cond k)

traceLabel :: Trace -> TraceContext -> Label -> (Trace, TraceContext)
traceLabel TUnknown k l        = (TUnknown, KLabel l k)
traceLabel (TLabel l body) k _ = (body, KLabel l k)
traceLabel other k l           = error $ show k

traceBreak :: Trace -> TraceContext -> Label -> Trace -> (Trace, TraceContext)
traceBreak c k l tr = (TBreak l tr, k)

traceSeq :: Trace -> TraceContext -> Int -> (Trace, TraceContext)
traceSeq TUnknown k n = (TUnknown, KSeq [] (take (n - 1) (repeat TUnknown)) k)
traceSeq (TSeq (t:ts)) k _ = (t, KSeq [] ts k)

traceSeqNext :: Trace -> TraceContext -> (Trace, TraceContext)
traceSeqNext c (KSeq done (next:remaining) k) =
  (next, KSeq (done ++ [c]) remaining k)
traceSeqNext _ k = error $ "traceSeqNext received k = " ++ show k

tracePop :: Trace -> TraceContext -> (Trace, TraceContext)
tracePop c (KSeq ss1 ss2 k)           = (TSeq (ss1 ++ [c] ++ ss2), k)
tracePop c (KIfTrue test falsePart k) = (TIf test c falsePart, k)
tracePop c (KIfFalse test truePart k) = (TIf test truePart c, k)
tracePop c (KWhile test k)            = (TWhile test c, k)
tracePop c (KLabel l k)               = (TLabel l c, k)
tracePop c (KNamed x k)               = (TLet x c, k)

tracePopTo :: Trace -> TraceContext -> Label -> (Trace, TraceContext)
tracePopTo c (KLabel l1 k) l2 =
  if (l1 == l2)
    then (TLabel l1 c, k)
    else tracePopTo c' k' l2
  where
    (c', k') = tracePop c (KLabel l1 k)
tracePopTo c k l = tracePopTo c' k' l
  where
    (c', k') = tracePop c k