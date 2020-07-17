module EvalJS
  ( eval
  , current
  ) where

import           Data.List       as List hiding (insert)
import           Data.Map.Strict as Map hiding (take)
import           Syntax          hiding (Env)
import Lib
import API

resultValue :: Result -> Val
resultValue RNothing      = VConst (CInt (-1))
resultValue (RReturn val) = val
resultValue (RBreak _)    = error "got a break"

vlookup st env x = (store st) ! (env ! x)

evalOp2 :: Op2 -> Val -> Val -> Val
evalOp2 Add (VConst (CInt m)) (VConst (CInt n)) = VConst (CInt (m + n))
evalOp2 Sub (VConst (CInt m)) (VConst (CInt n)) = VConst (CInt (m - n))
evalOp2 Eq (VConst c1) (VConst c2) = VConst (CBool (c1 == c2))
evalOp2 OGT (VConst (CInt m)) (VConst (CInt n)) = VConst (CBool (m > n))
evalOp2 _ _ _ = error "invalid args to evalOp2"

evalExpr :: State -> Env -> Expr -> Val
evalExpr _ _ (EConst c) = VConst c
evalExpr st env (EId x) = vlookup st env x
evalExpr st env (EOp2 op e1 e2) =
  evalOp2 op (evalExpr st env e1) (evalExpr st env e2)

evalBlock :: State -> Env -> [Stmt] -> (Result, State)
evalBlock st env [] = (RNothing, st)
evalBlock st env (s:ss) =
  case evalStmt st env s of
    (RBreak l, st', _)    -> (RBreak l, st')
    (RReturn v, st', _)   -> (RReturn v, st')
    (RNothing, st', env') -> evalBlock st' env' ss

withEnv env (x, y) = (x, y, env)

evalStmt :: State -> Env -> Stmt -> (Result, State, Env)
evalStmt st env (SLet x (BExpr e)) = (RNothing, st', env')
  where
    (st', addr) = alloc st (evalExpr st env e)
    env' = insert x addr env
evalStmt st env (SLet f (BFunc args body)) = (RNothing, st', env')
  where
    (st', addr) = alloc st (VClosure env args body)
    env' = insert f addr env
evalStmt st env (SLet r (BApp f args)) = (RNothing, st''', insert r addr env)
  where
    vargs = List.map (evalExpr st env) args
    (VClosure fenv fargs fbody) = vlookup st env f
    (st', env') = (extendEnv st fenv fargs vargs)
    (result, st'') = evalBlock st' env' fbody
    rValue = resultValue result
    (st''', addr) = alloc st'' rValue
--evalStmt st env (SLet r (BEvent EvListen args)) = 0
--  where
--    vargs = List.map (evalExpr st env) args
evalStmt st env (SSet (LVId x) (BExpr expr)) = (RNothing, st', env)
  where
    v = evalExpr st env expr
    addr = (env ! x)
    st' = updateStore st addr v
evalStmt st env (SSeq block) = (result, st', env)
  where
    (result, st') = evalBlock st env block
evalStmt st env (SIf test truePart falsePart) =
  case evalExpr st env test of
    (VConst (CBool True))  -> evalStmt st env truePart
    (VConst (CBool False)) -> evalStmt st env falsePart
evalStmt st env (SWhile test body) =
  case evalExpr st env test of
    (VConst (CBool True)) ->
      case evalStmt st env body of
        (RNothing, st', _) -> evalStmt st' env (SWhile test body)
        other              -> other
    (VConst (CBool False)) -> (RNothing, st, env)
evalStmt st env (SLabel l body) =
  case evalStmt st env body of
    (RBreak l', st', env') ->
      if l == l'
        then (RNothing, st', env')
        else (RBreak l', st', env')
    other -> other
evalStmt st env (SBreak l) = (RBreak l, st, env)
evalStmt st env (SReturn e) = (RReturn (evalExpr st env e), st, env)
evalStmt st env (SMeta m) = (RNothing, st', env)
  where
    st' = evalMeta st env m

evalTraceBinding :: [Trace] -> TraceBinding -> (Trace, [Trace])
evalTraceBinding args (TBTrace t)    = (t, args)
evalTraceBinding (a:args) (TBPopArg) = (a, args)

updateMetaState (tr, traceContext) st = st {current = tr, traceContext = traceContext}

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

eval :: Stmt -> State
eval s = st
  where
    (_, st, _) =
      evalStmt
        (State
           { nextAddr = 0
           , store = Map.empty
           , current = TUnknown
           , traceContext = KEmpty
           , argsStack = []
           })
        Map.empty
        s
