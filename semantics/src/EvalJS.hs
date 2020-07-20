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
