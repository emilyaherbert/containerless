module Compiler
  ( compile
  ) where

import           Data.List       as List hiding (insert)
import           Data.List       ((++))
import           Data.Map.Strict as Map hiding (foldl, map)
import           Syntax

data State =
  State
    { env         :: Env
    , nextHandler :: Int
    }
  deriving (Show)

extendEnv :: State -> Id -> Trace -> State
extendEnv (State { env = env, nextHandler = nextHandler }) x t =
  (State
     { env = insert x t env
     , nextHandler = nextHandler
     })

updateEnv :: State -> Env -> State
updateEnv (State { env = env, nextHandler = nextHandler }) env2 =
  (State
     { env = env2
     , nextHandler = nextHandler
     })

compileLVal :: State -> LVal -> Trace
compileLVal state (LVId x) = (env state) ! x

compileExpr :: State -> Expr -> Trace
compileExpr _ (EConst c) = TConst c
compileExpr state (EId x) = (env state) ! x
compileExpr state (EOp2 op e1 e2) =
  TOp2 op (compileExpr state e1) (compileExpr state e2)

compileStmts :: State -> [Stmt] -> ([Stmt], State)
compileStmts state [] = ([], state)
compileStmts state (s:ss) = (s' ++ ss', state'')
  where
    (s', state') = compileStmt state s
    (ss', state'') = compileStmts state' ss

compileSeq :: State -> [Stmt] -> ([Stmt], State)
compileSeq state [] = ([], state)
compileSeq state [s] = compileStmt state s
compileSeq state (s:ss) = (s' ++ (SMeta MSeqNext) : ss', state'')
  where
    (s', state') = compileStmt state s
    (ss', state'') = compileSeq state' ss

compileStmt :: State -> Stmt -> ([Stmt], State)
compileStmt state (SSeq ss) =
  ([SSeq (SMeta (MEnterSeq n) : compiled ++ [SMeta MPop])], state')
  where
    (compiled, state') = compileSeq state ss
    n = List.length ss
compileStmt state (SLet x (BExpr expr)) = ([s1, s2], extendEnv state x (TId x))
  where
    s1 = SMeta $ MLet x $ BTrace $ compileExpr state expr
    s2 = SLet x (BExpr expr)
compileStmt state (SLet f (BFunc params blk)) = ([s1, s2], state''')
  where
    env0 = Map.fromList $ map (\x -> (x, TId x)) params
    -- NOTE: Relies on left-biased union so that params correctly shadow the
    -- enclosing environment.
    env' =
      Map.union env0 $
      Map.fromList $ map (\x -> (x, TFrom (TId "t") x)) (keys (env state))
    tracedParams = map (\x -> SMeta (MLet x BPopArg)) $ "t" : params
    (compiled, state'') = compileStmt (updateEnv state env') $ SSeq $ tracedParams ++ blk
    s1 = SMeta $ MLet f $ BTrace $ TClos (env state)
    s2 =
      SLet f $
      BFunc params $ [SMeta (MLabel "return")] ++ compiled ++ [SMeta MPop]
    state''' = extendEnv state f (TId f)
compileStmt state (SLet x (BApp f args)) =
  (pArgs ++ [s1, s2, SMeta MPop], extendEnv state x (TId x))
  where
    pArgs =
      reverse $
      SMeta (MPushArg (TId f)) :
      map (\e -> SMeta (MPushArg (compileExpr state e))) args
    s1 = SMeta (MNamed x)
    s2 = SLet x (BApp f args)
{-
compileStmt env (SLet x (BEvent ev args)) =
  (pArgs ++ [s1, s2, SMeta MPop], insert x (TId x) env)
  where
    pArgs =
      reverse $
      SMeta (MPushArg (TId f)) :
      map (\e -> SMeta (MPushArg (compileExpr env e))) args
    s1 = SMeta (MNamed x)
    s2 = SLet x (BEvent ev args)
    -- s3 = SMeta (MSaveHander 0)
-}
compileStmt state (SSet lval (BExpr expr)) = ([s1, s2], state)
  where
    s1 = SMeta $ MSet (compileLVal state lval) $ BTrace $ compileExpr state expr
    s2 = SSet lval (BExpr expr)
compileStmt state (SIf c t f) = ([s1, SMeta MPop], state)
  where
    (t', _) = compileStmt state t
    (f', _) = compileStmt state f
    c' = compileExpr state c
    t'' = SSeq (SMeta (MIfTrue c') : t')
    f'' = SSeq (SMeta (MIfFalse c') : f')
    s1 = SIf c t'' f''
compileStmt state (SWhile expr body) = ([s1, s2, SMeta MPop], state)
  where
    (body', _) = compileStmt state body
    s1 = SMeta (MWhile (compileExpr state expr))
    s2 = SWhile expr (SSeq body')
compileStmt state (SLabel l body) = ([s1], state')
  where
    (compiled, state') = compileStmt state body
    body' = SSeq ((SMeta (MLabel l)) : compiled ++ [SMeta MPop])
    s1 = SLabel l body'
compileStmt state (SBreak l) =
  ([SMeta (MBreak l (TConst CUndefined)), SMeta (MPopTo l), SBreak l], state)
compileStmt state (SReturn e) = ([tBreak, popTo, SReturn e], state)
  where
    tBreak = SMeta $ MBreak "return" $ compileExpr state e
    popTo = SMeta $ MPopTo "return"
compileStmt state (SMeta m) = ([SMeta m], state)
compileStmt _ other = error $ show other

compile :: Stmt -> Stmt
compile s = SSeq ss
  where
    state = State { env = Map.fromList [("output", TId "output")], nextHandler = 0 }
    (ss, _) = compileStmt state s
