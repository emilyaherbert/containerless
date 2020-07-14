module Compiler
  ( compile
  ) where

import           Data.List       as List hiding (insert)
import           Data.List       ((++))
import           Data.Map.Strict as Map hiding (foldl, map)
import           Syntax

compileLVal :: Env -> LVal -> Trace
compileLVal env (LVId x) = env ! x

compileExpr :: Env -> Expr -> Trace
compileExpr _ (EConst c) = TConst c
compileExpr env (EId x) = env ! x
compileExpr env (EOp2 op e1 e2) =
  TOp2 op (compileExpr env e1) (compileExpr env e2)

compileStmts :: Env -> [Stmt] -> ([Stmt], Env)
compileStmts env [] = ([], env)
compileStmts env (s:ss) = (s' ++ ss', env'')
  where
    (s', env') = compileStmt env s
    (ss', env'') = compileStmts env' ss

compileSeq :: Env -> [Stmt] -> ([Stmt], Env)
compileSeq env [] = ([], env)
compileSeq env [s] = compileStmt env s
  where

compileSeq env (s:ss) = (s' ++ (SMeta MSeqNext) : ss', env'')
  where
    (s', env') = compileStmt env s
    (ss', env'') = compileSeq env' ss

compileStmt :: Env -> Stmt -> ([Stmt], Env)
compileStmt env (SSeq ss) =
  ([SSeq (SMeta (MEnterSeq n) : compiled ++ [SMeta MPop])], env')
  where
    (compiled, env') = compileSeq env ss
    n = List.length ss
compileStmt env (SLet x (BExpr expr)) = ([s1, s2], insert x (TId x) env)
  where
    s1 = SMeta $ MLet x $ BTrace $ compileExpr env expr
    s2 = SLet x (BExpr expr)
compileStmt env (SLet f (BFunc params blk)) = ([s1, s2], env''')
  where
    env0 = Map.fromList $ map (\x -> (x, TId x)) params
    -- NOTE: Relies on left-biased union so that params correctly shadow the
    -- enclosing environment.
    env' =
      Map.union env0 $
      Map.fromList $ map (\x -> (x, TFrom (TId "t") x)) (keys env)
    tracedParams = map (\x -> SMeta (MLet x BPopArg)) $ "t" : params
    (compiled, env'') = compileStmt env' $ SSeq $ tracedParams ++ blk
    s1 = SMeta $ MLet f $ BTrace $ TClos env
    s2 =
      SLet f $
      BFunc params $ [SMeta (MLabel "return")] ++ compiled ++ [SMeta MPop]
    env''' = insert f (TId f) env''
compileStmt env (SLet x (BApp f args)) =
  (pArgs ++ [s1, s2, SMeta MPop], insert x (TId x) env)
  where
    pArgs =
      reverse $
      SMeta (MPushArg (TId f)) :
      map (\e -> SMeta (MPushArg (compileExpr env e))) args
    s1 = SMeta (MNamed x)
    s2 = SLet x (BApp f args)
compileStmt env (SSet lval (BExpr expr)) = ([s1, s2], env)
  where
    s1 = SMeta $ MSet (compileLVal env lval) $ BTrace $ compileExpr env expr
    s2 = SSet lval (BExpr expr)
compileStmt env (SIf c t f) = ([s1, SMeta MPop], env)
  where
    (t', _) = compileStmt env t
    (f', _) = compileStmt env f
    c' = compileExpr env c
    t'' = SSeq (SMeta (MIfTrue c') : t')
    f'' = SSeq (SMeta (MIfFalse c') : f')
    s1 = SIf c t'' f''
compileStmt env (SWhile expr body) = ([s1, s2, SMeta MPop], env)
  where
    (body', _) = compileStmt env body
    s1 = SMeta (MWhile (compileExpr env expr))
    s2 = SWhile expr (SSeq body')
compileStmt env (SLabel l body) = ([s1], env')
  where
    (compiled, env') = compileStmt env body
    body' = SSeq ((SMeta (MLabel l)) : compiled ++ [SMeta MPop])
    s1 = SLabel l body'
compileStmt env (SBreak l) =
  ([SMeta (MBreak l (TConst CUndefined)), SMeta (MPopTo l), SBreak l], env)
compileStmt env (SReturn e) = ([tBreak, popTo, SReturn e], env)
  where
    tBreak = SMeta $ MBreak "return" $ compileExpr env e
    popTo = SMeta $ MPopTo "return"
compileStmt env (SMeta m) = ([SMeta m], env)
compileStmt _ other = error $ show other

compile :: Stmt -> Stmt
compile s = SSeq ss
  where
    (ss, _) = compileStmt (Map.fromList [("output", TId "output")]) s
