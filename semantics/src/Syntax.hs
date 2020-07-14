module Syntax
  ( Id
  , Label
  , Const(..)
  , Op2(..)
  , Expr(..)
  , Stmt(..)
  , Trace(..)
  , TraceContext(..)
  , Meta(..)
  , LVal(..)
  , Env
  , Binding(..)
  , Block
  , TraceBinding(..)
  ) where

import           Data.Map.Strict as Map hiding (map)

type Id = String

-- Labels for break expressions
type Label = String

data Const
  = CInt Int
  | CBool Bool
  | CUndefined
  deriving (Show, Eq, Ord)

data Op2
  = Add
  | Sub
  | Eq
  | OGT
  deriving (Show)

data Expr
  = EConst Const
  | EId Id
  | EOp2 Op2 Expr Expr
  deriving (Show)

data Binding
  = BExpr Expr -- let x = e1;
  | BFunc [Id] Block -- let f = function (x1 ... xn) stmt;
  | BApp Id [Expr] -- let r = f(e1 ... en);
  deriving (Show)

data TraceBinding
  = BTrace Trace
  | BPopArg
  deriving (Show)

type Block = [Stmt]

data Stmt
  = SLet Id Binding
  | SSet LVal Binding
  | SSeq Block
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SLabel Label Stmt
  | SBreak Label
  | SReturn Expr
  | SMeta Meta
  deriving (Show)

data Meta
  = MPop
  | MPopTo Label
  | MPushArg Trace
  | MEnterSeq Int -- 'TraceSeq n': Entering a block with 'n' statements
  | MSeqNext -- Moving on to next statement in a block
  | MLet Id TraceBinding -- Set the current statement to 'let x = t';
  | MSet Trace TraceBinding -- Set the current statement to [[t1 = t2]]
  | MNamed Id -- Enter the named expression of a let statement.
  | MIfTrue Trace
  | MIfFalse Trace
  | MWhile Trace
  | MLabel Label
  | MBreak Label Trace
  deriving (Show)

data TraceContext
  = KSeq [Trace] [Trace] TraceContext
  | KIfTrue Trace Trace TraceContext
  | KIfFalse Trace Trace TraceContext
  | KWhile Trace TraceContext
  | KLabel Label TraceContext
  | KNamed Id TraceContext
  | KEmpty
  deriving (Show)

-- Note: This model has just one kind of l-value. If we extend the model with
-- objects, that would introduce a second kind of l-value.
data LVal =
  LVId Id
  deriving (Show)

type Env = Map Id Trace

data Trace
  = TConst Const
  | TId Id
  | TFrom Trace Id
  | TClos Env
  | TOp2 Op2 Trace Trace
  | TSeq [Trace]
  | TIf Trace Trace Trace
  | TWhile Trace Trace
  | TLabel Label Trace
  | TBreak Label Trace
  | TLet Id Trace
  | TSet Trace Trace
  | TUnknown
  deriving (Show)
