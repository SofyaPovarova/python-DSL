{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Python
    ( BinOp(..)
    , Expr(..)
    , LangF(..)
  
    , Body
    , Lang

    , assign
    , func
    , if'
    , input
    , print'
    , return'
    , while
    ) where

import Value

import Control.Monad.Free
import Control.Monad.Free.TH

data BinOp =
    Add
  | Mul
  deriving (Eq, Ord)

instance Show BinOp where
  show Add = "+"
  show Mul = "*"

data Expr =
    Bin BinOp Expr Expr
  | Call Name [Expr]
  | Val Value
  | Var Name
  deriving (Eq, Ord)

-- TODO add ',' to Call args
instance Show Expr where
  show (Var name)           = name
  show (Val val)            = show val
  show (Bin op expr1 expr2) = unwords [show expr1, show op, show expr2]
  show (Call name args)     = name <> "(" <>
                              (unwords $ map show $ args) <> ")"

data LangF a =
    Assign Name Expr a
  | Func Name [Name] Body a
  | If' Expr Body a
  | Input (String -> a)
  | Print' [Expr] a
  | Return' Expr a
  | While Expr Body a
  deriving Functor

type Lang = Free LangF
type Body = Lang ()

makeFree ''LangF
