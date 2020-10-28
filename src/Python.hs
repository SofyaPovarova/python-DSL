{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Python
    ( BinOp(..)
    , Expr(..)
    , Lang(..)
    , UnOp(..)

    , Body
    , FreeLang
    , LangF(..)

    , assign'
    , def'
    , expr'
    , if'
    , return'
    , while'
    ) where

import Value

import Control.Monad.Free ( Free, MonadFree, liftF )
import Control.Monad.Free.TH ( makeFree )
import Data.List (intercalate)

data UnOp
  = Neg
  | Not
  deriving Eq

instance Show UnOp where
  show Neg = "-"
  show Not = "not"

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | Neq
  | GEt
  | Gt
  | LEt
  | Lt
  deriving Eq

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show And = "and"
  show Or  = "or"
  show Eq  = "=="
  show Neq = "!="
  show GEt = ">="
  show Gt  = ">"
  show LEt = "<="
  show Lt  = "<"

data Expr
  = Val Value
  | Var Name
  | Un UnOp Expr
  | Bin BinOp Expr Expr
  | Call Name [Expr]
  deriving Eq

data Lang
  = Expr Expr
  | Assign Name Expr
  | If Expr [Lang]
  | While Expr [Lang]
  | Def Name [Name]
  | Return Expr
  deriving Eq

data LangF next
  = Expr' Expr next
  | Assign' Name Expr next
  | If' Expr Body next
  | While' Expr Body next
  | Def' Name [Name] Body next
  | Return' Expr next
  deriving Functor

type FreeLang = Free LangF
type Body = FreeLang ()

makeFree ''LangF

instance Show Expr where
  show (Var name)           = name
  show (Val val)            = show val
  show (Un op expr)         = unwords [show op, show expr]
  show (Bin op expr1 expr2) = unwords [show expr1, show op, show expr2]
  show (Call name args)     = name <> "("
                              <> (intercalate ", " $ map show args) <> ")"
