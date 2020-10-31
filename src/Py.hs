{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Py
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

    , bodyToLangs
    , langToBody
    ) where

import PyValue

import Control.Monad.Free (Free(..), MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
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
  | Def Name [Name] [Lang]
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

langToBody :: Lang -> Body
langToBody lang = case lang of
  Expr e -> expr' e
  Assign n e ->  assign' n e
  If e l -> if' e $ sequence_ $ fmap langToBody l
  While e l -> while' e $ sequence_ $ fmap langToBody l
  Def n ns l -> def' n ns $ sequence_ $ fmap langToBody l
  Return e -> return' e

bodyToLangs :: Body -> [Lang]
bodyToLangs body = reverse $ go body []
  where
    go :: Body -> [Lang] -> [Lang]
    go b acc = case b of
      Pure _ -> acc
      Free (Expr' expr next) -> go next $ Expr expr : acc -- ++ [Expr expr]
      Free (Assign' name expr next) -> go next $ Assign name expr : acc -- ++ [Assign name expr]
      Free (If' expr nextBody next) -> go next $ If expr (bodyToLangs nextBody) : acc -- ++ [If expr $ bodyToLangs nextBody]
      Free (While' expr nextBody next) -> go next $ While expr (bodyToLangs nextBody) : acc -- ++ [While expr $ bodyToLangs nextBody]
      Free (Def' name params nextBody next) -> go next $ Def name params (bodyToLangs nextBody) : acc -- ++ [Def name params $ bodyToLangs nextBody]
      Free (Return' expr next) -> go next $ Return expr : acc -- ++ [Return expr]

instance Show Expr where
  show (Var name)           = name
  show (Val val)            = show val
  show (Un op expr)         = unwords [show op, show expr]
  show (Bin op expr1 expr2) = unwords [show expr1, show op, show expr2]
  show (Call name args)     = name ++ "(" ++
                              (intercalate ", " $ map show args) ++ ")"
