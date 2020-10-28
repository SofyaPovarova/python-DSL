module Main where

import Value
import PInterpreter
import Python

-- fact: FreeLang() of 'examples/fact.py'
fact :: FreeLang ()
fact = do
  def' "fact" ["x"] $ do
    assign' "cur" $ Val $ VInt 1
    assign' "res" $ Var "cur"
    while' (Bin LEt (Var "cur") (Var "x")) $ do
      assign' "res" $ Bin Mul (Var "res") (Var "cur")
      assign' "cur" $ Bin Add (Var "cur") (Val $ VInt 1)
    return' $ Var "res"
  expr' $ Call "print" [Call "fact" [Val $ VInt 3]]
  expr' $ Call "print" [Call "fact" [Val $ VInt 5]]

main :: IO ()
main = interpretIO fact
