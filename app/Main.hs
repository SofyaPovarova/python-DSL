module Main where

import PyInterpreter
import PyPrinter
import PyValue
import Py

main :: IO ()
main = do
  interpretIO factM
  putStr $ pretty factL
  interpretIO big_printM
  putStr $ pretty big_printL

-- Examples (here now, will be moved)

-- examples/big_print.py
-- i = "I"
-- l = int("1")
-- o = str(0)
-- v = "\\" + "/"
-- e = "E"
-- haskell = "h 4" + " $ " + "k e | |"
-- print(i, "  ", l, o, v, e, "  ", haskell)

big_printL :: [Lang]
big_printL =
  [ Assign "i" $ Val $ VString "I"
  , Assign "l" $ Call "int" [Val $ VString "1"]
  , Assign "o" $ Call "str" [Val $ VInt 0]
  , Assign "v" $ Bin Add (Val $ VString "\\") (Val $ VString "/")
  , Assign "e" $ Val $ VString "E"
  , Assign "haskell" $ Bin Add (Bin Add (Val $ VString "h 4") (Val $ VString " $ ")) (Val $ VString "k e | |")
  , Expr $ Call "print" [Var "i", Val $ VString "  ", Var "l", Var "o", Var "v", Var "e", Val $ VString "  ", Var "haskell"]
  ]

big_printM :: FreeLang ()
big_printM = do
  assign' "i" $ Val $ VString "I"
  assign' "l" $ Call "int" [Val $ VString "1"]
  assign' "o" $ Call "str" [Val $ VInt 0]
  assign' "v" $ (Bin Add (Val $ VString "\\") (Val $ VString "/"))
  assign' "e" $ Val $ VString "E"
  assign' "haskell" $ Bin Add (Bin Add (Val $ VString "h 4") (Val $ VString " $ ")) (Val $ VString "k e | |")
  expr' $ Call "print" [Var "i", Val $ VString "  ", Var "l", Var "o", Var "v", Var "e", Val $ VString "  ", Var "haskell"]

-- examples/fact.py
-- def fact(x):
--     cur = 1
--     res = 1
--     while cur <= x:
--         res = res * cur
--         cur = cur + 1
--     return res
-- print(fact(3))
-- print(fact(5))

factL :: [Lang]
factL =
  [ Def "fact" ["x"]
      [ Assign "cur" (Val $ VInt 1)
      , Assign "res" (Val $ VInt 1)
      , While (Bin LEt (Var "cur") (Var "x"))
          [ Assign "res" (Bin Mul (Var "res") (Var "cur"))
          , Assign "cur" (Bin Mul (Var "cur") (Val $ VInt 1))
          ]
      , Return $ Var "res"
      ]
  , Expr $ Call "print" [Call "fact" [Val $ VInt 3]]
  , Expr $ Call "print" [Call "fact" [Val $ VInt 5]]
  ]

factM :: FreeLang ()
factM = do
  def' "fact" ["x"] $ do
    assign' "cur" $ Val $ VInt 1
    assign' "res" $ Var "cur"
    while' (Bin LEt (Var "cur") (Var "x")) $ do
      assign' "res" $ Bin Mul (Var "res") (Var "cur")
      assign' "cur" $ Bin Add (Var "cur") (Val $ VInt 1)
    return' $ Var "res"
  expr' $ Call "print" [Call "fact" [Val $ VInt 3]]
  expr' $ Call "print" [Call "fact" [Val $ VInt 5]]
