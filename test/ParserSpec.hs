module ParserSpec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec

import Py
import PyParser
import PyValue

spec :: Spec
spec = do
  describe "factorial test" $
    it "check parsing of implementation of the factorial function" $ do
      let actual = (either (const []) bodyToLangs $ parse pyParse "" factorialString) == factorialLang
      actual `shouldBe` True
  describe "built-ins test" $
    it "check parsing built-in functions" $ do
      let actual = (either (const []) bodyToLangs $ parse pyParse "" builtinString) == builtinLang
      actual `shouldBe` True
  describe "arithmetic test" $
    it "check parsing arithmetic expressions" $ do
      let actual = (either (const []) bodyToLangs $ parse pyParse "" arithString) == arithLang
      actual `shouldBe` True

-- factorial
factorialString :: String
factorialString = unlines
  [ "def fact(x):"
  , "    cur = 1"
  , "    res = 1"
  , "    while cur <= x:"
  , "        res = res * cur"
  , "        cur = cur * 1"
  , "    return res"
  , "print(fact(1))"
  , "print(fact(3))"
  , "print(fact(5))"
  ]

factorialLang :: [Lang]
factorialLang =
  [ Def "fact" ["x"]
      [ Assign "cur" (Val $ VInt 1)
      , Assign "res" (Val $ VInt 1)
      , While (Bin LEt (Var "cur") (Var "x"))
          [ Assign "res" (Bin Mul (Var "res") (Var "cur"))
          , Assign "cur" (Bin Mul (Var "cur") (Val $ VInt 1))
          ]
      , Return $ Var "res"
      ]
  , Expr $ Call "print" [Call "fact" [Val $ VInt 1]]
  , Expr $ Call "print" [Call "fact" [Val $ VInt 3]]
  , Expr $ Call "print" [Call "fact" [Val $ VInt 5]]
  ]

-- built-ins
builtinString :: String
builtinString = unlines
  [ "i = \"I\""
  , "l = int(\"1\")"
  , "o = str(0)"
  , "v =  \"V\""
  , "e = int(3)"
  , "haskell = \"h 4 $ k e | |\""
  , "print(i, \"  \", l, o, v, e, \"  \", haskell)"
  , "inp = input()"
  , "print(int(inp))"
  ]

builtinLang :: [Lang]
builtinLang =
  [ Assign "i" $ Val $ VString "I"
  , Assign "l" $ Call "int" [Val $ VString "1"]
  , Assign "o" $ Call "str" [Val $ VInt 0]
  , Assign "v" $ Val $ VString "V"
  , Assign "e" $ Call "int" [Val $ VInt 3]
  , Assign "haskell" $ Val $ VString "h 4 $ k e | |"
  , Expr $ Call "print" [ Var "i", Val $ VString "  ",
                          Var "l", Var "o", Var "v", Var "e",
                          Val $ VString "  ", Var "haskell"
                        ]
  , Assign "inp" $ Call "input" []
  , Expr $ Call "print" [Call "int" [Var "inp"]]
  ]

-- arith
arithString :: String
arithString = unlines
  [ "a_ = 3 - 4*1"
  , "b_ = 1*2*3 + 2"
  , "if (a_ < 0) and (b_ <= 10):"
  , "    print(\"True condition\")"
  ]

arithLang :: [Lang]
arithLang =
  [ Assign "a_" $ Bin Sub (Val $ VInt 3) (Bin Mul (Val $ VInt 4) (Val $ VInt 1))
  , Assign "b_" $ Bin Add (Bin Mul (Bin Mul (Val $ VInt 1) (Val $ VInt 2)) (Val $ VInt 3)) (Val $ VInt 2)
  , If (Bin And (Bin Lt (Var "a_") (Val $ VInt 0)) (Bin LEt (Var "b_") (Val $ VInt 10)))
      [ Expr $ Call "print" [Val $ VString "True condition"] ]
  ]
