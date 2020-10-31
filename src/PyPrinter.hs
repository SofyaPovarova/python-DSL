module PyPrinter
  ( pretty
  ) where

import Data.List (intercalate)

import Py

instance Show Expr where
  show (Var name)           = name
  show (Val val)            = show val
  show (Un op expr)         = unwords [show op, show expr]
  show (Bin op expr1 expr2) = unwords [show expr1, show op, show expr2]
  show (Call name args)     = name ++ "(" ++
                              (intercalate ", " $ map show args) ++ ")"

pretty :: [Lang] -> String
pretty lang = concat $ reverse $ go lang 0 []
  where
    go :: [Lang] -> Int -> [String] -> [String]
    go (l:ls) n acc = case l of
      Expr expr -> go ls n $ (ident n ++ show expr ++ "\n") : acc
      Assign name expr -> go ls n $ (ident n ++ name ++ " = " ++ show expr ++ "\n") : acc
      If expr body ->
        go ls n $ (go body (n + 4) []) ++
          (ident n ++ "if " ++ show expr ++ ":\n") : acc
      While expr body ->
        go ls n $ (go body (n + 4) []) ++
          (ident n ++ "while " ++ show expr ++ ":\n") : acc
      Def name params body ->
        go ls n $ (go body (n + 4) []) ++
          (ident n ++ "def " ++ name ++ "(" ++
          (intercalate ", " params) ++ ")" ++ ":\n") : acc
      Return expr -> go ls n $ (ident n ++ "return " ++ show expr ++ "\n") : acc
    go [] _ acc = acc

    ident :: Int -> String
    ident n = replicate n ' '
