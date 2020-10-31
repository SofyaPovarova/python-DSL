module Main where

import Text.Megaparsec
import System.Environment (getArgs)

import Py
import PyInterpreter
import PyParser
import PyPrinter

type Code = FreeLang ()

processCode :: FilePath -> IO Code
processCode path = do
  fCode <- readFile path
  case parse pyParse path fCode of
    Right lang -> return lang
    Left err -> do
      putStrLn $ errorBundlePretty err
      error ""

printCode :: Code -> IO ()
printCode = putStr . pretty . bodyToLangs

interpretCode :: Code -> IO ()
interpretCode = interpretIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["print", path] -> processCode path >>= printCode
    ["run", path] -> processCode path >>= interpretCode
    _ -> error "usage must be: stack run <print>|<run> filepath"
