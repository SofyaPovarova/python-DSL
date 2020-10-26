{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module PInterpreter
  (

  ) where

import Value
import Python

import Data.IORef
import qualified Data.Map as M
import Control.Monad.Catch
import Control.Monad.Free
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Reader

type Values = M.Map Name Value
type AppM = ReaderT Env IO

-- TODO think if input and output are needed
data Env = Env { eValues :: IORef Values
               , eInput :: [String]
               , eOutput :: [String]
               }

data Error =
    TypesMismatch BinOp Value Value
  | Undefined Name
  deriving Eq
  deriving anyclass Exception

instance Show Error where
  show (TypesMismatch op val1 val2) = "incorrect types for " <> (show op) <> " operation: " <>
                                      (valueType val1) <> " and " <> (valueType val2)
  show (Undefined name)             = "name '" <> name <> "' is not defined"



setValue :: Name -> Value -> AppM ()
setValue name val = do
  env <- ask
  vals <- liftIO $ readIORef $ eValues env
  liftIO $ writeIORef (eValues env) $ M.insert name val vals

getValue :: Name -> AppM Value
getValue name = do
  env <- ask
  vals <- liftIO $ readIORef $ eValues env
  let mVal = M.lookup name vals
  case mVal of
    Just val -> return val
    Nothing -> throwM $ Undefined name

eval :: Expr -> AppM Value
eval (Val val) = return val
eval (Var name) = getValue name
eval (Bin op expr1 expr2) = do
  val1 <- eval expr1
  val2 <- eval expr2
  evalBin op val1 val2

evalBin :: BinOp -> Value -> Value -> AppM Value
evalBin op val1 val2 =
  case op of
    Add -> return $ val1 + val2
    Mul -> return $ val1 * val2

freeToAppM :: Lang () -> AppM ()
freeToAppM (Pure l) = return l
freeToAppM (Free (Assign name expr next)) = do
  val <- eval expr
  setValue name val
  freeToAppM next
freeToAppM (Free (Print' expr next)) = do
  env <- ask
  vals <- mapM eval expr
  liftIO $ putStrLn $ concat $ map show vals
  let output = show vals : eOutput env
  withReaderT (\env -> env {eOutput = output}) $ freeToAppM next
freeToAppM (Free (Input f)) = do
  env <- ask
  str <- liftIO $ getLine
  let input = str : eInput env
  withReaderT (\env -> env {eInput = input}) $ freeToAppM $ f str

interpretIO :: AppM () -> IO ()
interpretIO ioReader = do
  vals <- newIORef M.empty
  runReaderT ioReader $ Env vals [] []

example :: Lang ()
example = do
  let expr1 = Bin Add (Val $ VInt 2) (Val $ VInt 3)
  let expr2 = Val $ VInt 10
  name <- input
  assign name expr1
  print' $ [Val $ VString $ "The value of '" ++ name ++ "' is ", Var name]
  assign name expr2
  print' $ [Val $ VString $ "Now the value of '" ++ name ++ "' is ", Var name]
  print' $ [Var "b"]
