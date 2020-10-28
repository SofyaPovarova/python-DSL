{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module PInterpreter
  ( interpretIO
  ) where

import Value
import Python

import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import qualified Data.Map as M
import Control.Monad.Catch
    ( Exception, MonadCatch(catch), MonadThrow(throwM) )
import Control.Monad.Free ( Free(Free, Pure) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )
import GHC.Float (float2Int)
import Text.Read (readMaybe)


type ScopeValues = M.Map Name ValueOrBody

data ValueOrBody =
    VBValue Value
  | VBDef [Name] Body

pyFuncs :: M.Map Name PyFunc
pyFuncs = M.fromList
  [ ("input", PFInput),
    ("int", PFInt),
    ("print", PFPrint),
    ("str", PFStr)
  ]

data PyFunc =
    PFInput
  | PFInt
  | PFPrint
  | PFStr
  deriving Eq

type AppM = ReaderT Env IO

pyFunc :: PyFunc -> [Value] -> AppM Value
pyFunc PFInput args = case args of
  [] -> do
    str <- liftIO getLine
    return $ VString str
  _ -> throwM $ UnsupportedArgs "input" args
pyFunc PFInt args = case args of
  a:[] -> do
    case a of
      VBool b -> return $ VInt $ fromEnum b
      VFloat f -> return $ VInt $ float2Int f
      i@(VInt _) -> return i
      VString s -> maybe (throwM $ UnsupportedArgs "int" args) (return . VInt) (readMaybe s)
      VNone -> throwM $ UnsupportedArgs "int" args
  _ -> throwM $ UnsupportedArgs "int" args
pyFunc PFPrint args = do
  liftIO $ putStrLn $ unwords $ map show args
  return VNone
pyFunc PFStr args = case args of
  a:[] -> return $ VString $ show a
  _ -> throwM $ UnsupportedArgs "str" args


newtype Env = Env { eScopeValues :: IORef ScopeValues }

data Error =
    NotFunction Name
  | NotVariable Name
  | ReturnCheat Value
  | Undefined Name
  | UnsupportedArgs Name [Value]
  | TypesMismatch BinOp Value Value
  deriving Eq
  deriving anyclass Exception

instance Show Error where
  show (NotFunction name)           = name <> " is not a function"
  show (NotVariable name)           = name <> " is not a variable"
  show (Undefined name)             = "name '" <> name <> "' is not defined"
  show (UnsupportedArgs name vals)  = name <> "() doesn't support arguments: "
                                      <> unwords (map show vals)
  show (TypesMismatch op val1 val2) = "can't perform '" <> show op <> "' of "
                                      <> valueType val1 <> " and "
                                      <> valueType val2
  show (ReturnCheat v)              = "incorrect calling of return(" <> show v <> ")"


setValue :: Name -> ValueOrBody -> AppM ()
setValue name valueOrBody = do
  env <- ask
  let scopeRef = eScopeValues env
  scope <- liftIO $ readIORef scopeRef
  liftIO $ writeIORef scopeRef $ M.insert name valueOrBody scope

getValue :: Name -> AppM ValueOrBody
getValue name = do
  scope <- ask >>= (liftIO . readIORef . eScopeValues)
  let mVal = M.lookup name scope
  case mVal of
    Just val -> return val
    Nothing -> throwM $ Undefined name

eval :: Expr -> AppM Value
eval (Val val) = return val
eval (Var name) = do
  val <- getValue name
  case val of
    VBValue v -> return v
    VBDef _ _ -> throwM $ NotVariable name
eval (Un op expr) = do
  val <- eval expr
  evalUn op val
eval (Bin op expr1 expr2) = do
  val1 <- eval expr1
  val2 <- eval expr2
  evalBin op val1 val2
eval (Call name exprs) = do
  env <- ask
  let scopeRef = eScopeValues env
  scope <- liftIO $ readIORef scopeRef
  vals <- mapM eval exprs
  case M.lookup name pyFuncs of
    Just pf -> do -- built in functions
      val <- pyFunc pf vals
      return val
    Nothing -> do -- defined functions
      case M.lookup name scope of
        Just (VBDef params body) -> do
          if length vals /= length params
          then throwM $ UnsupportedArgs name vals
          else do
            let innerScopeL = zip params $ map VBValue vals
            let innerScope = M.fromList $ (M.toList scope) <> innerScopeL
            liftIO $ writeIORef scopeRef innerScope
            returned <- (toAppM body >> return VNone) `catch` \case
              ReturnCheat v -> return v
              otherErr -> throwM otherErr
            liftIO $ writeIORef scopeRef scope
            return returned
        Just (VBValue _) -> throwM $ NotFunction name
        Nothing -> throwM $ Undefined name

evalUn :: UnOp -> Value -> AppM Value
evalUn op val =
  case op of
    Neg -> return $ negate val
    Not -> return $ negate $ VBool $ isTrue val

evalBin :: BinOp -> Value -> Value -> AppM Value
evalBin op val1 val2 =
  case op of
    Add -> return $ val1 + val2
    Sub -> return $ val1 - val2
    Mul -> return $ val1 * val2
    Div -> return $ val1 * val2
    And -> return $ val1 `pyAnd` val2
    Or  -> return $ val1 `pyOr` val2
    Eq  -> return $ VBool $ val1 == val2
    Neq -> return $ VBool $ val1 /= val2
    GEt -> return $ VBool $ val1 >= val2
    Gt  -> return $ VBool $ val1 > val2
    LEt -> return $ VBool $ val1 <= val2
    Lt  -> return $ VBool $ val1 < val2

toAppM :: FreeLang () -> AppM ()
toAppM (Pure l) = return l
toAppM (Free (Expr' expr next)) = do
  _ <- eval expr
  toAppM next
toAppM (Free (Assign' name expr next)) = do
  eval expr >>= (setValue name . VBValue)
  toAppM next
toAppM (Free (If' expr body next)) = do
  toBody <- eval expr
  if isTrue toBody
  then do
    toAppM body
    toAppM next
  else toAppM next
toAppM while@(Free (While' expr body next)) = do
  toBody <- eval expr
  if isTrue toBody
  then do
    toAppM body
    toAppM while
  else toAppM next
toAppM (Free (Def' name params body next)) = do
  setValue name $ VBDef params body
  toAppM next
toAppM (Free (Return' expr _)) = do
  eval expr >>= throwM . ReturnCheat

interpretIO :: FreeLang () -> IO ()
interpretIO fLang = do
  scope <- newIORef M.empty
  runReaderT (toAppM fLang) $ Env scope
