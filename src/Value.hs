module Value
  ( Name
  , Value(..)
  , isTrue
  , pyAnd
  , pyOr
  , valueType
  ) where

type Name = String

data Value
  = VInt Int
  | VFloat Float
  | VBool Bool
  | VString String
  | VNone
  deriving (Eq, Ord)

instance Num Value where
  (VInt a) + (VInt b)           = VInt $ a + b
  (VInt a) + (VFloat b)         = VFloat $ (fromIntegral a) + b
  (VFloat a) + (VInt b)         = VFloat $ a + fromIntegral b
  (VFloat a) + (VFloat b)       = VFloat $ a + b
  (VBool a) + (VBool b)         = VInt $ fromEnum a + fromEnum b
  (VString a) + (VString b)     = VString $ a ++ b
  a + b                         = error $ "unsupported '+' operation for "
                                  <> valueType a <> " and " <> valueType b

  (VBool a) - (VBool b)         = VInt $ fromEnum a - fromEnum b
  a - b                         = a + negate b

  (VInt a) * (VInt b)           = VInt $ a * b
  (VInt a) * (VFloat b)         = VFloat $ (fromIntegral a) * b
  (VFloat a) * (VInt b)         = VFloat $ a * fromIntegral b
  (VFloat a) * (VFloat b)       = VFloat $ a * b
  (VBool a) * (VBool b)         = VInt $ fromEnum a * fromEnum b
  (VBool a) * (VInt b)          = VInt $ fromEnum a * b
  (VBool a) * (VFloat b)        = VFloat $ (fromIntegral $ fromEnum a) * b
  (VInt a) * (VBool b)          = VInt $ a * fromEnum b
  (VFloat a) * (VBool b)        = VFloat $ a * (fromIntegral $ fromEnum b)
  a * b                         = error $ "unsupported '*' operation for "
                                  <> valueType a <> " and " <> valueType b

  abs (VInt a)    = VInt $ abs a
  abs (VFloat a)  = VFloat $ abs a
  abs (VBool _)   = VBool True
  abs a           = error $ "unsupported 'abs' operation for " <> valueType a

  signum (VInt a)       = VInt $ signum a
  signum (VFloat a)     = VFloat $ signum a
  signum (VBool a)      = VInt $ fromEnum a
  signum a              = error $ "unsupported 'signum' operation for " <> valueType a

  fromInteger a = VInt $ fromIntegral a

  negate (VInt a)       = VInt $ negate a
  negate (VFloat a)     = VFloat $ negate a
  negate (VBool a)      = VBool $ not a
  negate a              = error $ "unsupported 'negate' operation for " <> valueType a

instance Fractional Value where
  recip (VInt a)      = VFloat $ 1 / fromIntegral a
  recip (VFloat a)    = VFloat $ 1 / a
  recip (VBool True)  = VFloat $ 1.0
  recip (VBool False) = error "Division by zero"
  recip s             = error $ "unsupported '/' operation for " <> valueType s

  fromRational a      = VFloat $ fromRational a


pyAnd :: Value -> Value -> Value
pyAnd (VBool a) (VBool b) = VBool $ a && b
pyAnd a b = error $ "unsupported 'and' operation for "
              <> valueType a <> " and " <> valueType b

pyOr :: Value -> Value -> Value
pyOr (VBool a) (VBool b) = VBool $ a || b
pyOr a b = error $ "unsupported 'or' operation for "
             <> valueType a <> " and " <> valueType b

instance Show Value where
  show (VBool b)    = show b
  show (VFloat f)   = show f
  show (VInt n)     = show n
  show VNone        = "None"
  show (VString s)  = s

isTrue :: Value -> Bool
isTrue val = case val of
  VBool b   -> b
  VFloat f  -> f /= 0.0
  VInt i    -> i /= 0
  VNone     -> False
  VString s -> not $ null s

valueType :: Value -> String
valueType val = case val of
  VInt _    -> "int"
  VFloat _  -> "float"
  VBool _   -> "bool"
  VString _ -> "str"
  VNone     -> "None"
