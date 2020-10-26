module Value
  ( Name
  , Value(..)

  , valueType
  ) where

type Name = String

data Value =
    VBool Bool
  | VFloat Float
  | VInt Int
  | VString String
  deriving (Eq, Ord)

-- TODO fromEnum, toEnum for adding Bools and numbers with Bools
-- TODO add error handling (better then just error "msg")
instance Num Value where
  (VInt a) + (VInt b)           = VInt $ a + b
  (VInt a) + (VFloat b)         = VFloat $ (fromIntegral a) + b
  (VFloat a) + (VInt b)         = VFloat $ a + fromIntegral b
  (VFloat a) + (VFloat b)       = VFloat $ a + b
  (VBool True) + (VBool True)   = VInt 2
  (VBool True) + (VBool False)  = VInt 1
  (VBool False) + (VBool True)  = VInt 1
  (VBool False) + (VBool False) = VInt 0
  (VString a) + (VString b)     = VString $ a ++ b
  a + b                         = error $ "Types mismatch or unsupported operation while `+`: "
                                  <> (show a) <> (show b)

  (VInt a) * (VInt b)           = VInt $ a * b
  (VInt a) * (VFloat b)         = VFloat $ (fromIntegral a) * b
  (VFloat a) * (VInt b)         = VFloat $ a * fromIntegral b
  (VFloat a) * (VFloat b)       = VFloat $ a * b
  (VBool True) * (VBool True)   = VBool True
  (VBool True) * (VBool False)  = VBool False
  (VBool False) * (VBool True)  = VBool False
  (VBool False) * (VBool False) = VBool False
  a * b                         = error $ "Types mismatch or unsupported operation while `*`: "
                                  <> (show a) <> (show b)

  abs (VInt a)    = VInt $ abs a
  abs (VFloat a)  = VFloat $ abs a
  abs (VBool _)   = VBool True
  abs a           = error $ "Unsupported abs operation for "
                    <> show a

  signum (VInt a)       = VInt $ signum a
  signum (VFloat a)     = VFloat $ signum a
  signum (VBool True)   = VInt 1
  signum (VBool False)  = VInt 0
  signum a              = error $ "Unsupported signum operation for "
                          <> show a

  fromInteger a = VInt $ fromIntegral a

  negate (VInt a)       = VInt $ negate a
  negate (VFloat a)     = VFloat $ negate a
  negate (VBool True)   = VBool False
  negate (VBool False)  = VBool True  

instance Show Value where
  show (VInt n)     = show n
  show (VFloat f)   = show f
  show (VBool b)    = show b
  show (VString s)  = s

valueType :: Value -> String
valueType val = case val of
  VBool _   -> "bool"
  VFloat _  -> "float"
  VInt _    -> "int"
  VString _ -> "str"
