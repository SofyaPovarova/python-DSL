module PyParser
  ( pyParse
  ) where

import qualified Text.Megaparsec.Char.Lexer as Lex

import Data.Foldable (sequenceA_)
import Data.Void (Void)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Text.Megaparsec
import Text.Megaparsec.Char

import Py
import PyValue

type Parser = Parsec Void String

sp :: Parser ()
sp = Lex.space (void $ oneOf [' ', '\t']) empty empty

spOrNl :: Parser ()
spOrNl = Lex.space (void $ oneOf [' ', '\t', '\n']) empty empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sp

symbol :: String -> Parser String
symbol = Lex.symbol sp

semi :: Parser String
semi = symbol ":"

comma :: Parser String
comma = symbol ","

eq :: Parser String
eq = symbol "="

inParenth :: Parser a -> Parser a
inParenth = between (symbol "(") (symbol ")")

int :: Parser Value
int = VInt <$> lexeme Lex.decimal

bool :: Parser Value
bool = VBool <$>
  ( (symbol "True" *> return True) <|>
    (symbol "False" *> return False)
  )

str :: Parser Value
str = VString <$>
  ( (single '"') *> someTill Lex.charLiteral
    (single '"')
  )

term :: Parser Expr
term =
  inParenth expr
  <|> call
  <|> Var <$> name
  <|> Val <$> int
  <|> Val <$> bool
  <|> Val <$> str
  <|> symbol "None" *> return (Val VNone)

name :: Parser Name
name = lexeme $ try $ do
  onlyChar <- letterChar
  otherSym <- many (alphaNumChar <|> single '_')
  fName <- return $ onlyChar : otherSym
  checkPyWord fName

call :: Parser Expr
call = try $ do
  fName <- name
  args <- inParenth $ sepBy expr comma
  return $ Call fName args

langExpr :: Parser Lang
langExpr = do
  e <- expr
  spOrNl
  return $ Expr e

langAssign :: Parser Lang
langAssign = do
  vName <- name
  eq
  vExpr <- expr
  spOrNl
  return $ Assign vName vExpr

langIf :: Parser Lang
langIf = indent $ do
  symbol "if"
  ifExpr <- expr
  semi
  indentBody $ If ifExpr . bodyToLangs

langWhile :: Parser Lang
langWhile = indent $ do
  symbol "while"
  whileExpr <- expr
  semi
  indentBody $ While whileExpr . bodyToLangs

langDef :: Parser Lang
langDef = indent $ do
  symbol "def"
  fName <- name
  params <- inParenth $ sepBy name comma
  semi
  indentBody $ Def fName params . bodyToLangs

langReturn :: Parser Lang
langReturn = do
  symbol "return"
  retExpr <- expr
  spOrNl
  return $ Return retExpr

indent :: Parser (Lex.IndentOpt Parser a b) -> Parser a
indent = Lex.indentBlock spOrNl

indentBody :: (Body -> Lang) -> Parser (Lex.IndentOpt Parser Lang Lang)
indentBody bToL =
  return $ Lex.IndentSome Nothing (return . bToL . langsToBody) lang

body :: Parser Body
body = langsToBody <$> many lang

lang :: Parser Lang
lang = choice $ (try <$>)
  [ langAssign
  , langExpr
  , langIf
  , langWhile
  , langDef
  , langReturn
  ]

expr :: Parser Expr
expr = makeExprParser term
  [ [ Prefix (Un Neg <$ symbol "-")]
  , [ Prefix (Un Not <$ symbol "not")]
  , [ InfixL (Bin Mul <$ symbol "*")
    , InfixL (Bin Div <$ symbol "/")
    ]
  , [ InfixL (Bin And <$ symbol "and") ]
  , [ InfixL (Bin Or <$ symbol "or") ]
  , [ InfixL (Bin Add <$ symbol "+")
    , InfixL (Bin Sub <$ symbol "-")
    ]
  , [ InfixL (Bin Eq <$ symbol "==")
    , InfixL (Bin Neq <$ symbol "!=")
    , InfixL (Bin GEt <$ symbol ">=")
    , InfixL (Bin Gt <$ symbol ">")
    , InfixL (Bin LEt <$ symbol "<=")
    , InfixL (Bin Lt <$ symbol "<")
    ]
  ]

pyParse :: Parser Body
pyParse = (Lex.nonIndented spOrNl body) <* eof


-- helpers

langsToBody :: [Lang] -> Body
langsToBody = sequenceA_ . (langToBody <$>)

pyWords :: [String]
pyWords =
  [ "None"
  , "not"
  , "and"
  , "or"
  , "if"
  , "while"
  , "def"
  , "return"
  ]

checkPyWord :: MonadFail m => String -> m String
checkPyWord word
  | word `elem` pyWords = fail $ "name '" ++ show word ++ "' is reserved"
  | otherwise = return word
