module Cuttlefish.Parser.Core where

import           Data.Char
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text ( Text )
import qualified Data.Text                  as T
import           Control.Monad ( void )
import           Data.Void
import           Data.String.Conversions
import           Cuttlefish.Ast

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

int :: Parser Int
int = lexeme (L.signed sc L.decimal)

float :: Parser Double
float = lexeme (L.signed sc L.float)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

squotes :: Parser a -> Parser a
squotes = between (symbol "'") (symbol "'")

dquotes :: Parser a -> Parser a
dquotes = between (symbol "\"") (symbol "\"")

comma :: Parser ()
comma = void $ symbol ","

dot :: Parser ()
dot = void $ symbol "."

colon :: Parser ()
colon = void $ symbol ":"

pipe :: Parser ()
pipe = void $ symbol "|"

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text]
rws =
  [ "type"
  , "int"
  , "float"
  , "char"
  , "if"
  , "else"
  , "let"
  , "mut"
  , "for"
  , "in"
  , "of"
  , "class"
  , "member"
  , "return"
  , "match"
  , "func" ]

binopChars :: [Char]
binopChars = "&|=!><+-*/^"

binop :: Parser Text
binop = (lexeme . try) (T.pack <$> some (oneOf binopChars))

typeIdentifier :: Parser Text
typeIdentifier = (lexeme . try) p
  where
    p = fmap T.pack $ (:) <$> upperChar
                          <*> many alphaNumChar

identifier :: Parser Text
identifier = (lexeme . try) p
  where
    p = fmap T.pack $ (:) <$> lowerChar
                          <*> many alphaNumChar

identifier' :: Parser Text
identifier' = T.pack <$> some alphaNumChar

maybeList :: Maybe [a] -> [a]
maybeList (Just xs) = xs
maybeList Nothing   = []
