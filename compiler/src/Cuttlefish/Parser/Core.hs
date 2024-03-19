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

type Parser' a = Parser () -> Parser a

fsc :: Parser ()
fsc = L.space space1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

hsc :: Parser ()
hsc = L.space hspace1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

lexeme :: Parser () -> Parser a -> Parser a
lexeme sc = L.lexeme sc

integer :: Parser Int
integer = lexeme hsc (L.signed hsc L.decimal)

float :: Parser Double
float = L.signed hsc (L.signed hsc L.float)

parens :: Parser a -> Parser a
parens = between (L.symbol fsc "(") (L.symbol fsc ")")

brackets :: Parser a -> Parser a
brackets = between (L.symbol fsc "[") (L.symbol fsc "]")

braces :: Parser a -> Parser a
braces = between (L.symbol fsc "{") (L.symbol fsc "}")

squotes :: Parser a -> Parser a
squotes = between (L.symbol hsc "'") (L.symbol fsc "'")

dquotes :: Parser a -> Parser a
dquotes = between (L.symbol hsc "\"") (L.symbol fsc "\"")

comma :: Parser ()
comma = void $ L.symbol fsc ","

rword :: Parser () -> Text -> Parser ()
rword sc w = (lexeme sc . try) (string w *> notFollowedBy alphaNumChar)

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
  , "class"
  , "member"
  , "of"
  , "return" ]

binopChars :: [Char]
binopChars = "&|=!><+-*/^"

binop :: Parser Text
binop = (L.lexeme hsc . try) (T.pack <$> some (oneOf binopChars))

typeIdentifier :: Parser' Text
typeIdentifier sc = (lexeme sc . try) p
  where
    p = fmap T.pack $ (:) <$> upperChar
                          <*> many alphaNumChar

identifier :: Parser () -> Parser Text
identifier sc = (L.lexeme sc . try) (p >>= check)
  where
    p = fmap T.pack $ (:) <$> lowerChar
                          <*> many (alphaNumChar <|> single '_')
    check x = if x `elem` rws
      then fail $ "keyword " <> show x <> "cannot be used as an identifier."
      else return x
