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

hsc :: Parser ()
hsc = L.space hspace1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

endLine :: Parser a -> Parser a
endLine p = p <* sc

integer :: Parser Int
integer = lexeme (L.signed hsc L.decimal)

float :: Parser Double
float = L.signed hsc L.float

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hsc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

squotes :: Parser a -> Parser a
squotes = between (L.symbol hsc "'") (L.symbol hsc "'")

dquotes :: Parser a -> Parser a
dquotes = between (L.symbol hsc "\"") (L.symbol hsc "\"")

comma :: Parser ()
comma = void $ symbol ","

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
  , "class"
  , "member"
  , "of"
  , "return" ]

binopChars :: [Char]
binopChars = "&|=!><+-*/^"

binop :: Parser Text
binop = (lexeme . try) (T.pack <$> some (oneOf binopChars))

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = fmap T.pack $ (:) <$> lowerChar
                          <*> many (alphaNumChar <|> single '_')
    check x = if x `elem` rws
      then fail $ "keyword " <> show x <> "cannot be used as an identifier."
      else return x
