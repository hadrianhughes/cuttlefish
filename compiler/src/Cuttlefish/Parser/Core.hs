module Cuttlefish.Parser.Core where

import           Control.Applicative (liftA2)
import           Data.Char
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text ( Text )
import qualified Data.Text                  as T
import           Control.Monad ( void )
import           Data.Void
import           Data.String.Conversions
import           Cuttlefish.Parser.Ast

type Parser = Parsec Void Text

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "//"

fsc :: Parser ()
fsc = L.space space1 lineCmnt empty

hsc :: Parser ()
hsc = L.space hspace1 lineCmnt empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hsc

int :: Parser Int
int = lexeme (L.signed hsc L.decimal)

float :: Parser Double
float = lexeme (L.signed hsc L.float)

parens :: Parser () -> Parser a -> Parser a
parens sc = between (L.symbol sc "(") (sc *> L.symbol sc ")")

brackets :: Parser () -> Parser a -> Parser a
brackets sc = between (L.symbol sc "[") (sc *> L.symbol sc "]")

braces :: Parser () -> Parser a -> Parser a
braces sc = between (L.symbol sc "{") (sc *> L.symbol sc "}")

manyInBraces :: Parser a -> Parser [a]
manyInBraces p = braces fsc (manyTill (p <* fsc) (lookAhead $ L.symbol fsc "}"))

angles :: Parser () -> Parser a -> Parser a
angles sc = between (L.symbol sc "<") (sc *> L.symbol sc ">")

squotes :: Parser a -> Parser a
squotes = between (L.symbol hsc "'") (L.symbol hsc "'")

dquotes :: Parser a -> Parser a
dquotes = between (L.symbol hsc "\"") (L.symbol hsc "\"")

comma :: Parser ()
comma = void $ L.symbol hsc ","

dot :: Parser ()
dot = void $ L.symbol hsc "."

colon :: Parser ()
colon = void $ L.symbol hsc ":"

pipe :: Parser ()
pipe = void $ L.symbol hsc "|"

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text]
rws =
  [ "type"
  , "int"
  , "float"
  , "char"
  , "if"
  , "then"
  , "else"
  , "let"
  , "mut"
  , "for"
  , "in"
  , "of"
  , "class"
  , "extends"
  , "member"
  , "return"
  , "match"
  , "func"
  , "effect" ]

checkIsRW :: Text -> Parser Text
checkIsRW w = if w `elem` rws
  then fail $ "keyword " <> show w <> " cannot be an identifier"
  else pure w

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
identifier = (lexeme . try) (binop <|> normalP)
  where
    normalP = fmap T.pack $ (:) <$> letterChar <*> many alphaNumChar

sepBy2 :: Parser a -> Parser () -> Parser [a]
sepBy2 p sep = (liftA2 (:)) (p <* sep) (p `sepBy1` sep)
