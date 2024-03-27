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
import           Cuttlefish.Ast

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
parens sc = between (L.symbol sc "(") (L.symbol sc ")")

brackets :: Parser () -> Parser a -> Parser a
brackets sc = between (L.symbol sc "[") (L.symbol sc "]")

braces :: Parser () -> Parser a -> Parser a
braces sc = between (L.symbol sc "{") (L.symbol sc "}")

angles :: Parser () -> Parser a -> Parser a
angles sc = between (L.symbol sc "<") (L.symbol sc ">")

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
  , "member"
  , "return"
  , "match"
  , "func"
  , "effect" ]

checkIsRW :: Text -> Parser Text
checkIsRW w = if w `elem` rws
  then fail $ "keyword " <> show w <> " cannot be an identifier"
  else return w

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
identifier = (lexeme . try) (binopP <|> normalP)
  where
    binopP  = parens hsc binop
    normalP = fmap T.pack $ (:) <$> lowerChar
                               <*> many alphaNumChar

identifier' :: Parser Text
identifier' = T.pack <$> some alphaNumChar

maybeList :: Maybe [a] -> [a]
maybeList (Just xs) = xs
maybeList Nothing   = []

sepBy2 :: Parser a -> Parser () -> Parser [a]
sepBy2 p sep = (liftA2 (:)) (p <* sep) (p `sepBy1` sep)
