module Cuttlefish.Parser.Lexer where

import           Control.Applicative (liftA2)
import           Control.Applicative.Combinators
import           Data.Char
import           Control.Monad ( void )
import           Data.Void
import           Data.String.Conversions
import           Cuttlefish.Parser.Core (Parser)
import qualified Cuttlefish.Parser.Core as P
import           Cuttlefish.Parser.Ast

sc = P.sc lineComment
sc' = P.sc' lineComment

symbol :: String -> Parser String
symbol = P.symbol sc'

lineComment :: Parser ()
lineComment = void $ P.symbol' "//" *> many (P.notOneOf "\n")

int = P.int

float = P.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parens' :: Parser a -> Parser a
parens' = between (P.symbol sc "(") (P.symbol sc ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

brackets' :: Parser a -> Parser a
brackets' = between (P.symbol sc "[") (P.symbol sc "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

braces' :: Parser a -> Parser a
braces' = between (P.symbol sc "{") (P.symbol sc "}")

manyInBraces :: Parser a -> Parser [a]
manyInBraces p = braces (manyTill (p <* sc) (P.lookAhead $ P.char '}'))

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

rword :: String -> Parser ()
rword w = symbol w *> P.notFollowedBy P.alphaNumChar

rws :: [String]
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

checkIsRW :: String -> Parser String
checkIsRW w = if w `elem` rws
  then fail $ "keyword " <> w <> " cannot be an identifier"
  else pure w

binopChars :: [Char]
binopChars = "&|=!><+-*/^"

binop :: Parser String
binop = some (P.oneOf binopChars)

typeIdentifier :: Parser String
typeIdentifier = (:) <$> P.upperChar <*> many P.alphaNumChar

identifier :: Parser String
identifier = binopP <|> normalP
  where
    binopP  = parens binop
    normalP = (:) <$> P.letterChar <*> many P.alphaNumChar

sepBy2 :: Parser a -> Parser () -> Parser [a]
sepBy2 p sep = (liftA2 (:)) (p <* sep) (p `sepBy1` sep)
