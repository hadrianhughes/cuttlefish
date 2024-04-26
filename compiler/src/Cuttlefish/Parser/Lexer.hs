module Cuttlefish.Parser.Lexer where

import           Control.Applicative (liftA2)
import           Control.Applicative.Combinators
import           Data.Char
import           Data.Text ( Text )
import qualified Data.Text                  as T
import           Control.Monad ( void )
import           Data.Void
import           Data.String.Conversions
import           Cuttlefish.Parser.Core (Parser)
import qualified Cuttlefish.Parser.Core as P
import           Cuttlefish.Parser.Ast

parens :: Parser a -> Parser a
parens = between (P.char '(') (P.char ')')

brackets :: Parser a -> Parser a
brackets = between (P.char '[') (P.char ']')

braces :: Parser a -> Parser a
braces = between (P.char '{') (P.char '}')

manyInBraces :: Parser a -> Parser [a]
manyInBraces p = braces (manyTill (p <* P.sc) (P.lookAhead $ P.char '}'))

angles :: Parser a -> Parser a
angles = between (P.char '<') (P.char '>')

squotes :: Parser a -> Parser a
squotes = between (P.char '\'') (P.char '\'')

dquotes :: Parser a -> Parser a
dquotes = between (P.char '\"') (P.char '\"')

comma :: Parser ()
comma = void $ P.char ','

dot :: Parser ()
dot = void $ P.char '.'

colon :: Parser ()
colon = void $ P.char ':'

pipe :: Parser ()
pipe = void $ P.char '|'

rword :: Text -> Parser ()
rword w = P.symbol (T.unpack w) *> P.notFollowedBy P.alphaNumChar

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
  else pure w

binopChars :: [Char]
binopChars = "&|=!><+-*/^"

binop :: Parser Text
binop = T.pack <$> some (P.oneOf binopChars)

typeIdentifier :: Parser Text
typeIdentifier = fmap T.pack $ (:) <$> P.upperChar <*> many P.alphaNumChar

identifier :: Parser Text
identifier = binopP <|> normalP
  where
    binopP  = parens binop
    normalP = fmap T.pack $ (:) <$> P.letterChar <*> many P.alphaNumChar

sepBy2 :: Parser a -> Parser () -> Parser [a]
sepBy2 p sep = (liftA2 (:)) (p <* sep) (p `sepBy1` sep)
