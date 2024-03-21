module Cuttlefish.Parser.Core where

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt empty
  where
    lineCmnt = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Parser a -> Parser a
symbol = L.symbol sc

int :: Parser Int
int = lexeme sc (L.signed sc L.decimal)

float :: Parser Double
float = lexeme sc (L.signed sc L.float)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

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
  , "match" ]

binopChars :: [Char]
binopChars = "&|=!><+-*/^"
