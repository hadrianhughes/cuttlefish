module Cuttlefish.Parser
  ( programP
  , runParser
  , errorBundlePretty
  )
where

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

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text]
rws =
  [ "type"
  , "int"
  , "unit"
  , "float"
  , "char" ]

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = fmap T.pack $ (:) <$> lowerChar
                          <*> many (alphaNumChar <|> single '_')
    check x = if x `elem` rws
      then fail $ "keyword " <> show x <> "cannot be used as an identifier."
      else return x

primType :: Parser PrimType
primType = Int   <$ rword "int"
       <|> Float <$ rword "float"
       <|> Bool  <$ rword "bool"
       <|> Unit  <$ rword "unit"
       <|> Char  <$ rword "char"

typeIdentifier :: Parser Text
typeIdentifier = (lexeme . try) p
  where
    p = fmap T.pack $ (:) <$> upperChar
                          <*> many alphaNumChar

inlineType :: Parser TypeExpr
inlineType = InlineType <$> typeIdentifier <*> many typeExpr'

typeExpr :: Parser TypeExpr
typeExpr = FuncType   <$> typeExpr <* symbol "->" *> typeExpr
       <|> ListType   <$> brackets typeExpr
       <|> TupleType  <$> parens (typeExpr `sepBy` comma)
       <|> StructType <$> braces (keyValPair `sepBy` comma)
       <|> SetType    <$> braces typeExpr
       <|> inlineType
       <|> TypeVar    <$> camelId
       <|> PrimType   <$> primType
         where
          camelId = fmap T.pack $ (:) <$> lowerChar <*> many alphaNumChar
          keyValPair = do
            key <- identifier <* symbol ":"
            val <- typeExpr
            return (key, val)

typeExpr' :: Parser TypeExpr
typeExpr' = inlineType <|> parens typeExpr

typeDefn :: Parser TypeDefn
typeDefn = TypeDefn <$> (rword "type" *> typeIdentifier) <*> (symbol "=" *> typeExpr)

programP :: Parser Program
programP = between sc eof $ Program <$> many typeDefn
