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

typeVariable :: Parser Text
typeVariable = (lexeme . try) p
  where
    p = fmap T.pack $ (:) <$> lowerChar
                          <*> many alphaNumChar

containedTypeExprP :: Parser TypeExpr
containedTypeExprP = ListType       <$> brackets containedTypeExprP
                <|> try (TupleType  <$> parens (containedTypeExprP `sepBy` comma))
                <|> try (StructType <$> braces (keyValPair `sepBy` comma))
                <|> SetType         <$> braces containedTypeExprP
                <|> try (InlineType <$> typeIdentifier <*> many containedTypeExprP)
                <|> try (PrimType   <$> primType)
                <|> TypeVar         <$> typeVariable
                <|> parens typeExprP
                where
                  keyValPair = do
                    key <- identifier
                    val <- symbol ":" *> containedTypeExprP
                    return (key, val)

typeExprP :: Parser TypeExpr
typeExprP = try (FuncType <$> (containedTypeExprP <* symbol "->") <*> containedTypeExprP)
       <|> containedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = TypeDefn
  <$> (rword "type" *> typeIdentifier)
  <*> many typeVariable
  <*> (symbol "=" *> typeExprP)

programP :: Parser Program
programP = between sc eof $ Program <$> many typeDefnP
