module Cuttlefish.Parser.Types where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Cuttlefish.Ast
import Cuttlefish.Parser.Core

primTypeP :: Parser PrimType
primTypeP = Int   <$ rword "int"
        <|> Float <$ rword "float"
        <|> Char  <$ rword "char"
        <|> Unit  <$ symbol "()"

typeVarDefnP :: Parser TypeVarDefn
typeVarDefnP = TypeVarDefn <$> optional typeIdentifier <*> identifier

dataConstructorP :: Parser (Text, [TypeExpr])
dataConstructorP = do
  name <- typeIdentifier
  args <- maybeList <$> optional (parens $ openTypeExprP `sepBy1` comma)
  return (name, args)

closedTypeExprP :: Parser TypeExpr
closedTypeExprP = ListType         <$> brackets openTypeExprP
              <|> TupleType        <$> parens (openTypeExprP `sepBy1` comma)
              <|> try (StructType  <$> braces (keyValPair `sepBy` comma))
              <|> SetType          <$> braces openTypeExprP
              <|> try (Constructor <$> dataConstructorP `sepBy1` pipe)
              <|> PrimType         <$> primTypeP
              <|> TypeVar          <$> identifier
              where
                keyValPair = do
                  key <- (identifier <* colon)
                  val <- openTypeExprP
                  return (key, val)

openTypeExprP :: Parser TypeExpr
openTypeExprP = try (FuncType <$> (closedTypeExprP <* symbol "->") <*> openTypeExprP)
            <|> closedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = TypeDefn <$> (rword "type" *> typeIdentifier)
                     <*> (maybeList <$> optional (angles (typeVarDefnP `sepBy1` comma)))
                     <*> (symbol "=" *> openTypeExprP)
