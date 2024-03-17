module Cuttlefish.Parser.Types where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text ( Text )
import qualified Data.Text                  as T
import           Control.Monad ( void )
import           Cuttlefish.Ast
import           Cuttlefish.Parser.Core

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

typeConstraintP :: Parser TypeConstraint
typeConstraintP = TypeConstraint <$> typeIdentifier <*> typeVariable

typeExprP :: Parser TypeExpr
typeExprP = try (ConstraintWrap <$> ((:[]) <$> typeConstraintP <|> parens (typeConstraintP `sepBy1` comma)) <*> (symbol "=>" *> typeExprP))
        <|> try (FuncType <$> (containedTypeExprP <* symbol "->") <*> containedTypeExprP)
        <|> containedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = TypeDefn
  <$> (rword "type" *> typeIdentifier)
  <*> many typeVariable
  <*> (symbol "=" *> typeExprP)

typeSigP :: Parser TypeSig
typeSigP = TypeSig <$> (identifier <* symbol "::") <*> typeExprP
