module Cuttlefish.Parser.Types where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import           Cuttlefish.Parser.Ast
import           Cuttlefish.Parser.Core

primTypeP :: Parser PrimType
primTypeP = Int   <$ rword "int"
        <|> Float <$ rword "float"
        <|> Char  <$ rword "char"
        <|> Unit  <$ L.symbol hsc "()"

typeVarP :: Parser TypeVar
typeVarP = try (TypeVar <$> optional typeIdentifier <*> identifier)
           <|> TypeVar <$> (return Nothing) <*> identifier

dataConstructorP :: Parser (Text, [TypeExpr])
dataConstructorP = pair
  <$> typeIdentifier
  <*> (maybeList <$> optional (parens hsc $ openTypeExprP `sepBy1` comma)) <* fsc

closedTypeExprP :: Parser TypeExpr
closedTypeExprP = try (ListTypeExpr    <$> brackets hsc openTypeExprP)
              <|> try (TupleTypeExpr   <$> parens hsc (openTypeExprP `sepBy1` comma))
              <|> try (StructTypeExpr  <$> braces fsc (keyValPair `sepBy` (comma <* fsc)))
              <|> try (EffectTypeExpr  <$> (rword "effect" *> angles hsc openTypeExprP))
              <|> try (GenericTypeExpr <$> identifier <*> angles hsc (openTypeExprP `sepBy1` comma))
              <|> try (EnumTypeExpr    <$> dataConstructorP `sepBy1` pipe)
              <|> try (PrimTypeExpr    <$> primTypeP)
              <|> PlaceholderExpr      <$> identifier
              where
                keyValPair = pair <$> (identifier <* colon) <*> openTypeExprP

openTypeExprP :: Parser TypeExpr
openTypeExprP = try (FuncTypeExpr <$> (closedTypeExprP <* L.symbol fsc "->") <*> openTypeExprP)
            <|> closedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = parse <* fsc
  where
    parse = TypeDefn <$> (rword "type" *> typeIdentifier)
        <*> (maybeList <$> optional (angles hsc (typeVarP `sepBy1` comma)))
        <*> (L.symbol fsc "=" *> openTypeExprP)
