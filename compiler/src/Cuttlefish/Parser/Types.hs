module Cuttlefish.Parser.Types where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map                   as M
import           Data.Text (Text)
import           Data.Void
import           Cuttlefish.Parser.Ast
import           Cuttlefish.Parser.Core
import           Cuttlefish.Parser.Utils

primTypeP :: Parser PrimType
primTypeP = Int   <$ rword "int"
        <|> Float <$ rword "float"
        <|> Char  <$ rword "char"
        <|> Unit  <$ L.symbol hsc "()"

typeConstraintP :: Parser TypeConstraint
typeConstraintP = try (TypeConstraint <$> typeIdentifier <*> identifier)

dataConstructorP :: Parser (Text, [TypeExpr])
dataConstructorP = pair
  <$> typeIdentifier
  <*> (unmaybeList <$> optional (parens hsc $ openTypeExprP `sepBy1` comma)) <* fsc

closedTypeExprP :: Parser TypeExpr
closedTypeExprP = try (ListTypeExpr    <$> brackets hsc openTypeExprP)
              <|> try (TupleTypeExpr   <$> parens hsc (openTypeExprP `sepBy1` comma))
              <|> try ((StructTypeExpr . M.fromList) <$> braces fsc (keyValPair `sepBy` (comma <* fsc)))
              <|> try (EffectTypeExpr  <$> (rword "effect" *> angles hsc openTypeExprP))
              <|> try (GenericTypeExpr <$> identifier <*> angles hsc (openTypeExprP `sepBy1` comma))
              <|> try ((EnumTypeExpr . M.fromList) <$> dataConstructorP `sepBy1` pipe)
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
        <*> (unmaybeList <$> optional (angles hsc (typeConstraintP `sepBy1` comma)))
        <*> (L.symbol fsc "=" *> openTypeExprP)
