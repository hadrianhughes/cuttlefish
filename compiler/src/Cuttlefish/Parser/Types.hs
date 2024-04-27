module Cuttlefish.Parser.Types where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map                   as M
import           Data.Text (Text)
import           Data.Void
import           Cuttlefish.Parser.Ast
import           Cuttlefish.Parser.Core (Parser)
import qualified Cuttlefish.Parser.Core as P
import           Cuttlefish.Parser.Lexer
import           Cuttlefish.Parser.Utils

primTypeP :: Parser PrimType
primTypeP = Int   <$ rword "int"
        <|> Float <$ rword "float"
        <|> Char  <$ rword "char"
        <|> Unit  <$ symbol "()"

typeConstraintP :: Parser TypeConstraint
typeConstraintP = try (TypeConstraint <$> typeIdentifier <*> identifier)

dataConstructorP :: Parser (Text, [TypeExpr])
dataConstructorP = pair
  <$> typeIdentifier
  <*> (unmaybeList <$> optional (parens $ openTypeExprP `sepBy1` comma)) <* sc

closedTypeExprP :: Parser TypeExpr
closedTypeExprP = try (ListTypeExpr    <$> brackets openTypeExprP)
              <|> try (TupleTypeExpr   <$> parens (openTypeExprP `sepBy1` comma))
              <|> try ((StructTypeExpr . M.fromList) <$> braces' (keyValPair `sepBy` (comma <* sc)))
              <|> try (EffectTypeExpr  <$> (rword "effect" *> angles openTypeExprP))
              <|> try (GenericTypeExpr <$> identifier <*> angles (openTypeExprP `sepBy1` comma))
              <|> try ((EnumTypeExpr . M.fromList) <$> dataConstructorP `sepBy1` pipe)
              <|> try (PrimTypeExpr    <$> primTypeP)
              <|> PlaceholderExpr      <$> identifier
              where
                keyValPair = pair <$> (identifier <* colon) <*> openTypeExprP

openTypeExprP :: Parser TypeExpr
openTypeExprP = try (FuncTypeExpr <$> (closedTypeExprP <* P.symbol sc "->") <*> openTypeExprP)
            <|> closedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = parse <* sc
  where
    parse = TypeDefn <$> (rword "type" *> typeIdentifier)
        <*> (unmaybeList <$> optional (angles hsc (typeConstraintP `sepBy1` comma)))
        <*> (L.symbol fsc "=" *> openTypeExprP)
