module Cuttlefish.Parser.Types where

import           Control.Applicative
import           Control.Applicative.Combinators
import qualified Data.Map                   as M
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
typeConstraintP = TypeConstraint <$> typeIdentifier <*> identifier

dataConstructorP :: Parser (String, [TypeExpr])
dataConstructorP = pair
  <$> typeIdentifier
  <*> (unmaybeList <$> optional (parens $ openTypeExprP `sepBy1` comma)) <* sc

closedTypeExprP :: Parser TypeExpr
closedTypeExprP = ListTypeExpr    <$> brackets openTypeExprP
              <|> TupleTypeExpr   <$> parens (openTypeExprP `sepBy1` comma)
              <|> (StructTypeExpr . M.fromList) <$> braces' (keyValPair `sepBy` (comma <* sc))
              <|> EffectTypeExpr  <$> (rword "effect" *> angles openTypeExprP)
              <|> GenericTypeExpr <$> identifier <*> angles (openTypeExprP `sepBy1` comma)
              <|> (EnumTypeExpr . M.fromList <$> dataConstructorP `sepBy1` pipe)
              <|> PrimTypeExpr    <$> primTypeP
              <|> PlaceholderExpr      <$> identifier
              where
                keyValPair = pair <$> (identifier <* colon) <*> openTypeExprP

openTypeExprP :: Parser TypeExpr
openTypeExprP = FuncTypeExpr <$> (closedTypeExprP <* P.symbol sc "->") <*> openTypeExprP
            <|> closedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = parse <* sc
  where
    parse = TypeDefn <$> (rword "type" *> typeIdentifier)
        <*> (unmaybeList <$> optional (angles (typeConstraintP `sepBy1` comma)))
        <*> (P.symbol sc "=" *> openTypeExprP)
