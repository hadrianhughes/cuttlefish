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

typeVarDefnP :: Parser TypeVarDefn
typeVarDefnP = try (TypeVarDefn <$> optional typeIdentifier <*> identifier)
           <|> TypeVarDefn <$> (return Nothing) <*> identifier

dataConstructorP :: Parser (Text, [TypeExpr])
dataConstructorP = pair
  <$> typeIdentifier
  <*> (maybeList <$> optional (parens hsc (openTypeExprP `sepBy1` comma))) <* fsc

closedTypeExprP :: Parser TypeExpr
closedTypeExprP = try (ListType    <$> brackets hsc openTypeExprP)
              <|> try (TupleType   <$> parens hsc (openTypeExprP `sepBy1` comma))
              <|> try (StructType  <$> braces fsc (keyValPair `sepBy` (comma <* fsc)))
              <|> try (EnumType    <$> dataConstructorP `sepBy1` pipe)
              <|> try (EffectType  <$> (rword "effect" *> angles hsc openTypeExprP))
              <|> try (GenericType <$> identifier <*> angles hsc (openTypeExprP `sepBy1` comma))
              <|> try (PrimType    <$> primTypeP)
              <|> TypeVar          <$> identifier
              where
                keyValPair = pair <$> (identifier <* colon) <*> openTypeExprP

openTypeExprP :: Parser TypeExpr
openTypeExprP = try (FuncType <$> (closedTypeExprP <* L.symbol fsc "->") <*> openTypeExprP)
            <|> closedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = parse <* fsc
  where
    parse = TypeDefn <$> (rword "type" *> typeIdentifier)
        <*> (maybeList <$> optional (angles hsc (typeVarDefnP `sepBy1` comma)))
        <*> (L.symbol fsc "=" *> openTypeExprP)
