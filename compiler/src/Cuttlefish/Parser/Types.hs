module Cuttlefish.Parser.Types where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text ( Text )
import qualified Data.Text                  as T
import           Control.Monad ( void )
import           Cuttlefish.Ast
import           Cuttlefish.Parser.Core
import           Cuttlefish.Parser.Body

primType :: Parser' PrimType
primType sc = Int   <$ rword sc "int"
          <|> Float <$ rword sc "float"
          <|> Bool  <$ rword sc "bool"
          <|> Char  <$ rword sc "char"
          <|> Unit  <$ L.symbol sc "()"

typeIdentifier :: Parser' Text
typeIdentifier sc = (lexeme sc . try) p
  where
    p = fmap T.pack $ (:) <$> upperChar
                          <*> many alphaNumChar

typeVariable :: Parser' Text
typeVariable sc = (lexeme sc . try) p
  where
    p = fmap T.pack $ (:) <$> lowerChar
                          <*> many alphaNumChar

containedTypeExprP :: Parser TypeExpr
containedTypeExprP = ListType       <$> brackets containedTypeExprP
                <|> try (TupleType  <$> parens (containedTypeExprP `sepBy1` comma))
                <|> try (StructType <$> braces (keyValPair `sepBy` comma))
                <|> SetType         <$> braces containedTypeExprP
                <|> try (InlineType <$> typeIdentifier hsc <*> many containedTypeExprP)
                <|> try (PrimType   <$> primType fsc)
                <|> TypeVar         <$> typeVariable fsc
                <|> parens typeExprP
                where
                  keyValPair = do
                    key <- identifier hsc
                    val <- L.symbol hsc ":" *> containedTypeExprP
                    return (key, val)

typeConstraintP :: Parser TypeConstraint
typeConstraintP = TypeConstraint <$> typeIdentifier hsc <*> typeVariable fsc

typeExprP :: Parser TypeExpr
typeExprP = try (ConstraintWrap <$> ((:[]) <$> typeConstraintP <|> parens (typeConstraintP `sepBy1` comma)) <*> (L.symbol fsc "=>" *> typeExprP))
        <|> try (FuncType <$> (containedTypeExprP <* L.symbol fsc "->") <*> typeExprP)
        <|> containedTypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = TypeDefn
  <$> (rword hsc "type" *> typeIdentifier hsc)
  <*> many (typeVariable hsc)
  <*> (L.symbol fsc "=" *> typeExprP)

typeSigP :: Parser TypeSig
typeSigP = TypeSig
  <$> (identifier hsc <* L.symbol hsc "::")
  <*> typeExprP

classDefnP :: Parser ClassDefn
classDefnP = ClassDefn
  <$> (rword hsc "class" *> typeConstraintP)
  <*> braces (many typeSigP)

membershipP :: Parser MembershipDefn
membershipP = MembershipDefn
  <$> (rword hsc "member" *> typeIdentifier hsc)
  <*> (rword hsc "of" *> typeIdentifier hsc)
  <*> braces (many defnP)
