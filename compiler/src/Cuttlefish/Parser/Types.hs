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

typeVariable :: Parser' Text
typeVariable sc = (lexeme sc . try) p
  where
    p = fmap T.pack $ (:) <$> lowerChar
                          <*> many alphaNumChar

dataConstructor :: Parser (Text, [TypeExpr])
dataConstructor = p <* fsc
  where
    p = do
      name <- typeIdentifier hsc
      args <- many rank1TypeExprP
      return (name, args)

rank1TypeExprP :: Parser TypeExpr
rank1TypeExprP = ListType                <$> brackets rank1TypeExprP
                <|> try (TupleType       <$> parens (rank1TypeExprP `sepBy1` comma))
                <|> try (StructType      <$> braces (keyValPair `sepBy` comma))
                <|> SetType              <$> braces rank1TypeExprP
                <|> try (TypeConstructor <$> dataConstructor `sepBy1` (L.symbol hsc "|"))
                <|> try (PrimType        <$> primType fsc)
                <|> TypeVar              <$> typeVariable fsc
                <|> parens rank2TypeExprP
                where
                  keyValPair = do
                    key <- identifier hsc
                    val <- L.symbol hsc ":" *> rank1TypeExprP
                    return (key, val)

typeConstraintP :: Parser TypeConstraint
typeConstraintP = TypeConstraint <$> typeIdentifier hsc <*> typeVariable fsc

rank2TypeExprP :: Parser TypeExpr
rank2TypeExprP = try (ConstraintWrap <$> ((:[]) <$> typeConstraintP <|> parens (typeConstraintP `sepBy1` comma)) <*> (L.symbol fsc "=>" *> rank2TypeExprP))
        <|> try (FuncType <$> (rank1TypeExprP <* L.symbol fsc "->") <*> rank2TypeExprP)
        <|> rank1TypeExprP

typeDefnP :: Parser TypeDefn
typeDefnP = TypeDefn
  <$> (rword hsc "type" *> typeIdentifier hsc)
  <*> many (typeVariable hsc)
  <*> (L.symbol fsc "=" *> rank2TypeExprP)

typeSigP :: Parser TypeSig
typeSigP = TypeSig
  <$> (identifier hsc <* L.symbol hsc "::")
  <*> rank2TypeExprP

classDefnP :: Parser ClassDefn
classDefnP = ClassDefn
  <$> (rword hsc "class" *> typeConstraintP)
  <*> braces (many typeSigP)

membershipP :: Parser MembershipDefn
membershipP = MembershipDefn
  <$> (rword hsc "member" *> typeIdentifier hsc)
  <*> (rword hsc "of" *> typeIdentifier hsc)
  <*> braces (many defnP)
