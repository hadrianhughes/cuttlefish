module Cuttlefish.Parser.Body where

import           Data.Char
import qualified Data.Text              as T
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Cuttlefish.Parser.Core
import           Cuttlefish.Ast

literalP :: Parser Expr
literalP = IntLit   <$> integer
       <|> FloatLit <$> float
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> CharLit  <$> squotes (ord <$> satisfy (`notElem` ['\\', '\'']) <|> (single '\\' >> integer))

operatorP :: Parser Expr
operatorP = do
  arg1 <- rank1ExprP <* hsc
  fn   <- binop <* hsc
  arg2 <- rank1ExprP
  return $ FuncCall (Reference $ [fn]) [arg1, arg2]

rank1ExprP :: Parser Expr
rank1ExprP = Reference <$> identifier hsc `sepBy1` L.symbol hsc "."
         <|> ListExpr  <$> brackets (rank3ExprP fsc `sepBy` comma)
         <|> TupleExpr <$> parens (rank3ExprP fsc `sepBy1` comma)
         <|> literalP
         <|> parens (rank3ExprP fsc)

rank2ExprP :: Parser' Expr
rank2ExprP sc = expr <* sc
  where
    expr = try (DConstructorExpr <$> typeIdentifier hsc <*> many rank1ExprP)
       <|> try (ListAccess       <$> rank1ExprP <*> brackets (rank3ExprP hsc))
       <|> try (FuncCall         <$> rank1ExprP <*> some rank1ExprP)
       <|> try operatorP
       <|> rank1ExprP

rank3ExprP :: Parser' Expr
rank3ExprP sc = try (TernaryExpr <$> rank2ExprP sc <*> (L.symbol sc "?" *> rank2ExprP sc) <*> (L.symbol sc ":" *> rank2ExprP sc))
            <|> rank2ExprP sc

rank1BindP :: Parser Bind
rank1BindP = ListBind    <$> brackets (rank2BindP `sepBy` comma)
    <|> try (TupleBind   <$> parens (rank2BindP `sepBy1` comma))
    <|> SimpleBind       <$> identifier hsc
    <|> parens rank2BindP

rank2BindP :: Parser Bind
rank2BindP = try (DConstructorBind <$> typeIdentifier hsc <*> many (identifier hsc))
         <|> rank1BindP

defnP :: Parser Defn
defnP = try (Defn <$> identifier hsc <*> many (identifier hsc) <*> (L.symbol hsc "=" *> rank3ExprP fsc))
    <|> AlgoDefn <$> identifier hsc <*> many (identifier hsc) <*> algoP

algoP :: Parser Algo
algoP = Algo <$> braces (many statementP)

varBindP :: Parser Statement
varBindP = rword hsc "let" *> do
  mut   <- optional (rword hsc "mut")
  name  <- identifier hsc
  value <- L.symbol fsc "=" *> rank3ExprP fsc
  return $ VarDecl name value (isJust mut)

statementP :: Parser Statement
statementP = IfStmt           <$> (rword hsc "if" *> rank3ExprP hsc) <*> algoP <*> optional (rword hsc "else" *> algoP)
         <|> ForLoop          <$> (rword hsc "for" *> rank1BindP) <*> (rword hsc "in" *> rank3ExprP hsc) <*> algoP
         <|> varBindP
         <|> Return           <$> (rword hsc "return" *> rank3ExprP fsc)
         <|> try (Destructure <$> rank2BindP <*> (L.symbol fsc "=" *> rank3ExprP fsc))
         <|> Expr             <$> rank3ExprP fsc
