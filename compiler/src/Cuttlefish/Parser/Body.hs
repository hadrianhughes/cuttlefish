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
         <|> List <$> brackets (rank3ExprP fsc `sepBy` comma)
         <|> Tuple <$> parens (rank3ExprP fsc `sepBy1` comma)
         <|> literalP
         <|> parens (rank3ExprP fsc)

rank2ExprP :: Parser' Expr
rank2ExprP sc = expr <* sc
  where
    expr = try (DataConstructor <$> typeIdentifier hsc <*> many rank1ExprP)
       <|> try (ListAccess <$> rank1ExprP <*> brackets (rank3ExprP hsc))
       <|> try (FuncCall <$> rank1ExprP <*> some rank1ExprP)
       <|> try operatorP
       <|> rank1ExprP

rank3ExprP :: Parser' Expr
rank3ExprP sc = try (Ternary <$> rank2ExprP sc <*> (L.symbol sc "?" *> rank2ExprP sc) <*> (L.symbol sc ":" *> rank2ExprP sc))
            <|> rank2ExprP sc

bindExprP :: Parser Expr
bindExprP = List                    <$> brackets (bindExprP `sepBy` comma)
        <|> try    (Tuple           <$> parens (bindExprP `sepBy1` comma))
        <|> parens (DataConstructor <$> typeIdentifier hsc <*> many bindExprP)
        <|> Reference               <$> identifier hsc `sepBy1` L.symbol hsc "."

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
  return $ VarBind name value (isJust mut)

statementP :: Parser Statement
statementP = IfStmt  <$> (rword hsc "if" *> rank3ExprP hsc) <*> algoP <*> optional (rword hsc "else" *> algoP)
         <|> ForLoop <$> (rword hsc "for" *> bindExprP) <*> (rword hsc "in" *> rank3ExprP hsc) <*> algoP
         <|> varBindP
         <|> Return  <$> (rword hsc "return" *> rank3ExprP fsc)
         <|> Expr    <$> rank3ExprP fsc
