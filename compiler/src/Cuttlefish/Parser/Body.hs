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
  arg1 <- atomicExprP <* hsc
  fn   <- binop <* hsc
  arg2 <- atomicExprP
  return $ FuncCall (Reference $ [fn]) [arg1, arg2]

atomicExprP :: Parser Expr
atomicExprP = Reference <$> (identifier hsc `sepBy1` L.symbol hsc ".")
          <|> literalP
          <|> parens (openExprP fsc)

containedExprP :: Parser' Expr
containedExprP sc = expr <* sc
             where
              expr = try (ListAccess <$> atomicExprP <*> brackets (openExprP hsc))
                 <|> try (FuncCall <$> atomicExprP <*> some atomicExprP)
                 <|> try operatorP
                 <|> atomicExprP

openExprP :: Parser' Expr
openExprP sc = try (Ternary <$> containedExprP sc <*> (L.symbol sc "?" *> containedExprP sc) <*> (L.symbol sc ":" *> containedExprP sc))
        <|> containedExprP sc

defnP :: Parser Defn
defnP = try (Defn <$> identifier hsc <*> many (identifier hsc) <*> (L.symbol hsc "=" *> openExprP fsc))
    <|> AlgoDefn <$> identifier hsc <*> many (identifier hsc) <*> algoP

algoP :: Parser Algo
algoP = Algo <$> braces (many statementP)

varBindP :: Parser Statement
varBindP = rword hsc "let" *> do
  mut   <- optional (rword hsc "mut")
  name  <- identifier hsc
  value <- L.symbol fsc "=" *> openExprP fsc
  return $ VarBind name value (isJust mut)

statementP :: Parser Statement
statementP = IfStmt  <$> (rword hsc "if" *> openExprP hsc) <*> algoP <*> optional (rword hsc "else" *> algoP)
         <|> ForLoop <$> (rword hsc "for" *> identifier hsc) <*> (rword hsc "in" *> openExprP hsc) <*> algoP
         <|> varBindP
         <|> Return  <$> (rword hsc "return" *> openExprP fsc)
         <|> Expr    <$> openExprP fsc
