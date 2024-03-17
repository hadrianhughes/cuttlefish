module Cuttlefish.Parser.Body where

import           Data.Char
import qualified Data.Text              as T
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Cuttlefish.Parser.Core
import           Cuttlefish.Ast

literalP :: Parser Expr
literalP = IntLit   <$> integer
       <|> FloatLit <$> float
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> CharLit  <$> squotes (ord <$> satisfy (`notElem` ['\\', '\'']) <|> (single '\\' >> integer))

operatorP :: Parser Expr
operatorP = do
  arg1 <- containedExprP
  fn   <- binop
  arg2 <- containedExprP
  return $ FuncCall fn [arg1, arg2]

containedExprP :: Parser Expr
containedExprP = Reference <$> identifier
             <|> literalP
             <|> parens exprP

exprP :: Parser Expr
exprP = try (FuncCall <$> identifier <*> some containedExprP)
    <|> try operatorP
    <|> containedExprP

defnP :: Parser Defn
defnP = endLine $
        try (Defn <$> identifier <*> many identifier <*> (symbol "=" *> exprP))
    <|> AlgoDefn <$> identifier <*> many identifier <*> algoP

algoP :: Parser Algo
algoP = Algo <$> braces (many statementP)

varBindP :: Parser Statement
varBindP = rword "let" *> do
  mut   <- optional (rword "mut")
  name  <- identifier
  value <- symbol "=" *> exprP
  return $ VarBind name value (isJust mut)

statementP :: Parser Statement
statementP = endLine $
             IfStmt  <$> (rword "if" *> exprP) <*> algoP <*> optional (rword "else" *> algoP)
         <|> ForLoop <$> (rword "for" *> identifier) <*> (rword "in" *> exprP) <*> algoP
         <|> varBindP
         <|> Expr    <$> exprP
