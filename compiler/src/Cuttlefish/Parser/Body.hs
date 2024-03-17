module Cuttlefish.Parser.Body where

import           Data.Char
import qualified Data.Text              as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Cuttlefish.Parser.Core
import           Cuttlefish.Ast

containedExprP :: Parser Expr
containedExprP = Reference <$> identifier
             <|> parens exprP

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

statementP :: Parser Statement
statementP = endLine $
             IfStmt <$> (rword "if" *> exprP) <*> algoP <*> optional (rword "else" *> algoP)
         <|> Expr <$> exprP
