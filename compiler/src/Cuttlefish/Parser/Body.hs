module Cuttlefish.Parser.Body where

import Text.Megaparsec
import Text.Megaparsec.Char
import Cuttlefish.Parser.Core
import Cuttlefish.Ast

containedExprP :: Parser Expr
containedExprP = Reference <$> identifier
             <|> parens exprP

exprP :: Parser Expr
exprP = try (FuncCall <$> identifier <*> some containedExprP)
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
