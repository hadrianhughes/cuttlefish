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
  arg1 <- atomicExprP
  fn   <- binop
  arg2 <- atomicExprP
  return $ FuncCall fn [arg1, arg2]

atomicExprP :: Parser Expr
atomicExprP = Reference <$> identifier
          <|> literalP
          <|> parens openExprP

containedExprP :: Parser Expr
containedExprP = try (FuncCall <$> identifier <*> some containedExprP)
             <|> try operatorP
             <|> atomicExprP

openExprP :: Parser Expr
openExprP = try (Ternary <$> containedExprP <*> (symbol "?" *> containedExprP) <*> (symbol ":" *> containedExprP))
       <|> containedExprP

defnP :: Parser Defn
defnP = endLine $
        try (Defn <$> identifier <*> many identifier <*> (symbol "=" *> openExprP))
    <|> AlgoDefn <$> identifier <*> many identifier <*> algoP

algoP :: Parser Algo
algoP = Algo <$> braces (many statementP)

varBindP :: Parser Statement
varBindP = rword "let" *> do
  mut   <- optional (rword "mut")
  name  <- identifier
  value <- symbol "=" *> openExprP
  return $ VarBind name value (isJust mut)

statementP :: Parser Statement
statementP = endLine $
             IfStmt  <$> (rword "if" *> openExprP) <*> algoP <*> optional (rword "else" *> algoP)
         <|> ForLoop <$> (rword "for" *> identifier) <*> (rword "in" *> openExprP) <*> algoP
         <|> varBindP
         <|> Return  <$> (rword "return" *> openExprP)
         <|> Expr    <$> openExprP
