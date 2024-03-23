module Cuttlefish.Parser.Body where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Control.Monad ( void )
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Types

literalP :: Parser Expr
literalP = try (FloatLit <$> float)
       <|> IntLit   <$> int
       <|> CharLit  <$> squotes (satisfy (`notElem` ['\\', '\'']) <|> single '\\')
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> (symbol "()" *> return UnitLit)

funcCallP = chain
  (parens listAccessP <|> exprP)
  (parens $ allExprP `sepBy` comma)
  FuncCall

listAccessP = chain
  (try funcCallP <|> try structAccessP <|> exprP)
  (brackets allExprP)
  ListAccess

structAccessP = chain
  (try funcCallP <|> parens listAccessP <|> exprP)
  (dot *> identifier)
  StructAccess

methodCallP = chain
  structAccessP
  (parens (allExprP `sepBy` comma))
  FuncCall

ternaryP :: Parser Expr
ternaryP = TernaryExpr
       <$> allExprP
       <*> (symbol "?" *> allExprP)
       <*> (symbol ":" *> allExprP)

exprP :: Parser Expr
exprP = ListExpr  <$> brackets (exprP `sepBy` comma)
    <|> TupleExpr <$> parens (exprP `sepBy` comma)
    <|> VarRef    <$> identifier
    <|> literalP

allExprP :: Parser Expr
allExprP = try listAccessP
       <|> try methodCallP
       <|> try structAccessP
       <|> try funcCallP
       <|> try (parens ternaryP)
       <|> exprP

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> (try ternaryP <|> allExprP))
