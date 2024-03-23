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
  (try structAccessP <|> try funcCallP <|> exprP)
  (brackets allExprP)
  ListAccess

structAccessP = chain
  (try funcCallP <|> parens listAccessP <|> exprP)
  (dot *> identifier)
  StructAccess

methodCallP = chain
  (try itemFieldAccess <|> structAccessP)
  (parens $ allExprP `sepBy` comma)
  FuncCall

itemFieldAccess = chain
  listAccessP
  (dot *> identifier)
  StructAccess

itemFuncCall = chain
  listAccessP
  (parens $ allExprP `sepBy` comma)
  FuncCall

ternaryP :: Parser Expr
ternaryP = TernaryExpr
       <$> nestedExpr
       <*> (symbol "?" *> nestedExpr)
       <*> (symbol ":" *> nestedExpr)
       where
        nestedExpr = try itemFuncCall
                 <|> try funcCallP
                 <|> try methodCallP
                 <|> try itemFieldAccess
                 <|> try listAccessP
                 <|> try structAccessP
                 <|> parens ternaryP
                 <|> exprP

exprP :: Parser Expr
exprP = ListExpr  <$> brackets (exprP `sepBy` comma)
    <|> TupleExpr <$> parens (exprP `sepBy` comma)
    <|> VarRef    <$> identifier
    <|> literalP

allExprP :: Parser Expr
allExprP = try itemFuncCall
       <|> try methodCallP
       <|> try itemFieldAccess
       <|> try listAccessP
       <|> try structAccessP
       <|> try funcCallP
       <|> try ternaryP
       <|> exprP

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> (try ternaryP <|> allExprP))
