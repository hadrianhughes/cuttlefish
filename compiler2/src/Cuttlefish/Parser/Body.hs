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

funcCallP :: Parser Expr
funcCallP = do
  first <- exprP
  calls <- some $ parens (allExprP `sepBy` comma)
  return $ foldl FuncCall first calls

listAccessP :: Parser Expr
listAccessP = do
  first <- try funcCallP <|> exprP
  indices <- some $ brackets allExprP
  return $ foldl ListAccess first indices

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
       <|> try funcCallP
       <|> try (parens ternaryP)
       <|> exprP

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> try ternaryP <|> allExprP)
