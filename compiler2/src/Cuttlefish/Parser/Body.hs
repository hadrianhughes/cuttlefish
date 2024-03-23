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
  first <- parens listAccessP <|> exprP
  calls <- some $ parens (allExprP `sepBy` comma)
  return $ foldl FuncCall first calls

listAccessP :: Parser Expr
listAccessP = do
  first <- try funcCallP <|> try structAccessP <|> exprP
  indices <- some $ brackets allExprP
  return $ foldl ListAccess first indices

structAccessP :: Parser Expr
structAccessP = do
  first <- try funcCallP <|> parens listAccessP <|> exprP
  accessors <- some $ dot *> identifier
  return $ foldl StructAccess first accessors

methodCallP :: Parser Expr
methodCallP = do
  first <- try funcCallP <|> parens listAccessP <|> exprP
  accessors <- some $ dot *> identifier
  let field = foldl StructAccess first accessors

  calls <- some $ parens (allExprP `sepBy` comma)
  return $ foldl FuncCall field calls

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
