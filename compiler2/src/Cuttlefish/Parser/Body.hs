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
  calls <- some $ parens ((try listAccessP <|> try funcCallP <|> exprP) `sepBy` comma)
  return $ foldl FuncCall first calls

listAccessP :: Parser Expr
listAccessP = do
  first <- try funcCallP <|> exprP
  indices <- some $ brackets (try listAccessP <|> try funcCallP <|> exprP)
  return $ foldl ListAccess first indices

exprP :: Parser Expr
exprP = ListExpr  <$> brackets (exprP `sepBy` comma)
    <|> TupleExpr <$> parens (exprP `sepBy` comma)
    <|> VarRef    <$> identifier
    <|> literalP

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> exprP')
  where
    exprP' = try listAccessP <|> try funcCallP <|> exprP
