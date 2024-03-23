module Cuttlefish.Parser.Expr where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Control.Monad ( void )
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Types

chainAcc :: Expr -> ChainTerm -> Expr
chainAcc e c =
  case c of
    (StructTerm field) -> StructAccess e field
    (ListTerm index)   -> ListAccess e index
    (FuncTerm args)    -> FuncCall e args

chainExprP :: Parser Expr
chainExprP = do
  start <- try exprP <|> parens chainExprP
  terms <- many term
  return $ foldl chainAcc start terms
  where
    term :: Parser ChainTerm
    term = StructTerm   <$> (dot *> identifier)
       <|> ListTerm     <$> brackets (chainExprP <|> exprP)
       <|> FuncTerm     <$> (parens $ (chainExprP <|> exprP) `sepBy` comma)

literalP :: Parser Expr
literalP = try (FloatLit <$> float)
       <|> IntLit   <$> int
       <|> CharLit  <$> squotes (satisfy (`notElem` ['\\', '\'']) <|> single '\\')
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> (symbol "()" *> return UnitLit)

ternaryP :: Parser Expr
ternaryP = TernaryExpr
       <$> nestedExpr
       <*> (symbol "?" *> nestedExpr)
       <*> (symbol ":" *> nestedExpr)
       where
        nestedExpr = chainExprP <|> exprP

exprP :: Parser Expr
exprP = ListExpr  <$> brackets (exprP `sepBy` comma)
    <|> TupleExpr <$> parens (exprP `sepBy` comma)
    <|> VarRef    <$> identifier
    <|> literalP

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> (try ternaryP <|> chainExprP <|> exprP))