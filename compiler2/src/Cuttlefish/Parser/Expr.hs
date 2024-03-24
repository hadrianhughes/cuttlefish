module Cuttlefish.Parser.Expr where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Control.Monad ( void )
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Types

data ChainTerm = StructTerm Text
               | ListTerm   Expr
               | FuncTerm   [Expr]
               | BinaryOp   Text Expr

chainAcc :: Expr -> ChainTerm -> Expr
chainAcc e c =
  case c of
    (StructTerm field) -> StructAccess e field
    (ListTerm index)   -> ListAccess e index
    (FuncTerm args)    -> FuncCall e args
    (BinaryOp op e2)   -> FuncCall (VarRef op) [e, e2]

chainExprP :: Parser Expr
chainExprP = do
  start <- try exprP <|> parens chainExprP
  terms <- many term
  return $ foldl chainAcc start terms
  where
    term :: Parser ChainTerm
    term = StructTerm   <$> (dot *> identifier)
       <|> ListTerm     <$> brackets topLevelExprP
       <|> FuncTerm     <$> (parens $ topLevelExprP `sepBy` comma)
       <|> BinaryOp     <$> binop <*> (try chainExprP <|> exprP)

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
        nestedExpr = try chainExprP <|> exprP

matchExprP :: Parser Expr
matchExprP = MatchExpr
  <$> (rword "match" *> bindP)
  <*> braces (caseP `sepBy1` comma)
  where
    caseP = do
      bind <- bindP
      expr <- symbol "->" *> topLevelExprP
      return (bind, expr)

exprP :: Parser Expr
exprP = ListExpr  <$> brackets (topLevelExprP `sepBy` comma)
    <|> TupleExpr <$> parens (topLevelExprP `sepBy` comma)
    <|> matchExprP
    <|> parens ternaryP
    <|> VarRef    <$> identifier
    <|> literalP

topLevelExprP :: Parser Expr
topLevelExprP = try ternaryP <|> try chainExprP <|> exprP

bindP :: Parser Bind
bindP = try (TupleBind <$> parens (bindP `sepBy1` comma))
    <|> ConstructorBind
      <$> typeIdentifier
      <*> (maybeList <$> optional (parens (identifier `sepBy1` comma)))
    <|> SimpleBind <$> identifier

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> topLevelExprP)
