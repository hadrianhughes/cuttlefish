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
literalP = FloatLit <$> float
       <|> IntLit   <$> int
       <|> CharLit  <$> squotes (satisfy (`notElem` ['\\', '\'']) <|> single '\\')
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> (symbol "()" *> return UnitLit)

rank1ExprP :: Parser Expr
rank1ExprP = literalP
          <|> VarRef    <$> identifier
          <|> ListExpr  <$> brackets (rank2ExprP `sepBy` comma)
          <|> TupleExpr <$> parens (rank2ExprP `sepBy` comma)
          <|> parens rank2ExprP

rank2ExprP :: Parser Expr
rank2ExprP = try (FuncCall <$> rank2ExprP <*> parens (rank2ExprP `sepBy` comma))
         <|> try (StructAccess <$> rank2ExprP <*> (dot *> identifier))
         <|> parens rank3ExprP

rank3ExprP :: Parser Expr
rank3ExprP = TernaryExpr <$> rank2ExprP <*> (symbol "?" *> rank2ExprP) <*> (symbol ":" *> rank2ExprP)

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> rank3ExprP)
