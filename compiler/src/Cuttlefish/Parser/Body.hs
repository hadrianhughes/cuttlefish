module Cuttlefish.Parser.Body where

import Text.Megaparsec
import Cuttlefish.Parser.Core
import Cuttlefish.Ast

containedExprP :: Parser Expr
containedExprP = Reference <$> identifier
             <|> parens exprP

exprP :: Parser Expr
exprP = FuncCall <$> identifier <*> many containedExprP
    <|> containedExprP

defnP :: Parser Defn
defnP = Defn
  <$> identifier
  <*> many identifier
  <*> (symbol "=" *> exprP)
