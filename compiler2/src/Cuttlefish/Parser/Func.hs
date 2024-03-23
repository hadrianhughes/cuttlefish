module Cuttlefish.Parser.Func where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Expr
import Cuttlefish.Parser.Types

data InlineArg = InlineArg Text TypeExpr

funcDefnP :: Parser FuncDefn
funcDefnP = do
  name     <- rword "func" *> identifier
  typeVars <- optional (angles $ some typeVarDefnP)
  args     <- parens $ argP `sepBy` comma
  rtnType  <- symbol "->" *> openTypeExprP
  body     <- symbol "=" *> (try chainExprP <|> exprP)

  let funcType  = foldr argFold rtnType args
      typeVars' = maybeList typeVars

  return $ FuncDefn
    name
    funcType
    typeVars'
    (map (\(InlineArg name _) -> SimpleBind name) args)
    body
  where
    argP :: Parser InlineArg
    argP = InlineArg <$> identifier <*> (colon *> openTypeExprP)
    argFold :: InlineArg -> TypeExpr -> TypeExpr
    argFold = \(InlineArg _ t1) t2 -> FuncType t1 t2
