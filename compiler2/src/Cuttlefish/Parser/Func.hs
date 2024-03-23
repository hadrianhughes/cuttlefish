module Cuttlefish.Parser.Func where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Expr
import Cuttlefish.Parser.Types

argP :: Parser (Text, TypeExpr)
argP = do
  name <- identifier
  typ  <- colon *> openTypeExprP
  return (name, typ)

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
    (map (\(name, _) -> SimpleBind name) args)
    body
  where
    argFold :: (Text, TypeExpr) -> TypeExpr -> TypeExpr
    argFold = \(_, t1) t2 -> FuncType t1 t2

funcDefnP' :: Parser FuncDefn
funcDefnP' = do
  (name, funcType) <- rword "func" *> parens argP
  typeVars         <- optional (angles $ some typeVarDefnP)
  args             <- parens (identifier `sepBy` comma)
  body             <- symbol "=" *> (try chainExprP <|> exprP)

  let typeVars' = maybeList typeVars

  return $ FuncDefn
    name
    funcType
    typeVars'
    (map SimpleBind args)
    body
