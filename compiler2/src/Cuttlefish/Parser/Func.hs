module Cuttlefish.Parser.Func where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Expr
import Cuttlefish.Parser.Types

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
    (map fst args)
    body
  where
    argFold :: (a, TypeExpr) -> TypeExpr -> TypeExpr
    argFold = \(_, t1) t2 -> FuncType t1 t2
    argP = do
      name <- bindP
      typ  <- colon *> openTypeExprP
      return (name, typ)

funcDefnP' :: Parser FuncDefn
funcDefnP' = do
  (name, funcType) <- rword "func" *> parens nameTypeP
  typeVars         <- optional (angles $ some typeVarDefnP)
  args             <- parens (bindP `sepBy` comma)
  body             <- symbol "=" *> (try chainExprP <|> exprP)

  let typeVars' = maybeList typeVars

  return $ FuncDefn
    name
    funcType
    typeVars'
    args
    body
  where
    nameTypeP = do
      name <- identifier
      typ  <- colon *> openTypeExprP
      return (name, typ)
