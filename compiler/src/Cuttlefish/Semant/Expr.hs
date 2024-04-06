module Cuttlefish.Semant.Expr where

import Control.Monad.Except
import Control.Monad.State
import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Core
import Cuttlefish.Semant.Error
import Cuttlefish.Semant.Sast
import Cuttlefish.Semant.Types
import Cuttlefish.Semant.Utils
import                         qualified Data.Map as M

checkExpr :: Expr -> Semant SExpr
checkExpr = \case
  IntLit i   -> pure (PrimType Int, SIntLit i)
  CharLit c  -> pure (PrimType Char, SCharLit c)
  StrLit s   -> pure (ListType $ PrimType Char, SStrLit s)
  FloatLit f -> pure (PrimType Float, SFloatLit f)
  UnitLit    -> pure (PrimType Unit, SUnitLit)
  ListAccess list idx -> do
    list'@(listType, listExpr)     <- checkExpr list
    -- TODO: Implement correct list type in error
    unless (isList listType)
      $ throwError
      $ TypeError (ListType $ PrimType Unit) listType listExpr

    idx'@(idxType, idxExpr) <- checkExpr idx
    unless (idxType == PrimType Int)
      $ throwError
      $ TypeError (PrimType Int) idxType idxExpr

    pure (listType, SListAccess list' idx')
  StructAccess struct field -> do
    struct'@(strType, strExpr) <- checkExpr struct
    -- TODO: Implement correct struct type in error
    case strType of
      StructType st ->
        case M.lookup field st of
          Just t  -> return (t, SStructAccess struct' field)
          Nothing -> throwError $ UndefinedField struct' field
      _ -> throwError $ TypeError (StructType M.empty) strType strExpr
  IfExpr conds elseExpr -> do
    conds' <- mapM checkIfCond $ M.toList conds
    -- TODO: Check all branches return the right type

    elseExpr' <- checkExpr elseExpr
    -- TODO: Replace unit with correct type
    pure (PrimType Unit, SIfExpr (M.fromList conds') elseExpr')
  where
    checkIfCond :: (Expr, Expr) -> Semant (SExpr, SExpr)
    checkIfCond (cond, expr) = do
      typeDefns <- gets typeDefns
      case M.lookup "Bool" typeDefns of
        Nothing -> error "Bool type does not exist in the env."
        Just (STypeDefn _ _ bool) -> do
          cond'@(condType, condExpr) <- checkExpr cond
          unless (condType == bool)
            $ throwError
            $ TypeError bool condType condExpr

          -- TODO: Figure out correct expected type
          expr' <- checkExpr expr
          pure (cond', expr')
