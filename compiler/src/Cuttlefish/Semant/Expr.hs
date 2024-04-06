module Cuttlefish.Semant.Expr where

import Control.Monad.Except
import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Core
import Cuttlefish.Semant.Error
import Cuttlefish.Semant.Sast
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
    -- TODO: Solution for correct list type in error
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
    -- TODO: Solution for correct struct type in error
    case strType of
      StructType st ->
        case M.lookup field st of
          Just t  -> return (t, SStructAccess struct' field)
          Nothing -> throwError $ UndefinedField struct' field
      _ -> throwError $ TypeError (StructType M.empty) strType strExpr
  where
    isList :: Type -> Bool
    isList = \case
      ListType _ -> True
      _          -> False
