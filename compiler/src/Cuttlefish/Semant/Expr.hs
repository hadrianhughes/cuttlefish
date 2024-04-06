module Cuttlefish.Semant.Expr where

import Control.Monad.Except
import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Core
import Cuttlefish.Semant.Error
import Cuttlefish.Semant.Sast

checkExpr :: Expr -> Semant SExpr
checkExpr = \case
  (IntLit i)   -> pure (PrimType Int, SIntLit i)
  (CharLit c)  -> pure (PrimType Char, SCharLit c)
  (StrLit s)   -> pure (ListType $ PrimType Char, SStrLit s)
  (FloatLit f) -> pure (PrimType Float, SFloatLit f)
  UnitLit      -> pure (PrimType Unit, SUnitLit)
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
  where
    isList :: Type -> Bool
    isList = \case
      ListType _ -> True
      _          -> False
