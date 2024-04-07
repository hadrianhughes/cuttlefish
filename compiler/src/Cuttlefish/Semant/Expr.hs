module Cuttlefish.Semant.Expr where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Core
import Cuttlefish.Semant.Error
import Cuttlefish.Semant.Sast
import Cuttlefish.Semant.Types
import Cuttlefish.Semant.Utils
import Cuttlefish.Utils
import                         qualified Data.Map as M
import Data.Maybe

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
          Just t  -> pure (t, SStructAccess struct' field)
          Nothing -> throwError $ UndefinedField struct' field
      _ -> throwError $ TypeError (StructType M.empty) strType strExpr
  IfExpr conds elseExpr -> do
    conds' <- traverse checkIfCond $ M.toList conds
    -- TODO: Check all branches return the right type

    elseExpr' <- checkExpr elseExpr
    -- TODO: Replace unit with correct type
    pure (PrimType Unit, SIfExpr (M.fromList conds') elseExpr')
  FuncCall fn args -> do
    fn'@(fnType, fnExpr) <- checkExpr fn
    case fnType of
      t@(FuncType a b) -> do
        let (argTypes, rtnType) = flatFuncType t
        when (length args /= length argTypes)
          $ throwError
          $ IncorrectArity (ArityFuncCall fn')

        args' <- traverse checkExpr args
        traverse (\(t, e) -> assertType t e) $ zip argTypes args'

        pure (rtnType, SFuncCall fn' args')
      -- TODO: Implement correct function type in error
      _ -> throwError $ TypeError (PrimType Unit) fnType fnExpr
  EffectRun expr -> do
    expr'@(exprType, exprVal) <- checkExpr expr
    case exprType of
      EffectType rtnType -> do
        -- TODO: Check effect rtnType is correct
        pure expr'
      -- TODO: Implement correct effect type in error
      _ -> throwError $ TypeError (EffectType $ PrimType Unit) exprType exprVal
  ListExpr elmnts -> do
    elmnts' <- traverse checkExpr elmnts
    -- TODO: Check element types match list type and return correct type
    pure (ListType $ PrimType Unit, SListExpr elmnts')
  TupleExpr items -> do
    items' <- traverse checkExpr items
    -- TODO: Check item types match tuple type and return correct type
    pure (TupleType [], STupleExpr items')
  StructExpr props -> do
    props' <- traverse checkExpr props
    -- TODO: Check struct properties match type
    pure (StructType M.empty, SStructExpr props')
  expr@(MatchExpr e cases) -> do
    expr' <- checkExpr e
    let asserts =
          case expr' of
            (TupleType _, _) -> \(xs, x) -> traverse (assertTupleBind expr') xs >> traverse (assertSimpleBind expr') (maybeToList x)
            (EnumType _, _)  -> \(xs, x) -> traverse (assertConstructorBind expr') xs >> traverse (assertSimpleBind expr') (maybeToList x)
            _                -> \_       -> pure []

    asserts (splitLast $ M.keys cases)
    cases' <- traverse checkExpr cases

    pure (PrimType Unit, SMatchExpr expr' cases')
  expr@(VarRef name) -> do
    localVars <- gets localVars
    case M.lookup name localVars of
      Nothing -> throwError $ UndefinedVar name expr
      Just (var@(varType, _), _) -> do
        -- TODO: Check var has the correct type
        pure $ (varType, SVarRef name)
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
    assertTupleBind :: SExpr -> Bind -> Semant ()
    assertTupleBind s = \case
      TupleBind _ -> pure ()
      bind        -> throwError $ PatternMismatch bind s
    assertConstructorBind :: SExpr -> Bind -> Semant ()
    assertConstructorBind s = \case
      ConstructorBind _ _ -> pure ()
      bind                -> throwError $ PatternMismatch bind s
    assertSimpleBind :: SExpr -> Bind -> Semant ()
    assertSimpleBind s = \case
      SimpleBind _ -> pure ()
      bind         -> throwError $ PatternMismatch bind s

assertType :: Type -> SExpr -> Semant ()
assertType t (et, expr) =
  if t /= et
  then throwError $ TypeError t et expr
  else pure ()
