module Cuttlefish.Semant.Types where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast  as AST
import           Cuttlefish.Semant.Core
import           Cuttlefish.Semant.Error
import           Cuttlefish.Semant.Sast
import qualified Data.Map               as M

convertTypeExpr :: TypeExpr -> Semant Type
convertTypeExpr expr =
  case expr of
    (FuncTypeExpr e1 e2)     -> FuncType      <$> convertTypeExpr e1 <*> convertTypeExpr e2
    (ListTypeExpr e)         -> ListType      <$> convertTypeExpr e
    (TupleTypeExpr es)       -> TupleType     <$> traverse convertTypeExpr es
    (StructTypeExpr fields)  -> StructType    <$> traverse convertTypeExpr fields
    (EnumTypeExpr cases)     -> EnumType      <$> traverse (traverse convertTypeExpr) cases
    (EffectTypeExpr e)       -> EffectType    <$> convertTypeExpr e
    (GenericTypeExpr n args) -> GenericType n <$> traverse convertTypeExpr args
    (PrimTypeExpr p)         -> pure $ PrimType p
    (PlaceholderExpr name)   -> pure $ Placeholder name

checkTypeDefn :: TypeDefn -> Semant STypeDefn
checkTypeDefn defn = do
  -- Check for duplicate definition
  defns <- gets typeDefns
  let name = AST.typeName defn
  when (M.member name defns)
    $ throwError
    $ DuplicateDefn name DTypeDefn

  type' <- convertTypeExpr $ AST.typeExpr defn
  let defn' = STypeDefn name (AST.typeConstraints defn) type'

  modify $ \env -> env { typeDefns = M.insert name defn' defns }

  pure defn'
