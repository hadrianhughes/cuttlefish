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
    (FuncTypeExpr e1 e2)     -> FuncType      <$> (convertTypeExpr e1) <*> (convertTypeExpr e2)
    (ListTypeExpr e)         -> ListType      <$> (convertTypeExpr e)
    (TupleTypeExpr es)       -> TupleType     <$> (mapM convertTypeExpr es)
    (StructTypeExpr fields)  -> do
      vals <- mapM (convertTypeExpr . snd) fields
      let keys = map fst fields
      return (StructType $ zip keys vals)
    (EnumTypeExpr cases)     -> EnumType      <$> mapM evalEnumCase cases
    (EffectTypeExpr e)       -> EffectType    <$> convertTypeExpr e
    (GenericTypeExpr n args) -> GenericType n <$> mapM convertTypeExpr args
    (PrimTypeExpr p)         -> return $ PrimType p
    (PlaceholderExpr name)   -> return $ Placeholder name
    where
      evalEnumCase (name, args) = do
        args' <- mapM convertTypeExpr args
        return (name, args')

checkTypeDefn :: TypeDefn -> Semant STypeDefn
checkTypeDefn defn = do
  -- Check for duplicate definition
  defns <- gets typeDefns
  let name = AST.typeName defn
  when (M.member name defns) $ throwError (DuplicateDefn name DTypeDefn)

  type' <- convertTypeExpr $ AST.typeExpr defn
  let defn' = STypeDefn name (AST.typeConstraints defn) type'

  modify $ \env -> env { typeDefns = M.insert name defn' defns }

  return defn'
