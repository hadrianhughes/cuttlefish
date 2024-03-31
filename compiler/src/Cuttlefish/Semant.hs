module Cuttlefish.Semant where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast   as AST
import           Cuttlefish.Semant.Error as SError
import           Cuttlefish.Semant.Sast
import           Data.Text (Text)
import qualified Data.Map                as M
import qualified Data.Set                as S

type TypeDefns  = M.Map Text STypeDefn
type ClassDefns = M.Map Text SClassDefn

data Env = Env { typeDefns  :: TypeDefns
               , classDefns :: ClassDefns}

type Semant = ExceptT SemantError (State Env)

typeofTypeExpr :: TypeExpr -> Semant Type
typeofTypeExpr expr =
  case expr of
    (FuncTypeExpr e1 e2)    -> FuncType  <$> (typeofTypeExpr e1) <*> (typeofTypeExpr e2)
    (ListTypeExpr e)        -> ListType  <$> (typeofTypeExpr e)
    (TupleTypeExpr es)      -> TupleType <$> (mapM typeofTypeExpr es)
    (StructTypeExpr fields) -> do
      vals <- mapM (typeofTypeExpr . snd) fields
      let keys = map fst fields
      return (StructType $ zip keys vals)
    (EnumTypeExpr cases)    -> EnumType   <$> mapM evalEnumCase cases
    (EffectTypeExpr e)      -> EffectType <$> typeofTypeExpr e
    (PrimTypeExpr p)        -> return $ PrimType p
    (PlaceholderExpr name)  -> return $ Placeholder name
    (GenericTypeExpr _ _)   -> throwError (IllegalGeneric expr)
    where
      evalEnumCase (name, args) = do
        args' <- mapM typeofTypeExpr args
        return (name, args')

typeVarsUsed :: Type -> S.Set Text
typeVarsUsed = \case
  (FuncType t1 t2)    -> typeVarsUsed t1 <> typeVarsUsed t2
  (ListType t)        -> typeVarsUsed t
  (TupleType ts)      -> S.unions $ map typeVarsUsed ts
  (StructType fields) -> S.unions $ map (typeVarsUsed . snd) fields
  (EnumType cases)    -> S.unions $ map typeVarsUsed $ concat $ map snd cases
  (EffectType t)      -> typeVarsUsed t
  (Placeholder name)  -> S.singleton name
  _                   -> S.empty

checkTypeDefn :: TypeDefn -> Semant STypeDefn
checkTypeDefn defn = do
  defns <- gets typeDefns
  let name = AST.typeName defn
  when (M.member name defns) $ throwError (DuplicateDefn name DTypeDefn)

  type' <- typeofTypeExpr $ AST.typeExpr defn
  let defn'         = STypeDefn name (AST.typeVars defn) type'
      varsUsed      = typeVarsUsed type'
      varsDeclared  = S.fromList $ map typeVarName $ AST.typeVars defn
      undefinedVars = S.difference varsUsed varsDeclared
      unusedVars    = S.difference varsDeclared varsUsed

  unless (null undefinedVars) $ throwError (UndefinedTypeVars (S.toList undefinedVars) (AST.typeExpr defn))
  unless (null unusedVars) $ throwError (UnusedTypeVars (S.toList unusedVars) (UTVType defn))

  modify $ \env -> env { typeDefns = M.insert name defn' defns }

  return defn'

checkClassDefn :: ClassDefn -> Semant SClassDefn
checkClassDefn defn = do
  defns <- gets classDefns
  let name = AST.className defn
  when (M.member name defns) $ throwError (DuplicateDefn name DClassDefn)

  sigs' <- mapM evalSig $ AST.classSigs defn
  let defn' = SClassDefn name (AST.classVar defn) sigs'

  modify $ \env -> env { classDefns = M.insert name defn' defns }

  return defn'
  where
    evalSig :: (Text, TypeExpr) -> Semant (Text, Type)
    evalSig (name, t) = do
      t' <- typeofTypeExpr t
      return (name, t')

checkProgram :: Program -> Either SemantError SProgram
checkProgram prog = evalState (runExceptT (checkProgram' prog)) env
  where
    env = Env { typeDefns = M.empty, classDefns = M.empty }

    checkProgram' :: Program -> Semant SProgram
    checkProgram' prog = do
      types   <- mapM checkTypeDefn $ AST.pTypes prog
      classes <- mapM checkClassDefn $ AST.pClasses prog
      return $ SProgram types [] [] classes []
