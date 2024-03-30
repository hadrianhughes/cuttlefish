module Cuttlefish.Semant where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast   as AST
import           Cuttlefish.Semant.Error as SError
import           Cuttlefish.Semant.Sast
import           Data.Text (Text)
import qualified Data.Map                as M

type TypeDefns = M.Map Text STypeDefn

data Env = Env { typeDefns :: TypeDefns }

type Semant = ExceptT SemantError (State Env)

typeofTypeExpr :: TypeExpr -> Type
typeofTypeExpr expr =
  case expr of
    (FuncTypeExpr e1 e2)    -> FuncType (typeofTypeExpr e1) (typeofTypeExpr e2)
    (ListTypeExpr e)        -> ListType (typeofTypeExpr e)
    (TupleTypeExpr es)      -> TupleType (map typeofTypeExpr es)
    (StructTypeExpr fields) ->
      StructType [(n, typeofTypeExpr e) | (n, e) <- fields]
    (EnumTypeExpr cases)    ->
      EnumType [(n, map typeofTypeExpr es) | (n, es) <- cases]
    (EffectTypeExpr e)      -> EffectType (typeofTypeExpr e)
    (PrimTypeExpr p)        -> PrimType p

checkTypeDefn :: TypeDefn -> Semant STypeDefn
checkTypeDefn defn = do
  defns <- gets typeDefns
  let name = AST.typeName defn
  when (M.member name defns) $ throwError (DuplicateDefn name DTypeDefn)

  let defn' = STypeDefn name (AST.typeVars defn) (typeofTypeExpr $ AST.typeExpr defn)

  modify $ \env -> env { typeDefns = M.insert name defn' defns }

  return defn'

checkProgram :: Program -> Either SemantError SProgram
checkProgram prog = evalState (runExceptT (checkProgram' prog)) env
  where
    env = Env { typeDefns = M.empty }

    checkProgram' :: Program -> Semant SProgram
    checkProgram' prog = do
      let types = AST.pTypes prog
      types' <- mapM checkTypeDefn types
      return $ SProgram types' [] [] [] []
