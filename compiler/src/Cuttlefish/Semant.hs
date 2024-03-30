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
