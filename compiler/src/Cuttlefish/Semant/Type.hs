module Cuttlefish.Semant.Type where

import           Control.Monad.Except
import           Cuttlefish.Parser.Ast
import           Cuttlefish.Semant.Sast
import           Data.Text (Text)
import qualified Data.Map                as M

typeofTypeExpr :: TypeExpr -> Semant Type
typeofTypeExpr expr =
  case expr of
    (FuncTypeExpr e1 e2)    -> FuncType (typeofTypeExpr defns e1) (typeofTypeExpr defns e2)
    (ListTypeExpr e)        -> ListType $ typeofTypeExpr defns e
    (TupleTypeExpr es)      -> TupleType $ map (typeofTypeExpr defns) es
    (StructTypeExpr fields) -> StructType [(n, typeofTypeExpr defns e) | (n, e) <- fields]
    (EnumTypeExpr cases)    -> EnumType [(n, map (typeofTypeExpr defns) es) | (n, es) <- cases]
    (EffectTypeExpr e)      -> EffectType $ typeofTypeExpr defns e
    (GenericTypeExpr name args) -> do

resolveGeneric :: Text -> [Type] -> Semant Type
resolveGeneric name args = do
  defns <- gets typeDefns
  when (M.notMember name defns) $ throwError ()
