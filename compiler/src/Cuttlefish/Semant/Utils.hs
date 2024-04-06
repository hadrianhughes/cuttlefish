module Cuttlefish.Semant.Utils where

import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Sast

flatFuncType :: Type -> ([Type], Type)
flatFuncType (FuncType arg rtn) = do
  let (rest, rtn') = case rtn of
        (FuncType _ _) -> flatFuncType rtn
        _              -> ([], rtn)
  (arg : rest, rtn')
flatFuncType t = error ("Called with non-function type: " ++ show t)

isList :: Type -> Bool
isList = \case
  ListType _ -> True
  _          -> False
