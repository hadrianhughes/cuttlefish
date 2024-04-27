module Cuttlefish.Parser.Utils where

import Cuttlefish.Parser.Ast
import Data.Functor.Const

bindHasVar :: String -> Bind -> Bool
bindHasVar var = \case
  (SimpleBind var2)        -> var == var2
  (TupleBind binds)        -> any (bindHasVar var) binds
  (ConstructorBind _ vars) -> any (== var) vars

pair :: a -> b -> (a, b)
pair a b = (a, b)

unmaybeList :: Maybe [a] -> [a]
unmaybeList = \case
  Nothing -> []
  Just xs -> xs
