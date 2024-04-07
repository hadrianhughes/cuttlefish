module Cuttlefish.Parser.Utils where

import Cuttlefish.Parser.Ast
import Data.Text (Text)

bindHasVar :: Text -> Bind -> Bool
bindHasVar var = \case
  (SimpleBind var2)        -> var == var2
  (TupleBind binds)        -> any (bindHasVar var) binds
  (ConstructorBind _ vars) -> any (== var) vars

data BindType = SimpleBindType
              | TupleBindType
              | ConstructorBindType

bindType :: Bind -> BindType
bindType = \case
  SimpleBind _        -> SimpleBindType
  TupleBind _         -> TupleBindType
  ConstructorBind _ _ -> ConstructorBindType

pair :: a -> b -> (a, b)
pair a b = (a, b)

maybeList :: Maybe [a] -> [a]
maybeList (Just xs) = xs
maybeList Nothing   = []
