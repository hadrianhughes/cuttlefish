module Cuttlefish.Parser.Utils where

import Cuttlefish.Parser.Ast
import Data.Text (Text)

bindHasVar :: Text -> Bind -> Bool
bindHasVar var = \case
  (SimpleBind var2)        -> var == var2
  (TupleBind binds)        -> any (bindHasVar var) binds
  (ConstructorBind _ vars) -> any (== var) vars
