module Cuttlefish.Semant.Core where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Semant.Error
import           Cuttlefish.Semant.Sast
import qualified Data.Map                as M
import           Data.Text (Text)

data VarLoc = InScope | AboveScope

data Env = Env { typeDefns   :: M.Map Text STypeDefn
               , classDefns  :: M.Map Text SClassDefn
               , memberDefns :: M.Map Text SMembershipDefn
               , funcDefns   :: M.Map Text SFuncDefn
               , localVars   :: M.Map (Text, VarLoc) SExpr }

type Semant = ExceptT SemantError (State Env)
