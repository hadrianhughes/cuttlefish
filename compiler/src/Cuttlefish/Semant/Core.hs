module Cuttlefish.Semant.Core where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Semant.Error
import           Cuttlefish.Semant.Sast
import qualified Data.Map                as M
import           Data.Text (Text)

data Env = Env { typeDefns   :: M.Map Text STypeDefn
               , classDefns  :: M.Map Text SClassDefn
               , memberDefns :: M.Map Text SMembershipDefn
               , funcDefns   :: M.Map Text SFuncDefn }

type Semant = ExceptT SemantError (State Env)
