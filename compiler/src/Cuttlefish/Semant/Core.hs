module Cuttlefish.Semant.Core where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Semant.Error
import           Cuttlefish.Semant.Sast
import qualified Data.Map                as M

data VarLoc = InScope | AboveScope

data Env = Env { typeDefns   :: M.Map String STypeDefn
               , classDefns  :: M.Map String SClassDefn
               , memberDefns :: M.Map String SMembershipDefn
               , funcDefns   :: M.Map String SFuncDefn
               , localVars   :: M.Map String (SExpr, VarLoc) }

type Semant = ExceptT SemantError (State Env)
