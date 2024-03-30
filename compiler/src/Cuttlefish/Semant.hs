module Cuttlefish.Semant where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast   as AST
import           Cuttlefish.Semant.Error as SError
import           Cuttlefish.Semant.Sast
import           Data.Text (Text)
import qualified Data.Map                as M

type Types = M.Map Text Type

data Env = Env { types :: Types }

type Semant = ExceptT SemantError (State Env)

--checkTypeDefn :: TypeDefn -> Semant STypeDefn
--checkTypeDefn td = do
--  types <- gets types
--  let name = AST.typeName td
--  when (M.member name types) $ throwError $ DuplicateDefn name SError.TypeDefn
--  modify $ \env -> env { types = M.insert name (typeExpr td) types }
