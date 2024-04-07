module Cuttlefish.Semant.Class where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast  as AST
import           Cuttlefish.Semant.Core
import           Cuttlefish.Semant.Error
import           Cuttlefish.Semant.Sast
import           Cuttlefish.Semant.Types
import qualified Data.Map                as M
import           Data.Text (Text)

checkClassDefn :: ClassDefn -> Semant SClassDefn
checkClassDefn defn = do
  -- Check for duplicate definition
  defns <- gets classDefns
  let name = AST.className defn
  when (M.member name defns) $ throwError (DuplicateDefn name DClassDefn)

  sigs' <- traverse convertTypeExpr $ AST.classSigs defn
  let defn' = SClassDefn name (AST.classVar defn) sigs'

  modify $ \env -> env { classDefns = M.insert name defn' defns }

  pure defn'
