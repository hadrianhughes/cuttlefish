module Cuttlefish.Semant where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast   as AST
import           Cuttlefish.Parser.Utils
import           Cuttlefish.Semant.Class
import           Cuttlefish.Semant.Core
import           Cuttlefish.Semant.Error as SError
import           Cuttlefish.Semant.Func
import           Cuttlefish.Semant.Member
import           Cuttlefish.Semant.Sast  as SAST
import           Cuttlefish.Semant.Types
import           Cuttlefish.Semant.Utils
import           Cuttlefish.Utils
import           Data.Text (Text)
import qualified Data.Map                as M
import qualified Data.Set                as S

checkProgram :: Program -> Either SemantError SProgram
checkProgram prog = evalState (runExceptT (checkProgram' prog)) env
  where
    env = Env { typeDefns  = M.empty
              , classDefns = M.empty
              , funcDefns  = M.empty }

    checkProgram' :: Program -> Semant SProgram
    checkProgram' prog = do
      types   <- mapM checkTypeDefn   $ AST.pTypes prog
      classes <- mapM checkClassDefn  $ AST.pClasses prog
      members <- mapM checkMemberDefn $ AST.pMembers prog
      funcs   <- mapM checkFuncDefn   $ AST.pFuncs prog
      return $ SProgram types [] funcs classes members
