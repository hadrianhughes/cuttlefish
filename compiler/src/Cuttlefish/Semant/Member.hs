module Cuttlefish.Semant.Member where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast   as AST
import           Cuttlefish.Semant.Core
import           Cuttlefish.Semant.Error
import           Cuttlefish.Semant.Sast  as SAST
import           Cuttlefish.Semant.Utils
import qualified Data.Map                as M

checkMemberDefn :: MembershipDefn -> Semant SMembershipDefn
checkMemberDefn defn = do
  -- Check class exists
  classes <- gets classDefns
  let className = AST.membClass defn
      classDefn = M.lookup className classes
  case classDefn of
    Nothing -> throwError (UndefinedClass className $ UCMemberDefn defn)
    Just cd -> do
      -- Check type exists
      types <- gets typeDefns
      let typeName = AST.membType defn
      unless (M.member typeName types) $ throwError (UndefinedType typeName $ UTMemberDefn defn)

      defns' <- mapM (checkImpl cd) (AST.membDefns defn)
      return $ SMembershipDefn className typeName defns'
  where
    checkImpl :: SClassDefn -> MembershipImpl -> Semant SFuncDefn
    checkImpl classDefn impl = do
      let sigs   = SAST.classSigs classDefn

      -- Check func belongs to class
      let name = implName impl
          sig  = M.lookup name sigs

      case sig of
        Nothing -> throwError (UnexpectedSig name defn)
        Just t  -> do
          -- Check func provides enough args
          let (args, rtn) = flatFuncType t
              arity       = length args
              argc        = length $ implArgs impl
          when (argc /= arity) $ throwError (IncorrectArity $ ArityMember impl t)

          return $ SFuncDefn name t [] (implArgs impl) (PrimType Unit, SUnitLit)
          -- TODO: Get this working (relies on SExpr)
          --return $ SFuncDefn name t [] (implArgs impl) (implBody impl)
