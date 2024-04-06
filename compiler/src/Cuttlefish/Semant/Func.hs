module Cuttlefish.Semant.Func where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast   as AST
import           Cuttlefish.Parser.Utils
import           Cuttlefish.Semant.Core
import           Cuttlefish.Semant.Error
import           Cuttlefish.Semant.Sast
import           Cuttlefish.Semant.Types
import qualified Data.Map                as M
import           Data.Text (Text)

checkFuncDefn :: FuncDefn -> Semant SFuncDefn
checkFuncDefn defn = do
  -- Check for duplicate definitions
  funcs <- gets funcDefns
  let name = AST.funcName defn
  when (M.member name funcs) $ throwError (DuplicateDefn name DFuncDefn)

  -- Check for duplicate binds
  foldM checkDupArg [] (AST.funcArgs defn)

  -- Check class constraints exist
  classes <- gets classDefns
  let constraints = AST.funcConstraints defn
  forM constraints
    $ \c@(TypeConstraint cn v) -> unless (M.member cn classes)
      $ throwError (UndefinedClass cn $ UCConstraint c)

  -- Check constrained vars are used
  let args = AST.funcArgs defn
  forM constraints $ \(TypeConstraint _ var) ->
    unless (any (bindHasVar var) args) $
      throwError (UnusedTypeVar var $ UTVFunc defn)

  -- Check for appropriate number of binds
  funcType <-  convertTypeExpr $ AST.funcType defn
  funcType' <- case funcType of
    (FuncType _ _)   -> return funcType
    (EnumType cases) ->
      case M.keys cases of
        (name:[]) -> resolveExplicitType name
        _         -> throwInvalidFuncType funcType
    _ -> throwInvalidFuncType funcType

  let (argTypes, _) = flatFuncType funcType'
  when (length argTypes /= length args) $ throwError (IncorrectArity $ ArityFunc defn)

  return $ SFuncDefn
    name
    funcType'
    constraints
    args
    -- TODO: Implement actual body (relies on SExpr)
    (PrimType Unit, SUnitLit)
  where
    checkDupArg :: [Bind] -> Bind -> Semant [Bind]
    checkDupArg acc b = do
      case b of
        (SimpleBind var)         -> forM_ acc (argError var)
        (TupleBind bs)           -> forM_ bs (checkDupArg acc)
        (ConstructorBind _ vars) -> forM_ vars (forM_ acc . argError)
      return (b:acc)
    argError :: Text -> Bind -> Semant Bind
    argError var bind = do
      when (bindHasVar var bind) $ throwError (IllegalBinding var $ IBDuplicate bind)
      return bind
    resolveExplicitType :: Text -> Semant Type
    resolveExplicitType typeName = do
      types <- gets typeDefns
      case M.lookup typeName types of
        Just (STypeDefn _ _ t) -> case t of
          (FuncType _ _) -> return t
          _ -> throwError $ InvalidFuncType t defn
        Nothing -> throwError $ UndefinedType typeName (UTFuncDefn defn)
    throwInvalidFuncType t = error $ "Function has invalid type: " ++ show t

flatFuncType :: Type -> ([Type], Type)
flatFuncType (FuncType arg rtn) = do
  let (rest, rtn') = case rtn of
        (FuncType _ _) -> flatFuncType rtn
        _              -> ([], rtn)
  (arg : rest, rtn')
flatFuncType t = error ("Called with non-function type: " ++ show t)
