module Cuttlefish.Semant.Error where

import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Sast
import Data.Text (Text)

type Name = Text

data IllegalBindReason = Duplicate | Unit deriving Show
data DefnKind          = ConstDefn | FuncDefn | TypeDefn deriving Show
data RefKind           = VarRef | FuncRef deriving Show
data HeteroTypesKind   = List | Match deriving Show

data SemantError =
    IllegalBinding  Name IllegalBindReason
  | DuplicateDefn   Name DefnKind
  | UndefinedRef    Name RefKind Expr
  | NoMain
  | IllegalAssign   { lhs :: Expr }
  | UnreachableCode Statement
  | TypeError       { expected :: Type, received :: Type, loc :: Expr }
  | UndefinedType   { typ :: Type, loc :: Expr }
  | UndefinedAccess { struct :: Expr, field :: Name }
  | HeteroTypes     { loc :: Expr, locKind :: HeteroTypesKind }
  | BadStructFields { typName :: Name
                    , expFields :: [(Name, Type)]
                    , recFields :: [(Name, Type)]
                    , loc :: Expr }
  | NonExhaustMatch { matchingTyp :: Type, loc :: Expr }
  | IllegalEffect   { loc :: Expr, func :: SFuncDefn }
  deriving Show
