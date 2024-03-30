module Cuttlefish.Semant.Error where

import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Sast
import Data.Text (Text)

type Name = Text

data IllegalBindReason = Duplicate | Unit deriving Show
data DefnKind          = ConstDefn | FuncDefn deriving Show
data RefKind           = VarRef | FuncRef | TypeRef deriving Show
data HeteroTypesKind   = List | Match deriving Show

data SemantError =
    IllegalBinding  Name IllegalBindReason
  | DuplicateDefn   Name DefnKind
  | UndefinedRef    Name RefKind Expr
  | UndefinedType   Name TypeExpr
  | NoMain
  | IllegalAssign   { lhs :: Expr }
  | UnreachableCode Statement
  | TypeError       { expected :: Type, received :: Type, loc :: Expr }
  | UndefinedAccess { struct :: Expr, field :: Name }
  | HeteroTypes     { loc :: Expr, locKind :: HeteroTypesKind }
  | BadStructFields { typName :: Name
                    , expFields :: [(Name, Type)]
                    , recFields :: [(Name, Type)]
                    , loc :: Expr }
  | NonExhaustMatch { matchingTyp :: Type, loc :: Expr }
  | IllegalEffect   { loc :: Expr, func :: SFuncDefn }
  deriving Show
