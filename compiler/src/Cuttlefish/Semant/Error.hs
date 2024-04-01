module Cuttlefish.Semant.Error where

import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Sast
import Data.Text (Text)

type Name = Text

data IllegalBindReason  = IBDuplicate Bind | IBUnit deriving Show
data DefnKind           = DConstDefn | DFuncDefn | DTypeDefn | DClassDefn deriving Show
data RefKind            = VarRef | FuncRef deriving Show
data HeteroTypesKind    = List | Match deriving Show
data UnusedTypeVarsLoc  = UTVType TypeDefn | UTVFunc FuncDefn deriving Show
data UndefinedClassKind = UCMemberDefn MembershipDefn | UCConstraint TypeConstraint deriving Show
data UndefinedTypeKind  = UTExpr       Expr
                        | UTMemberDefn MembershipDefn
                        | UTFuncDefn   FuncDefn deriving Show
data ArityLoc           = ArityMember MembershipImpl Type
                        | ArityFunc   FuncDefn deriving Show

data SemantError =
    IllegalBinding    Name IllegalBindReason
  | DuplicateDefn     Name DefnKind
  | UndefinedRef      Name RefKind Expr
  | UndefinedType     Name UndefinedTypeKind
  | UndefinedTypeVars Name TypeExpr
  | UndefinedClass    Name UndefinedClassKind
  | UnusedTypeVar     Name UnusedTypeVarsLoc
  | UnexpectedSig     Name MembershipDefn
  | IncorrectArity    ArityLoc
  | NoMain
  | IllegalAssign     { lhs :: Expr }
  | UnreachableCode   Statement
  | TypeError         { expected :: Type, received :: Type, loc :: Expr }
  | UndefinedAccess   { struct :: Expr, field :: Name }
  | HeteroTypes       { loc :: Expr, locKind :: HeteroTypesKind }
  | BadStructFields   { typName :: Name
                      , expFields :: [(Name, Type)]
                      , recFields :: [(Name, Type)]
                      , loc :: Expr }
  | NonExhaustMatch   { matchingTyp :: Type, loc :: Expr }
  | IllegalEffect     { loc :: Expr, func :: SFuncDefn }
  | IllegalGeneric    TypeExpr
  | InvalidFuncType   Type FuncDefn
  deriving Show
