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
    IllegalBinding  Name IllegalBindReason
  | DuplicateDefn   Name DefnKind
  | UndefinedType   Name UndefinedTypeKind
  | UndefinedClass  Name UndefinedClassKind
  | UnusedTypeVar   Name UnusedTypeVarsLoc
  | UnexpectedSig   Name MembershipDefn
  | IncorrectArity  ArityLoc
  | InvalidFuncType Type FuncDefn
  | TypeError       { exp :: Type, act :: Type, loc :: SExpr' }
  | UndefinedField  SExpr Name
  deriving Show
