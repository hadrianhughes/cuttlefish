module Cuttlefish.Semant.Error where

import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Sast

type Name = String

data IllegalBindReason  = IBDuplicate Bind | IBUnit deriving Show
data DefnKind           = DConstDefn | DFuncDefn | DTypeDefn | DClassDefn deriving Show
data RefKind            = RefVar | RefFunc deriving Show
data HeteroTypesKind    = List | Match deriving Show
data UnusedTypeVarsLoc  = UTVType TypeDefn | UTVFunc FuncDefn deriving Show
data UndefinedClassKind = UCMemberDefn MembershipDefn | UCConstraint TypeConstraint deriving Show
data UndefinedTypeKind  = UTExpr       Expr
                        | UTMemberDefn MembershipDefn
                        | UTFuncDefn   FuncDefn deriving Show
data ArityLoc           = ArityMember   MembershipImpl Type
                        | ArityFuncDefn FuncDefn
                        | ArityFuncCall SExpr
                        deriving Show

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
  | UndefinedVar    Name Expr
  | PatternMismatch Bind SExpr
  deriving Show
