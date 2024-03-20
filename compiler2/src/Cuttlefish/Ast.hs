module Cuttlefish.Ast where

import Data.Text (Text)

data PrimType = Int | Float | Char | Unit deriving Show

data TypeExpr = FuncType    TypeExpr TypeExpr
              | ListType    TypeExpr
              | TupleType   [TypeExpr]
              | StructType  [(Text, TypeExpr)]
              | SetType     TypeExpr
              | Constructor [(Text, [TypeExpr])]
              | TypeVar     Text
              | PrimType    PrimType
              deriving Show

data TypeConstraint = TypeConstraint Text Text deriving Show

data FuncDefn = FuncDefn { funcName        :: Text
                         , funcType        :: TypeExpr
                         , funcConstraints :: [TypeConstraint]
                         , funcArgs        :: [Bind]
                         , funcBody        :: Expr
                         , funcIsAlgo      :: Bool } deriving Show

data ConstDefn = { constName  :: Text
                 , constValue :: Expr
                 , constType  :: Maybe TypeExpr } deriving Show

data Program = Program
  { funcDefns   :: [FuncDefn]
  , valDefns    :: [ConstDefn]
  , typeDefns   :: [TypeDefn]
  , classDefns  :: [ClassDefn]
  , memberships :: [Membership] } deriving Show
