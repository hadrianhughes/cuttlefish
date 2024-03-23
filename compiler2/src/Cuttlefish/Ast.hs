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

data TypeVarDefn = TypeVarDefn { varClass :: Maybe Text
                               , varName  :: Text } deriving Show

data TypeDefn = TypeDefn { typeName :: Text
                         , typeVars :: [TypeVarDefn]
                         , typeExpr :: TypeExpr } deriving Show

data Expr = VarRef      [Text]
          | ListAccess  Expr Expr
          | TernaryExpr Expr Expr Expr
          | FuncCall    Expr [Expr]
          | ListExpr    [Expr]
          | TupleExpr   [Expr]
          | MatchExpr   Text [(Bind, Expr)]
          | IntLit      Int
          | CharLit     Char
          | FloatLit    Double
          | UnitLit
          deriving Show

data FuncDefn = FuncDefn { funcName        :: Text
                         , funcType        :: TypeExpr
                         , funcTypeVars    :: [TypeVarDefn]
                         , funcArgs        :: [Bind]
                         , funcBody        :: Expr
                         , funcIsAlgo      :: Bool } deriving Show

data ConstDefn = ConstDefn { constName  :: Text
                           , constValue :: Expr
                           , constType  :: Maybe TypeExpr } deriving Show

data ClassDefn = ClassDefn { classBind :: Bind
                           , classSigs :: [(Text, TypeExpr)] } deriving Show

data MembershipDefn = MembershipDefn { membType  :: Text
                                     , membClass :: Text
                                     , membDefns :: FuncDefn } deriving Show

data Bind = SimpleBind      Text
          | TupleBind       [Bind]
          | ConstructorBind Text [Text]
          deriving Show

data Program = Program [TypeDefn] deriving Show
