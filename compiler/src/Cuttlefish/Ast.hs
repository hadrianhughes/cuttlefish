module Cuttlefish.Ast where

import Data.Text (Text)

data PrimType = Int | Float | Char | Unit deriving Show

data TypeExpr = FuncType    TypeExpr TypeExpr
              | ListType    TypeExpr
              | TupleType   [TypeExpr]
              | StructType  [(Text, TypeExpr)]
              | SetType     TypeExpr
              | Constructor [(Text, [TypeExpr])]
              | EffectType  TypeExpr
              | TypeVar     Text
              | PrimType    PrimType
              deriving Show

data TypeVarDefn = TypeVarDefn { varDefnClass :: Maybe Text
                               , varDefnName  :: Text } deriving Show

data TypeDefn = TypeDefn { typeName :: Text
                         , typeVars :: [TypeVarDefn]
                         , typeExpr :: TypeExpr } deriving Show

data Expr = VarRef       Text
          | ListAccess   Expr Expr
          | StructAccess Expr Text
          | IfExpr       Expr Expr Expr
          | FuncCall     { call :: Expr, callArgs :: [Expr] }
          | EffectRun    Expr
          | ListExpr     [Expr]
          | TupleExpr    [Expr]
          | MatchExpr    Bind [(Bind, Expr)]
          | BlockExpr    [Statement]
          | IntLit       Int
          | CharLit      Char
          | StrLit       Text
          | FloatLit     Double
          | UnitLit
          deriving Show

data FuncDefn = FuncDefn { funcName     :: Text
                         , funcType     :: TypeExpr
                         , funcTypeVars :: [TypeVarDefn]
                         , funcArgs     :: [Bind]
                         , funcBody     :: Expr } deriving Show

data ConstDefn = ConstDefn { constName  :: Text
                           , constType  :: Maybe TypeExpr
                           , constValue :: Expr } deriving Show

data ClassDefn = ClassDefn { classBind :: Bind
                           , classSigs :: [(Text, TypeExpr)] } deriving Show

data MembershipDefn = MembershipDefn { membType  :: Text
                                     , membClass :: Text
                                     , membDefns :: FuncDefn } deriving Show

data Bind = SimpleBind      Text
          | TupleBind       [Bind]
          | ConstructorBind Text [Text]
          deriving Show

data Statement = IfStmt { ifCond :: Expr
                        , ifThen :: [Statement]
                        , ifElse :: Maybe [Statement] }
               | VarDecl { varName  :: Text
                         , varType  :: Maybe TypeExpr
                         , varValue :: Expr }
               | AssignStmt Expr Expr
               | ExprStmt Expr
               | ForLoop { forBind :: Bind
                         , forList :: Expr
                         , forBody :: [Statement] }
               | ReturnStmt Expr
               deriving Show

data Program = Program
  [TypeDefn]
  [ConstDefn]
  [FuncDefn]
  deriving Show
