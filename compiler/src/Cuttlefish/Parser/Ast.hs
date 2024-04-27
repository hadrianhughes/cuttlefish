module Cuttlefish.Parser.Ast where

import qualified Data.Map              as M

data PrimType = Int | Float | Char | Unit deriving (Show, Eq, Ord)

data TypeExpr = FuncTypeExpr    TypeExpr TypeExpr
              | ListTypeExpr    TypeExpr
              | TupleTypeExpr   [TypeExpr]
              | StructTypeExpr  (M.Map String TypeExpr)
              | EnumTypeExpr    (M.Map String [TypeExpr])
              | EffectTypeExpr  TypeExpr
              | PlaceholderExpr String
              | PrimTypeExpr    PrimType
              | GenericTypeExpr String [TypeExpr]
              deriving (Show, Eq, Ord)

data TypeConstraint = TypeConstraint { constraintClass :: String
                                     , constraintVar   :: String } deriving (Show, Eq)

data TypeDefn = TypeDefn { typeName        :: String
                         , typeConstraints :: [TypeConstraint]
                         , typeExpr        :: TypeExpr } deriving (Show, Eq)

data Expr = VarRef       String
          | ListAccess   Expr Expr
          | StructAccess Expr String
          | IfExpr       { ifEConds :: M.Map Expr Expr, ifEElse :: Expr }
          | FuncCall     { call :: Expr, callArgs :: [Expr] }
          | EffectRun    Expr
          | ListExpr     [Expr]
          | TupleExpr    [Expr]
          | StructExpr   (M.Map String Expr)
          | MatchExpr    Expr (M.Map Bind Expr)
          | BlockExpr    [Statement]
          | IntLit       Int
          | CharLit      Char
          | StrLit       String
          | FloatLit     Double
          | UnitLit
          deriving (Show, Eq, Ord)

data FuncDefn = FuncDefn { funcName        :: String
                         , funcType        :: TypeExpr
                         , funcConstraints :: [TypeConstraint]
                         , funcArgs        :: [Bind]
                         , funcBody        :: Expr } deriving (Show, Eq)

data ConstDefn = ConstDefn { constName  :: String
                           , constType  :: Maybe TypeExpr
                           , constValue :: Expr } deriving (Show, Eq)

data ClassDefn = ClassDefn { className :: String
                           , classVar  :: String
                           , classSigs :: M.Map String TypeExpr } deriving (Show, Eq)

data MembershipImpl = MembershipImpl { implName     :: String
                                     , implArgs     :: [Bind]
                                     , implBody     :: Expr
                                     , implIsEffect :: Bool } deriving (Show, Eq)

data MembershipDefn = MembershipDefn { membClass :: String
                                     , membType  :: String
                                     , membDefns :: [MembershipImpl] } deriving (Show, Eq)

data Bind = SimpleBind      String
          | TupleBind       [Bind]
          | ConstructorBind String [String]
          deriving (Show, Eq, Ord)

data Statement = IfStmt     { ifConds  :: M.Map Expr Expr
                            , ifElse   :: Maybe Expr }
               | VarDecl    { varName  :: String
                            , varType  :: Maybe TypeExpr
                            , varValue :: Expr }
               | AssignStmt Expr Expr
               | EffectStmt Expr
               | ForLoop    { forBind :: Bind
                            , forList :: Expr
                            , forBody :: Expr }
               | ReturnStmt Expr
               deriving (Show, Eq, Ord)

data Program = Program { pTypes   :: [TypeDefn]
                       , pConsts  :: [ConstDefn]
                       , pFuncs   :: [FuncDefn]
                       , pClasses :: [ClassDefn]
                       , pMembers :: [MembershipDefn] }
                       deriving (Show, Eq)
