module Cuttlefish.Parser.Ast where

import qualified Data.Map              as M
import           Data.Text (Text)

data PrimType = Int | Float | Char | Unit deriving (Show, Eq, Ord)

data TypeExpr = FuncTypeExpr    TypeExpr TypeExpr
              | ListTypeExpr    TypeExpr
              | TupleTypeExpr   [TypeExpr]
              | StructTypeExpr  (M.Map Text TypeExpr)
              | EnumTypeExpr    (M.Map Text [TypeExpr])
              | EffectTypeExpr  TypeExpr
              | PlaceholderExpr Text
              | PrimTypeExpr    PrimType
              | GenericTypeExpr Text [TypeExpr]
              deriving (Show, Eq, Ord)

data TypeConstraint = TypeConstraint { constraintClass :: Text
                                     , constraintVar   :: Text } deriving (Show, Eq)

data TypeDefn = TypeDefn { typeName        :: Text
                         , typeConstraints :: [TypeConstraint]
                         , typeExpr        :: TypeExpr } deriving (Show, Eq)

data Expr = VarRef       Text
          | ListAccess   Expr Expr
          | StructAccess Expr Text
          | IfExpr       { ifEConds :: M.Map Expr Expr, ifEElse :: Expr }
          | FuncCall     { call :: Expr, callArgs :: [Expr] }
          | EffectRun    Expr
          | ListExpr     [Expr]
          | TupleExpr    [Expr]
          | StructExpr   (M.Map Text Expr)
          | MatchExpr    Bind (M.Map Bind Expr)
          | BlockExpr    [Statement]
          | IntLit       Int
          | CharLit      Char
          | StrLit       Text
          | FloatLit     Double
          | UnitLit
          deriving (Show, Eq, Ord)

data FuncDefn = FuncDefn { funcName        :: Text
                         , funcType        :: TypeExpr
                         , funcConstraints :: [TypeConstraint]
                         , funcArgs        :: [Bind]
                         , funcBody        :: Expr } deriving (Show, Eq)

data ConstDefn = ConstDefn { constName  :: Text
                           , constType  :: Maybe TypeExpr
                           , constValue :: Expr } deriving (Show, Eq)

data ClassDefn = ClassDefn { className :: Text
                           , classVar  :: Text
                           , classSigs :: M.Map Text TypeExpr } deriving (Show, Eq)

data MembershipImpl = MembershipImpl { implName     :: Text
                                     , implArgs     :: [Bind]
                                     , implBody     :: Expr
                                     , implIsEffect :: Bool } deriving (Show, Eq)

data MembershipDefn = MembershipDefn { membClass :: Text
                                     , membType  :: Text
                                     , membDefns :: [MembershipImpl] } deriving (Show, Eq)

data Bind = SimpleBind      Text
          | TupleBind       [Bind]
          | ConstructorBind Text [Text]
          deriving (Show, Eq, Ord)

data Statement = IfStmt     { ifConds :: M.Map Expr Expr
                            , ifElse  :: Maybe Expr }
               | VarDecl    { varName  :: Text
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
