module Cuttlefish.Parser.Ast where

import Data.Text (Text)

data PrimType = Int | Float | Char | Unit deriving (Show, Eq)

data TypeExpr = FuncType    TypeExpr TypeExpr
              | ListType    TypeExpr
              | TupleType   [TypeExpr]
              | StructType  [(Text, TypeExpr)]
              | EnumType    [(Text, [TypeExpr])]
              | EffectType  TypeExpr
              | GenericType Text [TypeExpr]
              | TypeVar     Text
              | PrimType    PrimType
              deriving (Show, Eq)

data TypeVarDefn = TypeVarDefn { varDefnClass :: Maybe Text
                               , varDefnName  :: Text } deriving (Show, Eq)

data TypeDefn = TypeDefn { typeName :: Text
                         , typeVars :: [TypeVarDefn]
                         , typeExpr :: TypeExpr } deriving (Show, Eq)

data Expr = VarRef       Text
          | ListAccess   Expr Expr
          | StructAccess Expr Text
          | IfExpr       Expr Expr Expr
          | FuncCall     { call :: Expr, callArgs :: [Expr] }
          | EffectRun    Expr
          | ListExpr     [Expr]
          | TupleExpr    [Expr]
          | StructExpr   [(Text, Expr)]
          | MatchExpr    Bind [(Bind, Expr)]
          | BlockExpr    [Statement]
          | IntLit       Int
          | CharLit      Char
          | StrLit       Text
          | FloatLit     Double
          | UnitLit
          deriving (Show, Eq)

data FuncDefn = FuncDefn { funcName     :: Text
                         , funcType     :: TypeExpr
                         , funcTypeVars :: [TypeVarDefn]
                         , funcArgs     :: [Bind]
                         , funcBody     :: Expr } deriving (Show, Eq)

data ConstDefn = ConstDefn { constName  :: Text
                           , constType  :: Maybe TypeExpr
                           , constValue :: Expr } deriving (Show, Eq)

data ClassDefn = ClassDefn { className :: Text
                           , classVar  :: Text
                           , classSigs :: [(Text, TypeExpr)] } deriving (Show, Eq)

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
          deriving (Show, Eq)

data Statement = IfStmt     { ifConds :: [(Expr, Expr)]
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
               deriving (Show, Eq)

data Program = Program { pTypes   :: [TypeDefn]
                       , pConsts  :: [ConstDefn]
                       , pFuncs   :: [FuncDefn]
                       , pClasses :: [ClassDefn]
                       , pMembers :: [MembershipDefn] }
                       deriving (Show, Eq)
