module Cuttlefish.Semant.Sast where

import Data.Text (Text)
import Cuttlefish.Ast

data Type = PrimType    PrimType
          | FuncType    Type Type
          | ListType    Type
          | TupleType   [Type]
          | SetType     Type
          | EnumType    Text
          | EffectType  Type
          | GenericType Text [Type]
          deriving (Show, Eq)

type SExpr = (Type, SExpr')
data SExpr' = SVarRef       Text
            | SListAccess   SExpr SExpr
            | SStructAccess SExpr Text
            | SIfExpr       SExpr SExpr
            | SFuncCall     { call :: SExpr, callArgs :: [SExpr] }
            | SEffectRun    SExpr
            | SListExpr     [SExpr]
            | STupleExpr    [SExpr]
            | SStructExpr   Text [(Text, SExpr)]
            | SMatchExpr    Bind
            | SBlockExpr    [SStatement]
            | SIntLit       Int
            | SCharLit      Char
            | SStrLit       Text
            | SFloatLit     Double
            | SUnitLit
            deriving (Show, Eq)

data SStatement = SIfStmt     { ifCond :: SExpr
                              , ifThen :: [SStatement]
                              , ifElse :: Maybe SExpr }
                | SVarDecl    { varName  :: Text
                              , varType  :: Maybe Type
                              , varValue :: SExpr }
                | SAssignStmt SExpr SExpr
                | SExprStmt   SExpr
                | ForLoop     { forBind :: Bind
                              , forList :: SExpr
                              , forBody :: SExpr }
                | SReturnStmt SExpr
                deriving (Show, Eq)

data STypeDefn = STypeDefn { typeName :: Text
                           , typeVars :: [TypeVarDefn]
                           , typ      :: Type }
                           deriving (Show, Eq)

data SFuncDefn = SFuncDefn { funcName     :: Text
                           , funcType     :: Type
                           , funcTypeVars :: [TypeVarDefn]
                           , funcArgs     :: [Bind]
                           , funcBody     :: SExpr }
                           deriving (Show, Eq)

data SConstDefn = SConstDefn { constName  :: Text
                             , constType  :: Maybe Type
                             , constValue :: SExpr }
                             deriving (Show, Eq)

data SClassDefn = SClassDefn { className :: Text
                             , classVar  :: Text
                             , classSigs :: [(Text, Type)]}
                             deriving (Show, Eq)

data SMembershipDefn = SMembershipDefn { membClass :: Text
                                       , membType  :: Text
                                       , membDefns :: [SFuncDefn] }
                                       deriving (Show, Eq)

data SProgram = SProgram { pTypes   :: [STypeDefn]
                         , pConsts  :: [SConstDefn]
                         , pFuncs   :: [SFuncDefn]
                         , pClasses :: [SClassDefn]
                         , pMembers :: [SMembershipDefn] }
                         deriving (Show, Eq)
