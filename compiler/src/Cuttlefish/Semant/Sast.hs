module Cuttlefish.Semant.Sast where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast
import qualified Data.Map              as M

data Type = PrimType    PrimType
          | FuncType    Type Type
          | ListType    Type
          | TupleType   [Type]
          | StructType  (M.Map String Type)
          | EnumType    (M.Map String [Type])
          | EffectType  Type
          | Placeholder String
          | GenericType String [Type]
          deriving (Show, Eq, Ord)

type SExpr = (Type, SExpr')
data SExpr' = SVarRef       String
            | SListAccess   SExpr SExpr
            | SStructAccess SExpr String
            | SIfExpr       { ifEConds :: M.Map SExpr SExpr, ifEElse :: SExpr }
            | SFuncCall     { call :: SExpr, callArgs :: [SExpr] }
            | SEffectRun    SExpr
            | SListExpr     [SExpr]
            | STupleExpr    [SExpr]
            | SStructExpr   (M.Map String SExpr)
            | SMatchExpr    SExpr (M.Map Bind SExpr)
            | SBlockExpr    [SStatement]
            | SIntLit       Int
            | SCharLit      Char
            | SStrLit       String
            | SFloatLit     Double
            | SUnitLit
            deriving (Show, Eq, Ord)

data SStatement = SIfStmt     { ifCond :: SExpr
                              , ifThen :: [SStatement]
                              , ifElse :: Maybe SExpr }
                | SVarDecl    { varName  :: String
                              , varType  :: Maybe Type
                              , varValue :: SExpr }
                | SAssignStmt SExpr SExpr
                | SExprStmt   SExpr
                | ForLoop     { forBind :: Bind
                              , forList :: SExpr
                              , forBody :: SExpr }
                | SReturnStmt SExpr
                deriving (Show, Eq, Ord)

data STypeDefn = STypeDefn { typeName       :: String
                           , typeConstraint :: [TypeConstraint]
                           , typ            :: Type }
                           deriving (Show, Eq)

data SFuncDefn = SFuncDefn { funcName        :: String
                           , funcType        :: Type
                           , funcConstraints :: [TypeConstraint]
                           , funcArgs        :: [Bind]
                           , funcBody        :: SExpr }
                           deriving (Show, Eq)

data SConstDefn = SConstDefn { constName  :: String
                             , constType  :: Maybe Type
                             , constValue :: SExpr }
                             deriving (Show, Eq)

data SClassDefn = SClassDefn { className :: String
                             , classVar  :: String
                             , classSigs :: M.Map String Type }
                             deriving (Show, Eq)

data SMembershipDefn = SMembershipDefn { membClass :: String
                                       , membType  :: String
                                       , membDefns :: [SFuncDefn] }
                                       deriving (Show, Eq)

data SProgram = SProgram { pTypes   :: [STypeDefn]
                         , pConsts  :: [SConstDefn]
                         , pFuncs   :: [SFuncDefn]
                         , pClasses :: [SClassDefn]
                         , pMembers :: [SMembershipDefn] }
                         deriving (Show, Eq)
