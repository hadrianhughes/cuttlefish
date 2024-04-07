module Cuttlefish.Semant.Sast where

import           Control.Monad.Except
import           Control.Monad.State
import           Cuttlefish.Parser.Ast
import qualified Data.Map              as M
import           Data.Text (Text)

data Type = PrimType    PrimType
          | FuncType    Type Type
          | ListType    Type
          | TupleType   [Type]
          | StructType  (M.Map Text Type)
          | EnumType    (M.Map Text [Type])
          | EffectType  Type
          | Placeholder Text
          | GenericType Text [Type]
          deriving (Show, Eq, Ord)

type SExpr = (Type, SExpr')
data SExpr' = SVarRef       Text
            | SListAccess   SExpr SExpr
            | SStructAccess SExpr Text
            | SIfExpr       { ifEConds :: M.Map SExpr SExpr, ifEElse :: SExpr }
            | SFuncCall     { call :: SExpr, callArgs :: [SExpr] }
            | SEffectRun    SExpr
            | SListExpr     [SExpr]
            | STupleExpr    [SExpr]
            | SStructExpr   (M.Map Text SExpr)
            | SMatchExpr    Bind
            | SBlockExpr    [SStatement]
            | SIntLit       Int
            | SCharLit      Char
            | SStrLit       Text
            | SFloatLit     Double
            | SUnitLit
            deriving (Show, Eq, Ord)

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
                deriving (Show, Eq, Ord)

data STypeDefn = STypeDefn { typeName       :: Text
                           , typeConstraint :: [TypeConstraint]
                           , typ            :: Type }
                           deriving (Show, Eq)

data SFuncDefn = SFuncDefn { funcName        :: Text
                           , funcType        :: Type
                           , funcConstraints :: [TypeConstraint]
                           , funcArgs        :: [Bind]
                           , funcBody        :: SExpr }
                           deriving (Show, Eq)

data SConstDefn = SConstDefn { constName  :: Text
                             , constType  :: Maybe Type
                             , constValue :: SExpr }
                             deriving (Show, Eq)

data SClassDefn = SClassDefn { className :: Text
                             , classVar  :: Text
                             , classSigs :: M.Map Text Type }
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
