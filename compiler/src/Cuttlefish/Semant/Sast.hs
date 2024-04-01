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
          | StructType  [(Text, Type)]
          | EnumType    [(Text, [Type])]
          | EffectType  Type
          | Placeholder Text
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
