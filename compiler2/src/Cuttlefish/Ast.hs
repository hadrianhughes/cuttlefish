module Cuttlefish.Ast where

import Data.Text (Text)

data FuncDefn = FuncDefn { funcName :: Text
                         , funcType :: TypeExpr
                         , funcArgs :: [Bind]
                         , funcBody :: Expr
                         , isAlgo   :: Bool }

data Program = Program
  { funcDefns   :: [FuncDefn]
  , valDefns    :: [ValDefn]
  , typeDefns   :: [TypeDefn]
  , classDefns  :: [ClassDefn]
  , memberships :: [Membership] } deriving Show
