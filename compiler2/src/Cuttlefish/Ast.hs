module Cuttlefish.Ast where

data Program = Program
  { funcDefns   :: [FuncDefn]
  , valDefns    :: [ValDefn]
  , typeDefns   :: [TypeDefn]
  , classDefns  :: [ClassDefn]
  , memberships :: [Membership] } deriving Show
