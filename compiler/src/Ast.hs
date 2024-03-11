module Ast where

data Type = Int | Float | Bool | Unit | Char

data Bind = Bind { name :: Text }

data TypeDef = TypeDef { name :: Text, value :: Type }

data Program = Program [Bind] [TypeDef] [TypeSig] deriving (Eq, Show)
