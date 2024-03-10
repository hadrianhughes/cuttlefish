module Ast where

data Bind = Bind { name :: Text }

data TypeDecl = TypeDecl { name :: Text, value :: Type }

data Program = Program [Bind] [Type] [TypeSig] deriving (Eq, Show)
