module Ast where

data PrimType = Int | Float | Bool | Unit | Char

data Type = FuncType { arg :: Type, rtn :: Type }
          | ListType Type
          | TupleType [Type]
          | StructType [(Text, Type)]
          | SetType Type
          | ConstrType { name :: Text, args :: [Type] }
          | TypeRef { name :: Text, args :: [Type] }
          | TypeVar Text
          | PrimType

data Defn = Defn { name :: Text }

data TypeDefn = TypeDefn { name :: Text, value :: Type }

data TypeSig = TypeSig Text Type

data Program = Program [Defn] [TypeDef] [TypeSig] deriving (Eq, Show)
