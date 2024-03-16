module Cuttlefish.Ast where

import Data.Text (Text)

data PrimType = Int | Float | Bool | Unit | Char deriving (Show)

data TypeExpr = FuncType { arg :: TypeExpr, rtn :: TypeExpr }
              | ListType TypeExpr
              | TupleType [TypeExpr]
              | StructType [(Text, TypeExpr)]
              | SetType TypeExpr
              | InlineType { inlineTypeName :: Text, args :: [TypeExpr] }
              | TypeVar Text
              | PrimType PrimType
              deriving (Show)

data Defn = Defn { defnName :: Text }

data TypeDefn = TypeDefn { typeDefnName :: Text
                         , typeArgs :: [Text]
                         , value :: TypeExpr } deriving (Show)

data TypeSig = TypeSig Text TypeExpr deriving (Show)

data Program = Program [TypeDefn] [TypeSig] deriving (Show)
