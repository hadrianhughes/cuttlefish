module Ast where

import Data.Text (Text)

data PrimType = Int | Float | Bool | Unit | Char

data TypeExpr = FuncType { arg :: TypeExpr, rtn :: TypeExpr }
              | ListType TypeExpr
              | TupleType [TypeExpr]
              | StructType [(Text, TypeExpr)]
              | SetType TypeExpr
              | InlineType { inlineTypeName :: Text, args :: [TypeExpr] }
              | TypeVar Text
              | PrimType PrimType

data Defn = Defn { defnName :: Text }

data TypeDefn = TypeDefn { typeDefnName :: Text, value :: TypeExpr }

data TypeSig = TypeSig Text TypeExpr

data Program = Program [Defn] [TypeDefn] [TypeSig]
