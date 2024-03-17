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

data TypeDefn = TypeDefn { typeDefnName :: Text
                         , typeArgs :: [Text]
                         , typeValue :: TypeExpr } deriving (Show)

data TypeSig = TypeSig Text TypeExpr deriving (Show)

data Expr = Reference Text
          | FuncCall  Text [Expr]
          | IntLit    Int
          | StrLit    Text
          | CharLit   Int
          | FloatLit  Double
          deriving (Show)

data Defn = Defn { defnName :: Text , fnArgs :: [Text] , value :: Expr }
          | AlgoDefn { defnName :: Text , fnArgs :: [Text] , algo :: Algo }
          deriving (Show)

data Algo = Algo [Statement] deriving (Show)

data Statement = IfStmt Expr Algo (Maybe Algo)
               | Expr Expr deriving (Show)

data Program = Program [TypeSig] [Defn] [TypeDefn] deriving (Show)
