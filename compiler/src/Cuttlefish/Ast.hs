module Cuttlefish.Ast where

import Data.Text (Text)

data PrimType = Int | Float | Bool | Unit | Char deriving Show

data TypeConstraint = TypeConstraint Text Text deriving Show

data TypeExpr = FuncType { arg :: TypeExpr, rtn :: TypeExpr }
              | ListType TypeExpr
              | TupleType [TypeExpr]
              | StructType [(Text, TypeExpr)]
              | SetType TypeExpr
              | TypeConstructor { constructors :: [(Text, [TypeExpr])] }
              | ConstraintWrap [TypeConstraint] TypeExpr
              | TypeVar Text
              | PrimType PrimType
              deriving Show

data TypeDefn = TypeDefn { typeDefnName :: Text
                         , typeArgs :: [Text]
                         , typeValue :: TypeExpr } deriving Show

data TypeSig = TypeSig Text TypeExpr deriving Show

data ClassDefn = ClassDefn { classType :: TypeConstraint
                           , classSigs :: [TypeSig] }
                           deriving Show

data Expr = Reference        [Text]
          | ListAccess       Expr Expr
          | TernaryExpr      Expr Expr Expr
          | FuncCall         Expr [Expr]
          | ListExpr         [Expr]
          | DConstructorExpr Text [Expr]
          | TupleExpr        [Expr]
          | MatchExpr        { matchValue :: Text, cases :: [(Bind, Expr)] }
          | IntLit           Int
          | StrLit           Text
          | CharLit          Int
          | FloatLit         Double
          deriving Show

data Bind = SimpleBind       Text
          | ListBind         [Bind]
          | TupleBind        [Bind]
          | DConstructorBind Text [Text]
          deriving Show

data Defn = Defn { defnName :: Text , fnArgs :: [Bind] , value :: Expr }
          | AlgoDefn { defnName :: Text , fnArgs :: [Bind] , algo :: Algo }
          deriving Show

data Algo = Algo [Statement] deriving Show

data MembershipDefn = MembershipDefn { membType :: Text
                                     , membClass :: Text
                                     , membDefns :: [Defn] }
                                     deriving Show

data Statement = IfStmt Expr Algo (Maybe Algo)
               | VarDecl { varName :: Text, varValue :: Expr, mutable :: Bool }
               | Destructure Bind Expr
               | Expr Expr
               | ForLoop Bind Expr Algo
               | Return Expr
               deriving Show

data Program = Program
                [TypeSig]
                [Defn]
                [TypeDefn]
                [ClassDefn]
                [MembershipDefn]
                deriving Show
