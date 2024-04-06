module Cuttlefish.Semant.Expr where

import Cuttlefish.Parser.Ast
import Cuttlefish.Semant.Core
import Cuttlefish.Semant.Sast

checkExpr :: Expr -> Semant SExpr
checkExpr = \case
  (IntLit i)   -> return (PrimType Int, SIntLit i)
  (CharLit c)  -> return (PrimType Char, SCharLit c)
  (StrLit s)   -> return (ListType $ PrimType Char, SStrLit s)
  (FloatLit f) -> return (PrimType Float, SFloatLit f)
  UnitLit      -> return (PrimType Unit, SUnitLit)
