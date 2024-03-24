module Cuttlefish.Parser.Body where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Control.Monad ( void )
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Types

-- Expressions

data ChainTerm = StructTerm Text
               | ListTerm   Expr
               | FuncTerm   [Expr]

data BinopTerm = BinopTerm Text Expr

chainAcc :: Expr -> ChainTerm -> Expr
chainAcc e c =
  case c of
    (StructTerm field) -> StructAccess e field
    (ListTerm index)   -> ListAccess e index
    (FuncTerm args)    -> FuncCall e args

chainExprP :: Parser Expr
chainExprP = do
  start <- try exprP
  terms <- many term
  return $ foldl chainAcc start terms
  where
    term :: Parser ChainTerm
    term = StructTerm   <$> (dot *> identifier)
       <|> ListTerm     <$> brackets topLevelExprP
       <|> FuncTerm     <$> (parens $ topLevelExprP `sepBy` comma)

operatorP :: Parser Expr
operatorP = do
  start <- try chainExprP <|> exprP
  terms <- many $ BinopTerm <$> binop <*> (try routineExprP <|> try chainExprP <|> exprP)
  return $ foldr (\(BinopTerm op arg2) expr -> FuncCall (VarRef op) [expr, arg2]) start terms

literalP :: Parser Expr
literalP = try (FloatLit <$> float)
       <|> IntLit   <$> int
       <|> CharLit  <$> squotes (satisfy (`notElem` ['\\', '\'']) <|> single '\\')
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> (symbol "()" *> return UnitLit)

ternaryP :: Parser Expr
ternaryP = TernaryExpr
       <$> nestedExpr
       <*> (symbol "?" *> nestedExpr)
       <*> (symbol ":" *> nestedExpr)
       where
        nestedExpr = try routineExprP <|> try chainExprP <|> exprP

matchExprP :: Parser Expr
matchExprP = MatchExpr
  <$> (rword "match" *> bindP)
  <*> braces (caseP `sepBy1` comma)
  where
    caseP = do
      bind <- bindP
      expr <- symbol "->" *> topLevelExprP
      return (bind, expr)

exprP :: Parser Expr
exprP = ListExpr    <$> brackets (topLevelExprP `sepBy` comma)
    <|> TupleExpr   <$> parens (topLevelExprP `sepBy` comma)
    <|> matchExprP
    <|> parens topLevelExprP
    <|> VarRef      <$> identifier
    <|> literalP

routineExprP :: Parser Expr
routineExprP = RoutineExpr <$> routineP

topLevelExprP :: Parser Expr
topLevelExprP = try routineExprP
            <|> try ternaryP
            <|> try operatorP
            <|> try chainExprP
            <|> exprP

bindP :: Parser Bind
bindP = try (TupleBind <$> parens (bindP `sepBy1` comma))
    <|> ConstructorBind
      <$> typeIdentifier
      <*> (maybeList <$> optional (parens (identifier `sepBy1` comma)))
    <|> SimpleBind <$> identifier

constDefnP :: Parser ConstDefn
constDefnP = ConstDefn
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> topLevelExprP)

-- Functions

funcDefnP :: Parser FuncDefn
funcDefnP = do
  name     <- rword "func" *> identifier
  typeVars <- optional (angles $ some typeVarDefnP)
  args     <- parens $ argP `sepBy` comma
  rtnType  <- symbol "->" *> openTypeExprP
  body     <- (symbol "=" *> topLevelExprP) <|> routineExprP

  let funcType  = foldr argFold rtnType args
      typeVars' = maybeList typeVars

  return $ FuncDefn
    name
    funcType
    typeVars'
    (map fst args)
    body
  where
    argFold :: (a, TypeExpr) -> TypeExpr -> TypeExpr
    argFold = \(_, t1) t2 -> FuncType t1 t2
    argP = do
      name <- bindP
      typ  <- colon *> openTypeExprP
      return (name, typ)

funcDefnP' :: Parser FuncDefn
funcDefnP' = do
  (name, funcType) <- rword "func" *> parens nameTypeP
  typeVars         <- optional (angles $ some typeVarDefnP)
  args             <- parens (bindP `sepBy` comma)
  body             <- (symbol "=" *> topLevelExprP) <|> routineExprP

  let typeVars' = maybeList typeVars

  return $ FuncDefn
    name
    funcType
    typeVars'
    args
    body
  where
    nameTypeP = do
      name <- identifier
      typ  <- colon *> openTypeExprP
      return (name, typ)

-- Statements

ifStmtP :: Parser Statement
ifStmtP = IfStmt
  <$> (rword "if" *> try ternaryP <|> try operatorP <|> try chainExprP <|> exprP)
  <*> routineP
  <*> optional (rword "else" *> routineP)

varDeclP :: Parser Statement
varDeclP = VarDecl
  <$> (rword "let" *> identifier)
  <*> optional (symbol ":" *> openTypeExprP)
  <*> (symbol "=" *> topLevelExprP)

assignStmtP :: Parser Statement
assignStmtP = AssignStmt
          <$> (try chainExprP <|> exprP)
          <*> (symbol "=" *> topLevelExprP)

exprStmtP :: Parser Statement
exprStmtP = ExprStmt <$> topLevelExprP

forLoopP :: Parser Statement
forLoopP = ForLoop
       <$> (rword "for" *> bindP)
       <*> (rword "in" *> (try chainExprP <|> exprP))
       <*> routineP

returnStmtP :: Parser Statement
returnStmtP = ReturnStmt <$> (rword "return" *> topLevelExprP)

routineP :: Parser [Statement]
routineP = braces (statementP `sepBy` eol)

statementP :: Parser Statement
statementP = try ifStmtP
         <|> try varDeclP
         <|> try assignStmtP
         <|> try exprStmtP
         <|> try forLoopP
         <|> returnStmtP
