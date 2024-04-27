module Cuttlefish.Parser.Body where

import           Data.Char
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Either
import           Control.Applicative
import           Control.Applicative.Combinators
import           Control.Monad ( void )
import           Cuttlefish.Parser.Ast
import           Cuttlefish.Parser.Core (Parser)
import qualified Cuttlefish.Parser.Core as P
import           Cuttlefish.Parser.Lexer
import           Cuttlefish.Parser.Types
import           Cuttlefish.Parser.Utils

-- Expressions

data ChainTerm = StructTerm String
               | ListTerm   Expr
               | FuncTerm   [Expr]

data BinopTerm = BinopTerm String Expr

chainAcc :: Expr -> ChainTerm -> Expr
chainAcc e = \case
  (StructTerm field) -> StructAccess e field
  (ListTerm index)   -> ListAccess e index
  (FuncTerm args)    -> FuncCall e args

chainExprP :: Parser Expr
chainExprP = do
  start <- exprP
  terms <- many term
  pure $ foldl chainAcc start terms
  where
    term :: Parser ChainTerm
    term = StructTerm   <$> (dot *> identifier)
       <|> ListTerm     <$> brackets' topLevelExprP
       <|> FuncTerm     <$> (parens' $ topLevelExprP `sepBy` (comma <* sc))

operatorP :: Parser Expr
operatorP = do
  start <- chainExprP <|> exprP
  terms <- many $ BinopTerm <$> binop <*> (blockExprP <|> chainExprP <|> exprP)
  pure $ foldr (\(BinopTerm op arg2) expr -> FuncCall (VarRef op) [expr, arg2]) start terms

literalP :: Parser Expr
literalP = FloatLit <$> float
       <|> IntLit   <$> int
       <|> CharLit  <$> squotes (P.satisfy (`notElem` ['\\', '\'']) <|> P.char '\\')
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> (symbol "()" *> pure UnitLit)

ifExprP :: Parser Expr
ifExprP = rword "if" *> do
  cond1     <- condExpr
  then1     <- rword "then" *> nestedExpr <* sc
  rest      <- many elifExpr
  elseBlock <- rword "else" *> nestedExpr
  pure $ IfExpr (M.fromList $ (cond1, then1) : rest) elseBlock
  where
    condExpr   = operatorP <|> chainExprP <|> exprP <* sc
    nestedExpr = blockExprP <|> operatorP <|> chainExprP <|> exprP
    elifExpr   = pair <$> (rword "else" *> rword "if" *> condExpr) <*> (rword "then" *> nestedExpr)

matchExprP :: Parser Expr
matchExprP = MatchExpr
  <$> (rword "match" *> chainExprP)
  <*> (M.fromList <$> (braces' $ caseP `sepBy1` (comma <* sc)))
  where
    caseP = pair <$> bindP <*> (P.symbol sc "->" *> topLevelExprP)

listExprP :: Parser Expr
listExprP = ListExpr <$> brackets' (topLevelExprP `sepBy` comma)

tupleExprP :: Parser Expr
tupleExprP = TupleExpr <$> parens' (topLevelExprP `sepBy2` comma)

structExprP :: Parser Expr
structExprP = (StructExpr . M.fromList) <$> braces' (keyValPair `sepBy` comma <* sc)
  where
    keyValPair = pair <$> (identifier <* colon <* sc) <*> exprP

varRefP :: Parser Expr
varRefP = VarRef <$> identifier

exprP :: Parser Expr
exprP = listExprP
    <|> tupleExprP
    <|> structExprP
    <|> matchExprP
    <|> parens' topLevelExprP
    <|> varRefP
    <|> literalP

blockExprP :: Parser Expr
blockExprP = BlockExpr <$> blockP

effectRunP :: Parser Expr
effectRunP = EffectRun <$> expr <* symbol "!"
  where
    expr = chainExprP <|> (VarRef <$> identifier)

topLevelExprP :: Parser Expr
topLevelExprP = blockExprP
            <|> ifExprP
            <|> operatorP
            <|> effectRunP
            <|> chainExprP
            <|> exprP

bindP :: Parser Bind
bindP = TupleBind <$> parens (bindP `sepBy1` comma)
    <|> ConstructorBind
      <$> typeIdentifier
      <*> (unmaybeList <$> optional (parens $ identifier `sepBy1` comma))
    <|> SimpleBind <$> identifier

constDefnP :: Parser ConstDefn
constDefnP = const <* sc
  where
    const = ConstDefn
        <$> (rword "let" *> identifier)
        <*> optional (colon *> openTypeExprP)
        <*> (P.symbol sc "=" *> topLevelExprP)

-- Functions

argP :: Parser (Bind, TypeExpr)
argP = pair <$> bindP <*> (colon *> openTypeExprP)

argFold :: (a, TypeExpr) -> TypeExpr -> TypeExpr
argFold (_, t1) t2 = FuncTypeExpr t1 t2

funcDefnP :: Parser FuncDefn
funcDefnP = parse <* sc
  where
    parse = do
      name     <- rword "func" *> identifier
      typeVars <- optional (angles $ some typeConstraintP)
      args     <- parens' $ argP `sepBy` (comma <* sc)
      rtnType  <- optional openTypeExprP
      body     <- (P.symbol sc "=" *> topLevelExprP) <|> blockExprP

      let rtnType'  = fromMaybe (PrimTypeExpr Unit) rtnType
          funcType  = foldr argFold rtnType' args
          typeVars' = unmaybeList typeVars

      pure $ FuncDefn
        name
        funcType
        typeVars'
        (map fst args)
        body

-- Supports typing a function with a predefined type
funcDefnP' :: Parser FuncDefn
funcDefnP' = parse <* sc
  where
    nameTypeP = pair <$> identifier <*> (colon *> openTypeExprP)
    parse = do
      (name, funcType) <- rword "func" *> parens nameTypeP
      typeVars         <- optional (angles $ some typeConstraintP)
      args             <- parens' (bindP `sepBy` (comma <* sc))
      body             <- (symbol "=" *> topLevelExprP) <|> blockExprP

      let typeVars' = unmaybeList typeVars

      pure $ FuncDefn
        name
        funcType
        typeVars'
        args
        body

effectDefnP :: Parser FuncDefn
effectDefnP = do
  name     <- rword "effect" *> identifier
  typeVars <- optional (angles $ some typeConstraintP)
  args     <- parens' $ argP `sepBy` (comma <* sc)
  rtnType  <- optional $ symbol "->" *> openTypeExprP
  body     <- blockExprP

  let rtnType'  = fromMaybe (PrimTypeExpr Unit) rtnType
      funcType  = foldr argFold (EffectTypeExpr rtnType') args
      typeVars' = unmaybeList typeVars

  pure $ FuncDefn
    name
    funcType
    typeVars'
    (map fst args)
    body

-- Statements

ifStmtP :: Parser Statement
ifStmtP = rword "if" *> do
  cond1     <- condExpr
  then1     <- blockExprP
  rest      <- many elifExpr
  elseBlock <- optional (pair <$> rword "else" *> blockExprP)
  pure $ IfStmt (M.fromList $ (cond1, then1) : rest) elseBlock
  where
    condExpr = operatorP <|> chainExprP <|> exprP
    elifExpr = pair <$> (rword "else" *> rword "if" *> condExpr) <*> blockExprP

varDeclP :: Parser Statement
varDeclP = VarDecl
  <$> (rword "let" *> identifier)
  <*> optional (colon *> openTypeExprP)
  <*> (P.symbol sc "=" *> topLevelExprP)

assignStmtP :: Parser Statement
assignStmtP = AssignStmt
          <$> (chainExprP <|> exprP)
          <*> (P.symbol sc "=" *> topLevelExprP)

forLoopP :: Parser Statement
forLoopP = ForLoop
       <$> (rword "for" *> bindP)
       <*> (rword "in" *> chainExprP <|> exprP)
       <*> blockExprP

returnStmtP :: Parser Statement
returnStmtP = ReturnStmt <$> (rword "return" *> topLevelExprP)

blockP :: Parser [Statement]
blockP = braces' $ many statementP

statementP :: Parser Statement
statementP = parse <* sc
  where
    parse = ifStmtP
        <|> varDeclP
        <|> assignStmtP
        <|> returnStmtP
        <|> EffectStmt <$> effectRunP
        <|> forLoopP

-- Classes

classDefnP :: Parser ClassDefn
classDefnP = ClassDefn
  <$> (rword "class" *> typeIdentifier)
  <*> identifier
  <*> (M.fromList <$> manyInBraces funcSig)
  where
    funcSig :: Parser (String, TypeExpr)
    funcSig = rword "func" *> do
      (name, args, rtn) <- sig
      let args' = if null args then [PrimTypeExpr Unit] else args
          typ   = foldr FuncTypeExpr rtn args'
      pure (name, typ)

    effectSig :: Parser (String, TypeExpr)
    effectSig = rword "effect" *> do
      (name, args, rtn) <- sig
      let args' = if null args then [PrimTypeExpr Unit] else args
          typ   = foldr FuncTypeExpr (EffectTypeExpr rtn) args'
      pure (name, typ)

    sig :: Parser (String, [TypeExpr], TypeExpr)
    sig = do
      name <- identifier
      args <- parens' (openTypeExprP `sepBy` (comma <* sc))
      rtn  <- symbol "->" *> closedTypeExprP
      pure (name, args, rtn)

-- Membership

memberDefnP :: Parser MembershipDefn
memberDefnP = MembershipDefn
  <$> (rword "member" *> typeIdentifier)
  <*> typeIdentifier
  <*> manyInBraces impl
  where
    impl :: Parser MembershipImpl
    impl = do
      implType <- eitherP (rword "func") (rword "effect")
      name <- identifier
      args <- parens' (bindP `sepBy` (comma <* sc))
      body <- (P.symbol sc "=" *> topLevelExprP) <|> blockExprP
      pure $ MembershipImpl
        name
        args
        body
        (isRight implType)
