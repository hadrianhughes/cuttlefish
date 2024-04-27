module Cuttlefish.Parser.Body where

import           Data.Char
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Either
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import           Control.Monad ( void )
import           Cuttlefish.Parser.Ast
import           Cuttlefish.Parser.Core
import           Cuttlefish.Parser.Types
import           Cuttlefish.Parser.Utils

-- Expressions

data ChainTerm = StructTerm Text
               | ListTerm   Expr
               | FuncTerm   [Expr]

data BinopTerm = BinopTerm Text Expr

chainAcc :: Expr -> ChainTerm -> Expr
chainAcc e = \case
  (StructTerm field) -> StructAccess e field
  (ListTerm index)   -> ListAccess e index
  (FuncTerm args)    -> FuncCall e args

chainExprP :: Parser Expr
chainExprP = do
  start <- try exprP
  terms <- many term
  pure $ foldl chainAcc start terms
  where
    term :: Parser ChainTerm
    term = StructTerm   <$> (dot *> identifier)
       <|> ListTerm     <$> brackets fsc topLevelExprP
       <|> FuncTerm     <$> (parens fsc $ topLevelExprP `sepBy` (comma <* fsc))

operatorP :: Parser Expr
operatorP = do
  start <- try chainExprP <|> exprP
  terms <- many $ BinopTerm <$> binop <*> (try blockExprP <|> try chainExprP <|> exprP)
  pure $ foldr (\(BinopTerm op arg2) expr -> FuncCall (VarRef op) [expr, arg2]) start terms

literalP :: Parser Expr
literalP = try (FloatLit <$> float)
       <|> IntLit   <$> int
       <|> CharLit  <$> squotes (satisfy (`notElem` ['\\', '\'']) <|> single '\\')
       <|> StrLit   <$> dquotes (takeWhileP Nothing (/= '"'))
       <|> (L.symbol hsc "()" *> pure UnitLit)

ifExprP :: Parser Expr
ifExprP = rword "if" *> do
  cond1     <- condExpr
  then1     <- rword "then" *> nestedExpr <* fsc
  rest      <- many (try elifExpr)
  elseBlock <- rword "else" *> nestedExpr
  pure $ IfExpr (M.fromList $ (cond1, then1) : rest) elseBlock
  where
    condExpr   = try operatorP <|> try chainExprP <|> exprP <* fsc
    nestedExpr = try blockExprP <|> try operatorP <|> try chainExprP <|> exprP
    elifExpr   = pair <$> (rword "else" *> rword "if" *> condExpr) <*> (rword "then" *> nestedExpr)

matchExprP :: Parser Expr
matchExprP = MatchExpr
  <$> (rword "match" *> chainExprP)
  <*> (M.fromList <$> braces fsc (caseP `sepBy1` (comma <* fsc)))
  where
    caseP = pair <$> bindP <*> (L.symbol fsc "->" *> topLevelExprP)

listExprP :: Parser Expr
listExprP = ListExpr <$> brackets fsc (topLevelExprP `sepBy` comma)

tupleExprP :: Parser Expr
tupleExprP = TupleExpr <$> parens fsc (topLevelExprP `sepBy2` comma)

structExprP :: Parser Expr
structExprP = (StructExpr . M.fromList) <$> braces fsc (keyValPair `sepBy` comma <* fsc)
  where
    keyValPair = pair <$> (identifier <* colon <* fsc) <*> exprP

varRefP :: Parser Expr
varRefP = VarRef <$> identifier

exprP :: Parser Expr
exprP = try listExprP
    <|> try tupleExprP
    <|> try structExprP
    <|> try matchExprP
    <|> try (parens fsc topLevelExprP)
    <|> try varRefP
    <|> literalP

blockExprP :: Parser Expr
blockExprP = BlockExpr <$> blockP

effectRunP :: Parser Expr
effectRunP = EffectRun <$> expr <* L.symbol hsc "!"
  where
    expr = try chainExprP <|> (VarRef <$> identifier)

topLevelExprP :: Parser Expr
topLevelExprP = try blockExprP
            <|> try ifExprP
            <|> try operatorP
            <|> try effectRunP
            <|> try chainExprP
            <|> exprP

bindP :: Parser Bind
bindP = try (TupleBind <$> parens hsc (bindP `sepBy1` comma))
    <|> ConstructorBind
      <$> typeIdentifier
      <*> (unmaybeList <$> optional (parens hsc (identifier `sepBy1` comma)))
    <|> SimpleBind <$> identifier

constDefnP :: Parser ConstDefn
constDefnP = const <* fsc
  where
    const = ConstDefn
        <$> (rword "let" *> identifier)
        <*> optional (colon *> openTypeExprP)
        <*> (L.symbol fsc "=" *> topLevelExprP)

-- Functions

argP :: Parser (Bind, TypeExpr)
argP = pair <$> bindP <*> (colon *> openTypeExprP)

argFold :: (a, TypeExpr) -> TypeExpr -> TypeExpr
argFold (_, t1) t2 = FuncTypeExpr t1 t2

funcDefnP :: Parser FuncDefn
funcDefnP = parse <* fsc
  where
    parse = do
      name     <- rword "func" *> identifier
      typeVars <- optional (angles hsc $ some typeConstraintP)
      args     <- parens fsc $ argP `sepBy` (comma <* fsc)
      rtnType  <- optional $ L.symbol hsc "->" *> openTypeExprP
      body     <- (L.symbol fsc "=" *> topLevelExprP) <|> blockExprP

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
funcDefnP' = parse <* fsc
  where
    nameTypeP = pair <$> identifier <*> (colon *> openTypeExprP)
    parse = do
      (name, funcType) <- rword "func" *> parens hsc nameTypeP
      typeVars         <- optional (angles hsc $ some typeConstraintP)
      args             <- parens fsc (bindP `sepBy` (comma <* fsc))
      body             <- (L.symbol hsc "=" *> topLevelExprP) <|> blockExprP

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
  typeVars <- optional (angles hsc $ some typeConstraintP)
  args     <- parens fsc $ argP `sepBy` (comma <* fsc)
  rtnType  <- optional $ L.symbol hsc "->" *> openTypeExprP
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
  rest      <- many (try elifExpr)
  elseBlock <- optional (pair <$> rword "else" *> blockExprP)
  pure $ IfStmt (M.fromList $ (cond1, then1) : rest) elseBlock
  where
    condExpr = try operatorP <|> try chainExprP <|> exprP
    elifExpr = pair <$> (rword "else" *> rword "if" *> condExpr) <*> blockExprP

varDeclP :: Parser Statement
varDeclP = VarDecl
  <$> (rword "let" *> identifier)
  <*> optional (colon *> openTypeExprP)
  <*> (L.symbol fsc "=" *> topLevelExprP)

assignStmtP :: Parser Statement
assignStmtP = AssignStmt
          <$> (try chainExprP <|> exprP)
          <*> (L.symbol fsc "=" *> topLevelExprP)

forLoopP :: Parser Statement
forLoopP = ForLoop
       <$> (rword "for" *> bindP)
       <*> (rword "in" *> (try chainExprP <|> exprP))
       <*> blockExprP

returnStmtP :: Parser Statement
returnStmtP = ReturnStmt <$> (rword "return" *> topLevelExprP)

blockP :: Parser [Statement]
blockP = braces fsc (many statementP)

statementP :: Parser Statement
statementP = parse <* fsc
  where
    parse = try ifStmtP
        <|> try varDeclP
        <|> try assignStmtP
        <|> try returnStmtP
        <|> try (EffectStmt <$> effectRunP)
        <|> try forLoopP

-- Classes

classDefnP :: Parser ClassDefn
classDefnP = ClassDefn
  <$> (rword "class" *> typeIdentifier)
  <*> identifier
  <*> optional (rword "extends" *> typeIdentifier)
  <*> (M.fromList <$> manyInBraces funcSig)
  where
    funcSig :: Parser (Text, TypeExpr)
    funcSig = rword "func" *> do
      (name, args, rtn) <- sig
      let args' = if null args then [PrimTypeExpr Unit] else args
          typ   = foldr FuncTypeExpr rtn args'
      pure (name, typ)

    effectSig :: Parser (Text, TypeExpr)
    effectSig = rword "effect" *> do
      (name, args, rtn) <- sig
      let args' = if null args then [PrimTypeExpr Unit] else args
          typ   = foldr FuncTypeExpr (EffectTypeExpr rtn) args'
      pure (name, typ)

    sig :: Parser (Text, [TypeExpr], TypeExpr)
    sig = do
      name <- identifier
      args <- parens fsc (openTypeExprP `sepBy` (comma <* fsc))
      rtn  <- L.symbol hsc "->" *> closedTypeExprP
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
      args <- parens fsc (bindP `sepBy` (comma <* fsc))
      body <- (L.symbol fsc "=" *> topLevelExprP) <|> blockExprP
      pure $ MembershipImpl
        name
        args
        body
        (isRight implType)
