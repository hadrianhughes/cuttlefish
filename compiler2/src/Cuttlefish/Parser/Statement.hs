module Cuttlefish.Parser.Statement where

import Text.Megaparsec
import Text.Megaparsec.Char
import Cuttlefish.Ast
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Expr
import Cuttlefish.Parser.Types

ifStmtP :: Parser Statement
ifStmtP = IfStmt
  <$> (rword "if" *> try chainExprP <|> exprP)
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
statementP = ifStmtP
         <|> varDeclP
         <|> try assignStmtP
         <|> try exprStmtP
         <|> forLoopP
         <|> returnStmtP
