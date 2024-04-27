module Cuttlefish.Parser
  ( programP
  , runParser
  )
where

import Control.Applicative
import Control.Applicative.Combinators
import Cuttlefish.Parser.Body
import Cuttlefish.Parser.Core (Parser, eof, runParser)
import Cuttlefish.Parser.Lexer
import Cuttlefish.Parser.Types
import Cuttlefish.Parser.Ast

data ProgramRoot = RTypeDefn   TypeDefn
                 | RConstDefn  ConstDefn
                 | RFuncDefn   FuncDefn
                 | RClassDefn  ClassDefn
                 | RMemberDefn MembershipDefn

programRootsP :: Parser [ProgramRoot]
programRootsP = many $ RTypeDefn   <$> typeDefnP
                   <|> RConstDefn  <$> constDefnP
                   <|> RFuncDefn   <$> (funcDefnP <|> funcDefnP' <|> effectDefnP)
                   <|> RClassDefn  <$> classDefnP
                   <|> RMemberDefn <$> memberDefnP

typeDefns :: [ProgramRoot] -> [TypeDefn]
typeDefns r = [td | RTypeDefn td <- r]

constDefns :: [ProgramRoot] -> [ConstDefn]
constDefns r = [c | RConstDefn c <- r]

funcDefns :: [ProgramRoot] -> [FuncDefn]
funcDefns r = [f | RFuncDefn f <- r]

classDefns :: [ProgramRoot] -> [ClassDefn]
classDefns r = [c | RClassDefn c <- r]

memberDefns :: [ProgramRoot] -> [MembershipDefn]
memberDefns r = [m | RMemberDefn m <- r]

programP :: Parser Program
programP = between sc eof $ do
  roots <- programRootsP
  pure $ Program
    (typeDefns   roots)
    (constDefns  roots)
    (funcDefns   roots)
    (classDefns  roots)
    (memberDefns roots)
