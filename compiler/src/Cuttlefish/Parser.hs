module Cuttlefish.Parser
  ( programP
  , runParser
  , errorBundlePretty
  )
where

import Text.Megaparsec
import Cuttlefish.Parser.Body
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Types
import Cuttlefish.Ast

data ProgramRoot = RTypeDefn  TypeDefn
                 | RConstDefn ConstDefn
                 | RFuncDefn  FuncDefn
                 | RClassDefn ClassDefn

programRootsP :: Parser [ProgramRoot]
programRootsP = many $
                try (RTypeDefn  <$> typeDefnP)
            <|> try (RConstDefn <$> constDefnP)
            <|> try (RFuncDefn  <$> (try funcDefnP <|> try funcDefnP' <|> effectDefnP))
            <|> try (RClassDefn <$> classDefn)

typeDefns :: [ProgramRoot] -> [TypeDefn]
typeDefns r = [td | RTypeDefn td <- r]

constDefns :: [ProgramRoot] -> [ConstDefn]
constDefns r = [c | RConstDefn c <- r]

funcDefns :: [ProgramRoot] -> [FuncDefn]
funcDefns r = [f | RFuncDefn f <- r]

classDefns :: [ProgramRoot] -> [ClassDefn]
classDefns r = [c | RClassDefn c <- r]

programP :: Parser Program
programP = between fsc eof $ do
  roots <- programRootsP
  return $ Program
    (typeDefns  roots)
    (constDefns roots)
    (funcDefns  roots)
    (classDefns roots)
