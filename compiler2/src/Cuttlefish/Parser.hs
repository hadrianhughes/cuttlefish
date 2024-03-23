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

programRootsP :: Parser [ProgramRoot]
programRootsP = many $
                try (RTypeDefn  <$> typeDefnP)
            <|> try (RConstDefn <$> constDefnP)

typeDefns :: [ProgramRoot] -> [TypeDefn]
typeDefns r = [td | RTypeDefn td <- r]

constDefns :: [ProgramRoot] -> [ConstDefn]
constDefns r = [c | RConstDefn c <- r]

programP :: Parser Program
programP = between sc eof $ do
  roots <- programRootsP
  return $ Program
    (typeDefns roots)
    (constDefns roots)
