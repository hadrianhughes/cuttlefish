module Cuttlefish.Parser
  ( programP
  , runParser
  , errorBundlePretty
  )
where

import Text.Megaparsec
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Types
import Cuttlefish.Ast

data ProgramRoot = RTypeDefn TypeDefn

programRootsP :: Parser [ProgramRoot]
programRootsP = many $ (RTypeDefn <$> typeDefnP)

typeDefns :: [ProgramRoot] -> [TypeDefn]
typeDefns r = [td | RTypeDefn td <- r]

programP :: Parser Program
programP = between sc eof $ do
  roots <- programRootsP
  return $ Program (typeDefns roots)
