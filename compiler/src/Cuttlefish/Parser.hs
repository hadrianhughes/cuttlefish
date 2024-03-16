module Cuttlefish.Parser
  ( programP
  , runParser
  , errorBundlePretty
  )
where

import Data.Either
import Text.Megaparsec
import Cuttlefish.Parser.Core
import Cuttlefish.Parser.Types
import Cuttlefish.Parser.Body
import Cuttlefish.Ast

data ProgramRoot = RTypeSig TypeSig
                 | RDefn Defn
                 | RTypeDefn TypeDefn

programRootsP :: Parser [ProgramRoot]
programRootsP = many $ try (RTypeDefn <$> typeDefnP)
            <|> try (RTypeSig <$> typeSigP)
            <|> try (RDefn <$> defnP)

sigs :: [ProgramRoot] -> [TypeSig]
sigs r = [s | RTypeSig s <- r]

defns :: [ProgramRoot] -> [Defn]
defns r = [d | RDefn d <- r]

typeDefns :: [ProgramRoot] -> [TypeDefn]
typeDefns r = [td | RTypeDefn td <- r]

programP :: Parser Program
programP = between sc eof $ do
  roots <- programRootsP
  return $ Program (sigs roots) (defns roots) (typeDefns roots)
