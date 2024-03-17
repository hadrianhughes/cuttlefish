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

data ProgramRoot = RClassDefn      ClassDefn
                 | RTypeSig        TypeSig
                 | RDefn           Defn
                 | RMembershipDefn MembershipDefn
                 | RTypeDefn       TypeDefn

programRootsP :: Parser [ProgramRoot]
programRootsP = many $
                try (RClassDefn      <$> classDefnP)
            <|> try (RTypeDefn       <$> typeDefnP)
            <|> try (RTypeSig        <$> typeSigP)
            <|> try (RMembershipDefn <$> membershipP)
            <|> try (RDefn           <$> defnP)

sigs :: [ProgramRoot] -> [TypeSig]
sigs r = [s | RTypeSig s <- r]

defns :: [ProgramRoot] -> [Defn]
defns r = [d | RDefn d <- r]

typeDefns :: [ProgramRoot] -> [TypeDefn]
typeDefns r = [td | RTypeDefn td <- r]

classDefns :: [ProgramRoot] -> [ClassDefn]
classDefns r = [c | RClassDefn c <- r]

membershipDefns :: [ProgramRoot] -> [MembershipDefn]
membershipDefns r = [m | RMembershipDefn m <- r]

programP :: Parser Program
programP = between fsc eof $ do
  roots <- programRootsP
  return $ Program
    (sigs roots)
    (defns roots)
    (typeDefns roots)
    (classDefns roots)
    (membershipDefns roots)
