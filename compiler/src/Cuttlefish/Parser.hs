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

programP :: Parser Program
programP = between sc eof $ Program
  <$> many typeDefnP
  <*> many typeSigP
