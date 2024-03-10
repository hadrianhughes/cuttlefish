module Parser where

import Text.Megaparsec (Parsec)
import Data.Text
import Data.Void

type Parser = Parsec Void Text
