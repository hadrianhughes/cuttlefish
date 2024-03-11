module Parser where

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text
import           Data.Void

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
