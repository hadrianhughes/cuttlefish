module Cuttlefish.NewParser.Core where

data Parser a = Parser { parse :: String -> Maybe (a, String) }

charP :: Char -> Parser Char
charP c = Parser $ \input ->
  case input of
    (x:xs) | x == c -> Just (x, xs)
    _               -> Nothing
