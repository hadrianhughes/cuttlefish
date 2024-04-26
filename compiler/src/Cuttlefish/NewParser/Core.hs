module Cuttlefish.NewParser.Core where

import Control.Applicative
import Data.Functor

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (x, xs) -> Just (f x, xs)
      Nothing      -> Nothing

instance Applicative Parser where
  pure x    = Parser $ \input -> Just (x, input)
  Parser p1 <*> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing          -> Nothing
      Just (f, input') ->
        case p2 input' of
          Nothing           -> Nothing
          Just (x, input'') -> Just (f x, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing -> p2 input
      result  -> result


charP :: Char -> Parser Char
charP c = Parser $ \input ->
  case input of
    (x:xs) | x == c -> Just (x, xs)
    _               -> Nothing

symbolP :: String -> Parser String
symbolP s = sequenceA $ map charP s

charIfP :: (Char -> Bool) -> Parser Char
charIfP pred = Parser $ \input ->
  case input of
    (x:xs) | pred x -> Just (x, xs)
    _               -> Nothing

charInP :: String -> Parser Char
charInP cs = charIfP (`elem` cs)

charNotInP :: String -> Parser Char
charNotInP cs = charIfP (not . (`elem` cs))

lineComment :: Parser ()
lineComment = void $ symbolP "//" *> many (charNotInP "\n")

sc :: Parser ()
sc = void $ many (lineComment <|> (void $ charInP " \t\n"))

sc' :: Parser ()
sc' = void $ many (lineComment <|> (void $ charInP " \t"))

intP :: Parser Int
intP = read <$> (some $ charInP "0123456789")

floatP :: Parser Double
floatP = mkFloat <$> numP <*> (charP '.' *> numP)
  where
    numP = some $ charInP "0123456789"
    mkFloat :: String -> String -> Double
    mkFloat l r = read $ l <> "." <> r
