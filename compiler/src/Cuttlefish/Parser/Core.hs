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


char :: Char -> Parser Char
char c = Parser $ \input ->
  case input of
    (x:xs) | x == c -> Just (x, xs)
    _               -> Nothing

symbol :: String -> Parser String
symbol s = sequenceA $ map charP s

charIf :: (Char -> Bool) -> Parser Char
charIf pred = Parser $ \input ->
  case input of
    (x:xs) | pred x -> Just (x, xs)
    _               -> Nothing

oneOf :: String -> Parser Char
oneOf cs = charIfP (`elem` cs)

charNotIn :: String -> Parser Char
charNotIn cs = charIfP (not . (`elem` cs))

lineComment :: Parser ()
lineComment = void $ symbolP "//" *> many (charNotInP "\n")

sc :: Parser ()
sc = void $ many (lineComment <|> (void $ oneOfP " \t\n"))

sc' :: Parser ()
sc' = void $ many (lineComment <|> (void $ oneOfP " \t"))

int :: Parser Int
int = read <$> (some digitP)

digit :: Parser Char
digit = oneOfP "0123456789"

float :: Parser Double
float = mkFloat <$> numP <*> (charP '.' *> numP)
  where
    numP = some digitP
    mkFloat :: String -> String -> Double
    mkFloat l r = read $ l <> "." <> r

upperChar :: Parser Char
upperChar = oneOfP "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowerChar :: Parser Char
lowerChar = oneOfP "abcdefghijklmnopqrstuvwxyz"

letterChar :: Parser Char
letterChar = upperCharP <|> lowerCharP

alphaNumChar :: Parser Char
alphaNumChar = letterCharP <|> digitP

between :: Parser open -> Parser close -> Parser a -> Parser a
between o c p = o *> p <* c

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy' p sep <|> pure []

sepBy' :: Parser a -> Parser sep -> Parser [a]
sepBy' p sep = (:) <$> p <*> many (sep *> p)
