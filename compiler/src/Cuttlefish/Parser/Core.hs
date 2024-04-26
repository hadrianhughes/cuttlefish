module Cuttlefish.Parser.Core where

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
symbol s = sequenceA $ map char s

charIf :: (Char -> Bool) -> Parser Char
charIf pred = Parser $ \input ->
  case input of
    (x:xs) | pred x -> Just (x, xs)
    _               -> Nothing

oneOf :: String -> Parser Char
oneOf cs = charIf (`elem` cs)

charNotIn :: String -> Parser Char
charNotIn cs = charIf (not . (`elem` cs))

lineComment :: Parser ()
lineComment = void $ symbol "//" *> many (charNotIn "\n")

sc :: Parser ()
sc = void $ many (lineComment <|> (void $ oneOf " \t\n"))

sc' :: Parser ()
sc' = void $ many (lineComment <|> (void $ oneOf " \t"))

int :: Parser Int
int = read <$> (some digit)

digit :: Parser Char
digit = oneOf "0123456789"

float :: Parser Double
float = mkFloat <$> num <*> (char '.' *> num)
  where
    num = some digit
    mkFloat :: String -> String -> Double
    mkFloat l r = read $ l <> "." <> r

upperChar :: Parser Char
upperChar = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowerChar :: Parser Char
lowerChar = oneOf "abcdefghijklmnopqrstuvwxyz"

letterChar :: Parser Char
letterChar = upperChar <|> lowerChar

alphaNumChar :: Parser Char
alphaNumChar = letterChar <|> digit

anyChar :: Parser Char
anyChar = charIf (const True)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy' p sep <|> pure []

sepBy' :: Parser a -> Parser sep -> Parser [a]
sepBy' p sep = (:) <$> p <*> many (sep *> p)

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \input ->
  case consume input of
    Nothing -> Nothing
    Just x  -> Just (x, input)
  where
    consume [] = Nothing
    consume s =
      case parse p s of
        Nothing -> consume $ tail s
        Just (x, _) -> Just x

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \input ->
  case consume input of
    Nothing -> Just ((), input)
    Just _  -> Nothing
  where
    consume [] = Nothing
    consume s =
      case parse p s of
        Nothing     -> consume $ tail s
        Just (x, _) -> Just x
