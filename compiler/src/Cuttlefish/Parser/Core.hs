module Cuttlefish.Parser.Core where

import Control.Applicative
import Control.Monad.Plus
import Data.Functor
import Data.Void

data ParseError = EndOfInput
                | Unexpected Char
                | Empty
                deriving Show

newtype Parser a = Parser { parse :: String -> Either ParseError (a, String) }

runParser :: Parser a -> String -> Either ParseError a
runParser p input =
  case parse p $ input of
    Left e       -> Left e
    Right (x, _) -> Right x

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Right (x, xs) -> Right (f x, xs)
      Left es       -> Left es

instance Applicative Parser where
  pure x    = Parser $ \input -> Right (x, input)
  Parser p1 <*> Parser p2 = Parser $ \input ->
    case p1 input of
      Left es           -> Left es
      Right (f, input') ->
        case p2 input' of
          Left es            -> Left es
          Right (x, input'') -> Right (f x, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Left Empty
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Left _ -> p2 input
      result -> result

instance Monad Parser where
  (Parser p) >>= f = Parser $ \input ->
    case p input of
      Left es           -> Left es
      Right (x, input') -> parse (f x) input'

instance MonadPlus Parser where
  mzero = empty

instance MonadFail Parser where
  fail _ = mzero


eof :: Parser ()
eof = Parser $ \input ->
  case input of
    []    -> Right ((), [])
    (x:_) -> Left (Unexpected x)

symbol :: Parser () -> String -> Parser String
symbol sc' s = sc' *> symbol' s

symbol' :: String -> Parser String
symbol' s = sequenceA $ map char s

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input ->
  case input of
    (x:xs)
      | pred x    -> Right (x, xs)
      | otherwise -> Left (Unexpected x)
    []            -> Left EndOfInput

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

notOneOf :: String -> Parser Char
notOneOf cs = satisfy (not . (`elem` cs))

sc :: Parser () -> Parser ()
sc comm = void $ many (comm <|> (void $ oneOf " \t\n"))

sc' :: Parser () -> Parser ()
sc' comm = void $ many (comm <|> (void $ oneOf " \t"))

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
anyChar = satisfy (const True)

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \input ->
  case consume input of
    Nothing -> Left EndOfInput
    Just x  -> Right (x, input)
  where
    consume [] = Nothing
    consume s =
      case parse p s of
        Left _       -> consume $ tail s
        Right (x, _) -> Just x

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \input ->
  case consume input of
    Nothing -> Right ((), input)
    Just x  -> Left Empty
  where
    consume [] = Nothing
    consume s =
      case parse p s of
        Left _       -> consume $ tail s
        Right (x, _) -> Just x
