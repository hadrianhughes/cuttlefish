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
