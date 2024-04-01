module Cuttlefish.Utils where

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ []     = Nothing
firstJust f (x:xs) =
  case f x of
    j@(Just _) -> j
    Nothing    -> firstJust f xs
