-- |
-- Creator: Marcel Schütz (2024)
--
-- Position type class

module Flex.Position (Pos(..)) where

class (Ord p) => Pos p where
  noPos :: p
  -- ^ No position
  explodeString :: String -> p -> p
  -- ^ Take a string together with its starting position and return the ending
  -- position of that string
  getStringPos :: String -> p -> p
  -- ^ Take a string together with its starting position and return the position
  -- of the whole string
