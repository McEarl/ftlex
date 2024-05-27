-- |
-- Creator: Marcel Schütz (2024)
--
-- Position type class

module Flex.Position (Pos(..)) where

import Data.Text.Lazy (Text)

class (Ord p) => Pos p where
  noPos :: p
  -- ^ No position
  explodeString :: String -> p -> p
  -- ^ Take a string together with its starting position and return the ending
  -- position of that string
  getStringPos :: String -> p -> p
  -- ^ Take a string together with its starting position and return the position
  -- of the whole string
  dropText :: p -> Text -> Text
  -- ^ @dropText pos text@ drops everyghing before a position @pos@ in a text
  -- @text@
