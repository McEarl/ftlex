-- |
-- Creator: Marcel Schütz (2024)
--
-- Helpers.

{-# LANGUAGE OverloadedStrings #-}

module Flex.Helpers (
  disjunction,
  codePoint
) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Char qualified as Char

-- | The disjunction of a list of predicates.
disjunction :: [a -> Bool] -> a -> Bool
disjunction ps x = foldr (\ p -> (||) (p x)) False ps

-- | Represent an integer as a hexadecimal number.
toHex :: Int -> Text
toHex n = let r = n `mod` 16 in
  if n - r == 0
  then Text.singleton (digits !! r)
  else toHex((n - r) `div` 16) <> Text.singleton (digits !! r)
  where
    digits = "0123456789ABCDEF"

-- | The Unicode code point of a character, e.g. @U+004C@.
codePoint :: Char -> Text
codePoint c = "U+" <> (Text.justifyRight 4 '0' . toHex . Char.ord) c
