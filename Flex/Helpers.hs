-- |
-- Creator: Marcel Schütz (2024)
--
-- Helpers.

{-# LANGUAGE OverloadedStrings #-}

module Flex.Helpers (
  disjunction,
  negation,
  neitherNor,
  toHex,
  fromHex,
  codePoint,
  singleton,
  pair
) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Char qualified as Char

-- | The disjunction of a list of predicates.
disjunction :: [a -> Bool] -> a -> Bool
disjunction ps x = foldr (\ p -> (||) (p x)) False ps

negation :: (a -> Bool) -> a -> Bool
negation p x = not (p x)

neitherNor :: [a -> Bool] -> a -> Bool
neitherNor ps x = foldr (\ p -> (&&) (not (p x))) True ps

-- | Represent an integer as a hexadecimal number.
toHex :: Int -> Text
toHex n = let r = n `mod` 16 in
  if n - r == 0
  then Text.singleton (digits !! r)
  else toHex((n - r) `div` 16) <> Text.singleton (digits !! r)
  where
    digits = "0123456789ABCDEF"

-- | Turns a text @txt@ that represents a hexadecimal number into an integer.
-- Assumes that @txt@ only consists of the characters @0..9@, @a..f@ and @A..F@.
fromHex :: Text -> Int
fromHex text =
  let strippedText = Text.dropWhile (== '0') text
      n = Text.length strippedText
  in case Text.uncons strippedText of
      Nothing -> 0
      Just (c, cs) -> (16^(n - 1) * makeInt c) + fromHex cs
  where
    makeInt '0' = 0
    makeInt '1' = 1
    makeInt '2' = 2
    makeInt '3' = 3
    makeInt '4' = 4
    makeInt '5' = 5
    makeInt '6' = 6
    makeInt '7' = 7
    makeInt '8' = 8
    makeInt '9' = 9
    makeInt 'a' = 10
    makeInt 'A' = 10
    makeInt 'b' = 11
    makeInt 'B' = 11
    makeInt 'c' = 12
    makeInt 'C' = 12
    makeInt 'd' = 13
    makeInt 'D' = 13
    makeInt 'e' = 14
    makeInt 'E' = 14
    makeInt 'f' = 15
    makeInt 'F' = 15
    makeInt _ = undefined

-- | The Unicode code point of a character, e.g. @U+004C@.
codePoint :: Char -> Text
codePoint c = "U+" <> (Text.justifyRight 4 '0' . toHex . Char.ord) c

-- | A singleton list.
singleton :: a -> [a]
singleton x = [x]

-- | A pair.
pair :: a -> b -> (a, b)
pair x y = (x, y)
