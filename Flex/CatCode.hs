-- |
-- Creator: Marcel Schütz (2024)
--
-- Category Codes

module Flex.CatCode (
  CatCode(..),
  CatCodeMap,
  isSpace,
  isLineBreak,
  isAlphanumChar,
  isSymbol,
  isCommentChar,
  isInvalidChar
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)


data CatCode =
    SpaceCat
  | LineBreakCat
  | AlphaNumCat
  | SymbolCat
  | CommentPrefixCat
  | InvalidCat
  deriving Eq

type CatCodeMap = Map.Map Char CatCode

-- | Checks whether a character is a space wrt. a given category code mapping
-- (default: @ @).
isSpace :: CatCodeMap -> Char -> Bool
isSpace catCodeMap c =
  Map.lookup c catCodeMap == Just SpaceCat

-- | Checks whether a character is a line break character wrt. a given category 
-- code mapping (default: @\\CR@).
isLineBreak :: CatCodeMap -> Char -> Bool
isLineBreak catCodeMap c =
  Map.lookup c catCodeMap == Just LineBreakCat

-- | Checks whether a character is an alpha-numeric character wrt. a given 
-- category code mapping (default: any ASCII letter or ASCII digit).
isAlphanumChar :: CatCodeMap -> Char -> Bool
isAlphanumChar catCodeMap c =
  Map.lookup c catCodeMap == Just AlphaNumCat

-- | Checks whether a character is a symbol wrt. a given category code mapping
-- (default: any ASCII character that is neither a control character, nor a
-- letter, nor a digit, nor a space, nor @#@).
isSymbol :: CatCodeMap -> Char -> Bool
isSymbol catCodeMap c =
  Map.lookup c catCodeMap == Just SymbolCat

-- | Checks whether a character is a comment prefix character wrt. a given
-- category code mapping (default: @%@).
isCommentChar :: CatCodeMap -> Char -> Bool
isCommentChar catCodeMap c =
  Map.lookup c catCodeMap == Just CommentPrefixCat

-- | Checks whether a character is invalid wrt. a given category code mapping
-- (default: any non-ASCII character and any ASCII control character except
-- @\\CR@).
isInvalidChar :: CatCodeMap -> Char -> Bool
isInvalidChar catCodeMap c =
     Map.lookup c catCodeMap == Just InvalidCat
  || isNothing (Map.lookup c catCodeMap)
