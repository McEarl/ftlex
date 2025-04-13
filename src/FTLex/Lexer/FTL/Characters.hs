-- |
-- Module      : FTLex.Lexer.FTL.Characters
-- Copyright   : (c) 2024-2025, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Characters that are allowed in the input.

module FTLex.Lexer.FTL.Characters (
  isSpaceChar,
  isLetterChar,
  isDigitChar,
  isAlphanumChar,
  isSymbolChar,
  isCommentPrefixChar,
  isUnknownChar
) where

import Data.Map (Map)
import Data.Map qualified as Map
import FTLex.Lexer.Base (allowedChars)
import FTLex.Helpers (disjunction)

-- | Category Codes.
data CatCode =
    SpaceCat          -- ^ Space
  | LetterCat         -- ^ Letter
  | DigitCat          -- ^ Digit
  | SymbolCat         -- ^ Symbol
  | CommentPrefixCat  -- ^ Comment Prefix
  | UnknownCat        -- ^ Unknown character
  deriving (Eq, Ord)

instance Show CatCode where
  show :: CatCode -> String
  show SpaceCat = "Space"
  show LetterCat = "Letter"
  show DigitCat = "Digit"
  show SymbolCat = "Symbol"
  show CommentPrefixCat = "Comment prefix"
  show UnknownCat = "Unknown character"

-- | A map that assigns a character a category code.
type CatCodeMap = Map Char CatCode

-- | Checks whether a character is a space character.
isSpaceChar :: Char -> Bool
isSpaceChar c = 
  Map.lookup c catCodes == Just SpaceCat

-- | Checks whether a character is a letter character.
isLetterChar :: Char -> Bool
isLetterChar c = 
  Map.lookup c catCodes == Just LetterCat

-- | Checks whether a character is a digit character.
isDigitChar :: Char -> Bool
isDigitChar c = 
  Map.lookup c catCodes == Just DigitCat

-- | Checks whether a character is an alpha-numerical character.
isAlphanumChar :: Char -> Bool
isAlphanumChar = disjunction [isLetterChar, isDigitChar]

-- | Checks whether a character is a symbol character.
isSymbolChar :: Char -> Bool
isSymbolChar c = 
  Map.lookup c catCodes == Just SymbolCat

-- | Checks whether a character is a comment prefix character.
isCommentPrefixChar :: Char -> Bool
isCommentPrefixChar c = 
  Map.lookup c catCodes == Just CommentPrefixCat

-- | Checks whether a character is an unknown character.
isUnknownChar :: Char -> Bool
isUnknownChar c = 
  Map.lookup c catCodes == Just UnknownCat

-- | Default category code mapping. Its domain is @allowedChars@; characters
-- not in the domain are detected by @isUnknownChar@.
catCodes :: CatCodeMap
catCodes = Map.fromSet defCatCodes allowedChars
  where
    defCatCodes ' ' = SpaceCat
    defCatCodes '\r' = SpaceCat
    defCatCodes '\n' = SpaceCat
    defCatCodes '\t' = SpaceCat
    defCatCodes '\x00A0' = SpaceCat -- non-breakable space
    defCatCodes c
      | c `elem` ['0' .. '9'] = DigitCat
    defCatCodes c
      | c `elem` ['A' .. 'Z'] = LetterCat
      | c `elem` ['a' .. 'z'] = LetterCat
      | c `elem` ['\x00C0' .. '\x00D6'] = LetterCat
      | c `elem` ['\x00D8' .. '\x00F6'] = LetterCat
      | c `elem` ['\x00F8' .. '\x00FF'] = LetterCat
    defCatCodes '#' = CommentPrefixCat
    defCatCodes _ = SymbolCat
