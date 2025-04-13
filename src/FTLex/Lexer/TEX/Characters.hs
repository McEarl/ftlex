-- |
-- Module      : FTLex.Lexer.TEX.Characters
-- Copyright   : (c) 2024-2025, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Character and category codes.

module FTLex.Lexer.TEX.Characters (
  CatCode(..),
  CatCodeMap,
  isEscapeChar,
  isBeginGroupChar,
  isEndGroupChar,
  isMathShiftChar,
  isAlignTab,
  isEndOfLine,
  isParamChar,
  isSuperscriptChar,
  isSubscriptChar,
  isIgnoredChar,
  isSpace,
  isLetter,
  isOtherChar,
  isActiveChar,
  isCommentPrefix,
  isInvalidChar,
  isUnknownChar,
  defaultCatCodes
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import FTLex.Lexer.Base (allowedChars)


-- * Category Codes

-- | Category Codes.
data CatCode =
    EscapeCharCat     -- ^  0: Escape character
  | BeginGroupCat     -- ^  1: Begin group character
  | EndGroupCat       -- ^  2: End group character
  | MathShiftCat      -- ^  3: Math shift character
  | AlignTabCat       -- ^  4: Alignment tab
  | EndOfLineCat      -- ^  5: Line break
  | ParamCharCat      -- ^  6: Parameter character
  | SuperscriptCat    -- ^  7: Superscript character
  | SubscriptCat      -- ^  8: Subscript character
  | IgnoredCat        -- ^  9: Ignored character
  | SpaceCat          -- ^ 10: Horizontal space
  | LetterCat         -- ^ 11: Letter
  | OtherCat          -- ^ 12: Other character
  | ActiveCat         -- ^ 13: Active character
  | CommentPrefixCat  -- ^ 14: Comment Prefix
  | InvalidCat        -- ^ 15: Invalid character
  | UnknownCat        -- ^     Unknown character
  deriving (Eq, Ord)

instance Show CatCode where
  show :: CatCode -> String
  show EscapeCharCat = "Escape character (0)"
  show BeginGroupCat = "Begin group character (1)"
  show EndGroupCat = "End group character (2)"
  show MathShiftCat = "Math shift character (3)"
  show AlignTabCat = "Alignment tab (4)"
  show EndOfLineCat = "Line break (5)"
  show ParamCharCat = "Parameter character (6)"
  show SuperscriptCat = "Superscript character (7)"
  show SubscriptCat = "Subscript character (8)"
  show IgnoredCat = "Ignored character (9)"
  show SpaceCat = "Horizontal space (10)"
  show LetterCat = "Letter (11)"
  show OtherCat = "Other character (12)"
  show ActiveCat = "Active character (13)"
  show CommentPrefixCat = "Comment prefix (14)"
  show InvalidCat = "Invalid character (15)"
  show UnknownCat = "Unknown character"

-- | A map that assigns a character a category code.
type CatCodeMap = Map Char CatCode

-- | Checks whether a character is an escape character wrt. a given category
-- code mapping.
isEscapeChar :: CatCodeMap -> Char -> Bool
isEscapeChar catCodeMap c =
  Map.lookup c catCodeMap == Just EscapeCharCat

-- | Checks whether a character is a begin group character wrt. a given
-- category code mapping.
isBeginGroupChar :: CatCodeMap -> Char -> Bool
isBeginGroupChar catCodeMap c =
  Map.lookup c catCodeMap == Just BeginGroupCat

-- | Checks whether a character is an end group character wrt. a given category
-- code mapping.
isEndGroupChar :: CatCodeMap -> Char -> Bool
isEndGroupChar catCodeMap c =
  Map.lookup c catCodeMap == Just EndGroupCat

-- | Checks whether a character is a math shift character wrt. a given category
-- code mapping.
isMathShiftChar :: CatCodeMap -> Char -> Bool
isMathShiftChar catCodeMap c =
  Map.lookup c catCodeMap == Just MathShiftCat

-- | Checks whether a character is an alignment tab wrt. a given category code
-- mapping.
isAlignTab :: CatCodeMap -> Char -> Bool
isAlignTab catCodeMap c =
  Map.lookup c catCodeMap == Just AlignTabCat

-- | Checks whether a character is a line break character wrt. a given category
-- code mapping.
isEndOfLine :: CatCodeMap -> Char -> Bool
isEndOfLine catCodeMap c =
  Map.lookup c catCodeMap == Just EndOfLineCat

-- | Checks whether a character is a parameter character wrt. a given category
-- code mapping.
isParamChar :: CatCodeMap -> Char -> Bool
isParamChar catCodeMap c =
  Map.lookup c catCodeMap == Just ParamCharCat

-- | Checks whether a character is a superscript character wrt. a given
-- category code mapping.
isSuperscriptChar :: CatCodeMap -> Char -> Bool
isSuperscriptChar catCodeMap c =
  Map.lookup c catCodeMap == Just SuperscriptCat

-- | Checks whether a character is a subscript character wrt. a given category
-- code mapping.
isSubscriptChar :: CatCodeMap -> Char -> Bool
isSubscriptChar catCodeMap c =
  Map.lookup c catCodeMap == Just SubscriptCat

-- | Checks whether a character is an ignored character wrt. a given category
-- code mapping.
isIgnoredChar :: CatCodeMap -> Char -> Bool
isIgnoredChar catCodeMap c =
  Map.lookup c catCodeMap == Just IgnoredCat

-- | Checks whether a character is a space wrt. a given category code mapping.
isSpace :: CatCodeMap -> Char -> Bool
isSpace catCodeMap c =
  Map.lookup c catCodeMap == Just SpaceCat

-- | Checks whether a character is a letter wrt. a given category code mapping.
isLetter :: CatCodeMap -> Char -> Bool
isLetter catCodeMap c =
  Map.lookup c catCodeMap == Just LetterCat

-- | Checks whether a character is an other character wrt. a given category
-- code mapping.
isOtherChar :: CatCodeMap -> Char -> Bool
isOtherChar catCodeMap c =
  Map.lookup c catCodeMap == Just OtherCat

-- | Checks whether a character is an active character wrt. a given category
-- code mapping.
isActiveChar :: CatCodeMap -> Char -> Bool
isActiveChar catCodeMap c =
  Map.lookup c catCodeMap == Just ActiveCat

-- | Checks whether a character is a comment prefix wrt. a given category code
-- mapping.
isCommentPrefix :: CatCodeMap -> Char -> Bool
isCommentPrefix catCodeMap c =
  Map.lookup c catCodeMap == Just CommentPrefixCat

-- | Checks whether a character is an invalid character wrt. a given category
-- code mapping.
isInvalidChar :: CatCodeMap -> Char -> Bool
isInvalidChar catCodeMap c =
  Map.lookup c catCodeMap == Just InvalidCat

-- | Checks whether a character is an unknown character wrt. a given category
-- code mapping.
isUnknownChar :: CatCodeMap -> Char -> Bool
isUnknownChar catCodeMap c = isNothing $ Map.lookup c catCodeMap

-- | Default category code mapping. Its domain is @allowedChars@; characters
-- not in the domain are detected by @isUnknownChar@.
defaultCatCodes :: CatCodeMap
defaultCatCodes = Map.fromSet defCatCodes allowedChars
  where
    defCatCodes '\\' = EscapeCharCat
    defCatCodes '{' = BeginGroupCat
    defCatCodes '}' = EndGroupCat
    defCatCodes '$' = MathShiftCat
    defCatCodes '&' = AlignTabCat
    defCatCodes '\r' = EndOfLineCat
    defCatCodes '#' = ParamCharCat
    defCatCodes '^' = SuperscriptCat
    defCatCodes '_' = SubscriptCat
    defCatCodes ' ' = SpaceCat
    defCatCodes c | c `elem` ['A' .. 'Z'] = LetterCat
    defCatCodes c | c `elem` ['a' .. 'z'] = LetterCat
    defCatCodes '~' = ActiveCat
    defCatCodes '%' = CommentPrefixCat
    defCatCodes '\NUL' = IgnoredCat
    defCatCodes '\DEL' = InvalidCat
    defCatCodes _ = OtherCat
