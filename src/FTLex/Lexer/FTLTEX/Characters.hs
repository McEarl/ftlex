-- |
-- Module      : FTLex.Lexer.FTLTEX.Characters
-- Copyright   : (c) 2024-2025, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Character and category codes.

module FTLex.Lexer.FTLTEX.Characters (
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

import Data.Map.Strict qualified as Map
import FTLex.Lexer.Base (allowedChars)
import FTLex.Lexer.TEX.Characters hiding (defaultCatCodes)

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
    defCatCodes '\n' = EndOfLineCat
    defCatCodes '#' = ParamCharCat
    defCatCodes '^' = SuperscriptCat
    defCatCodes '_' = SubscriptCat
    defCatCodes ' ' = SpaceCat
    defCatCodes '\t' = SpaceCat
    defCatCodes '\r' = SpaceCat
    defCatCodes '\x00A0' = SpaceCat -- non-breakable space
    defCatCodes c
      | c `elem` ['A' .. 'Z'] = LetterCat
      | c `elem` ['a' .. 'z'] = LetterCat
      | c `elem` ['\x00C0' .. '\x00D6'] = LetterCat
      | c `elem` ['\x00D8' .. '\x00F6'] = LetterCat
      | c `elem` ['\x00F8' .. '\x00FF'] = LetterCat
    defCatCodes '~' = ActiveCat
    defCatCodes '%' = CommentPrefixCat
    defCatCodes '\NUL' = InvalidCat
    defCatCodes _ = OtherCat
