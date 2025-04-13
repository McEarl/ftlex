-- |
-- Module      : FTLex.Lexer.Lexemes
-- Copyright   : (c) 2024-2025, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Lexemes.

module FTLex.Lexer.TEX.Lexemes (
  Lexeme(..),
  isCharacterLexeme,
  isControlWordLexeme,
  isControlSymbolLexeme,
  isControlSpaceLexeme,
  isParameterLexeme,
  isSkippedLexeme,
  isCommentLexeme,
  isEscapeCharLexeme,
  isBeginGroupCharLexeme,
  isEndGroupCharLexeme,
  isMathShiftCharLexeme,
  isAlignTabCharLexeme,
  isEndOfLineCharLexeme,
  isParamCharLexeme,
  isSuperscriptCharLexeme,
  isSubscriptCharLexeme,
  isIgnoredCharLexeme,
  isSpaceCharLexeme,
  isLetterCharLexeme,
  isOtherCharLexeme,
  isActiveCharLexeme,
  isCommentPrefixCharLexeme,
  isInvalidCharLexeme
) where

import Data.Text (Text)
import FTLex.Lexer.TEX.Characters
import FTLex.Position


-- * Lexemes

-- | TeX lexemes: TeX tokens (with additional source text and source position
-- information) plus two additional constructors for skipped characters and
-- comments.
data (Pos p) => Lexeme p =
    Character{
      charContent :: Char,
      charCatCode :: CatCode,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Character token
  | ControlWord{
      ctrlWordContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Control word
  | ControlSymbol{
      ctrlSymbolContent :: Char,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Control symbol
  | ControlSpace{
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Control space
  | Parameter{
      paramNumber :: Int,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Parameter token
  | Skipped{
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Skipped characters
  | Comment{
      commentContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Comment
  deriving (Eq, Ord)

instance (Pos p) => Show (Lexeme p) where
  show :: Pos p => Lexeme p -> String
  show (Character content catCode sourceText sourcePos) =
    "Character {\n" ++
    "    Content         : " ++ show content ++ "\n" ++
    "    Category Code   : " ++ show catCode ++ "\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (ControlWord content sourceText sourcePos) =
    "Control Word {\n" ++
    "    Content         : " ++ show content ++ "\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (ControlSymbol content sourceText sourcePos) =
    "Control Symbol {\n" ++
    "    Content         : " ++ show content ++ "\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (ControlSpace sourceText sourcePos) =
    "Control Space {\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (Parameter number sourceText sourcePos) =
    "Parameter {\n" ++
    "    Content         : " ++ show number ++ "\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (Skipped sourceText sourcePos) =
    "Skipped Characters [\n" ++
    "  Source Text     : " ++ show sourceText ++ "\n" ++
    "  Source Position : " ++ show sourcePos ++ "\n" ++
    "]"
  show (Comment content sourceText sourcePos) =
    "Comment [\n" ++
    "  Content         : " ++ show content ++ "\n" ++
    "  Source Text     : " ++ show sourceText ++ "\n" ++
    "  Source Position : " ++ show sourcePos ++ "\n" ++
    "]"

isCharacterLexeme :: (Pos p) => Lexeme p -> Bool
isCharacterLexeme Character{} = True
isCharacterLexeme _ = False

isControlWordLexeme :: (Pos p) => Lexeme p -> Bool
isControlWordLexeme ControlWord{} = True
isControlWordLexeme _ = False

isControlSymbolLexeme :: (Pos p) => Lexeme p -> Bool
isControlSymbolLexeme ControlSymbol{} = True
isControlSymbolLexeme _ = False

isControlSpaceLexeme :: (Pos p) => Lexeme p -> Bool
isControlSpaceLexeme ControlSpace{} = True
isControlSpaceLexeme _ = False

isParameterLexeme :: (Pos p) => Lexeme p -> Bool
isParameterLexeme Parameter{} = True
isParameterLexeme _ = False

isSkippedLexeme :: (Pos p) => Lexeme p -> Bool
isSkippedLexeme Skipped{} = True
isSkippedLexeme _ = False

isCommentLexeme :: (Pos p) => Lexeme p -> Bool
isCommentLexeme Comment{} = True
isCommentLexeme _ = False

isEscapeCharLexeme :: (Pos p) => Lexeme p -> Bool
isEscapeCharLexeme Character{charCatCode = catCode}
  | catCode == EscapeCharCat = True
isEscapeCharLexeme _ = False

isBeginGroupCharLexeme :: (Pos p) => Lexeme p -> Bool
isBeginGroupCharLexeme Character{charCatCode = catCode}
  | catCode == BeginGroupCat = True
isBeginGroupCharLexeme _ = False

isEndGroupCharLexeme :: (Pos p) => Lexeme p -> Bool
isEndGroupCharLexeme Character{charCatCode = catCode}
  | catCode == EndGroupCat = True
isEndGroupCharLexeme _ = False

isMathShiftCharLexeme :: (Pos p) => Lexeme p -> Bool
isMathShiftCharLexeme Character{charCatCode = catCode}
  | catCode == MathShiftCat = True
isMathShiftCharLexeme _ = False

isAlignTabCharLexeme :: (Pos p) => Lexeme p -> Bool
isAlignTabCharLexeme Character{charCatCode = catCode}
  | catCode == AlignTabCat = True
isAlignTabCharLexeme _ = False

isEndOfLineCharLexeme :: (Pos p) => Lexeme p -> Bool
isEndOfLineCharLexeme Character{charCatCode = catCode}
  | catCode == EndOfLineCat = True
isEndOfLineCharLexeme _ = False

isParamCharLexeme :: (Pos p) => Lexeme p -> Bool
isParamCharLexeme Character{charCatCode = catCode}
  | catCode == ParamCharCat = True
isParamCharLexeme _ = False

isSuperscriptCharLexeme :: (Pos p) => Lexeme p -> Bool
isSuperscriptCharLexeme Character{charCatCode = catCode}
  | catCode == SuperscriptCat = True
isSuperscriptCharLexeme _ = False

isSubscriptCharLexeme :: (Pos p) => Lexeme p -> Bool
isSubscriptCharLexeme Character{charCatCode = catCode}
  | catCode == SubscriptCat = True
isSubscriptCharLexeme _ = False

isIgnoredCharLexeme :: (Pos p) => Lexeme p -> Bool
isIgnoredCharLexeme Character{charCatCode = catCode}
  | catCode == IgnoredCat = True
isIgnoredCharLexeme _ = False

isSpaceCharLexeme :: (Pos p) => Lexeme p -> Bool
isSpaceCharLexeme Character{charCatCode = catCode}
  | catCode == SpaceCat = True
isSpaceCharLexeme _ = False

isLetterCharLexeme :: (Pos p) => Lexeme p -> Bool
isLetterCharLexeme Character{charCatCode = catCode}
  | catCode == LetterCat = True
isLetterCharLexeme _ = False

isOtherCharLexeme :: (Pos p) => Lexeme p -> Bool
isOtherCharLexeme Character{charCatCode = catCode}
  | catCode == OtherCat = True
isOtherCharLexeme _ = False

isActiveCharLexeme :: (Pos p) => Lexeme p -> Bool
isActiveCharLexeme Character{charCatCode = catCode}
  | catCode == ActiveCat = True
isActiveCharLexeme _ = False

isCommentPrefixCharLexeme :: (Pos p) => Lexeme p -> Bool
isCommentPrefixCharLexeme Character{charCatCode = catCode}
  | catCode == CommentPrefixCat = True
isCommentPrefixCharLexeme _ = False

isInvalidCharLexeme :: (Pos p) => Lexeme p -> Bool
isInvalidCharLexeme Character{charCatCode = catCode}
  | catCode == InvalidCat = True
isInvalidCharLexeme _ = False
