-- |
-- Creator: Marcel Schütz (2024)
--
-- Debugging output.

{-# LANGUAGE OverloadedStrings #-}

module Ftlex.Debug (
  showFtlLexemes,
  showTexLexemes
) where

import Prelude hiding (showChar)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Ftlex.Ftl qualified as FTL
import Ftlex.Tex qualified as TEX
import Ftlex.Position


-- * FTL

-- | Show a list of FTL lexemes.
showFtlLexemes :: (Pos p) => [FTL.Lexeme p] -> Text
showFtlLexemes = Text.intercalate "\n" . map showLexeme
  where 
    showLexeme (FTL.Symbol content sourceText sourcePos) =
      "Symbol [\n" <>
      "  Content         : " <> showChar content <> "\n" <>
      "  Source Text     : " <> (Text.pack . show . Text.unpack) sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (FTL.Word content sourceText sourcePos) =
      "Word [\n" <>
      "  Content         : " <> showText content <> "\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (FTL.Space sourceText sourcePos) =
      "Space [\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (FTL.Comment content sourceText sourcePos) =
      "Comment [\n" <>
      "  Content         : " <> showText content <> "\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"


-- * TEX

-- | Show a list of TEX lexemes.
showTexLexemes :: (Pos p) => [TEX.Lexeme p] -> Text
showTexLexemes = Text.intercalate "\n" . map showLexeme
  where
    showLexeme (TEX.Character content catCode sourceText sourcePos) =
      "Character [\n" <>
      "  Content         : " <> showChar content <> "\n" <>
      "  Category Code   : " <> showCatCode catCode <> "\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (TEX.ControlWord content sourceText sourcePos) =
      "Control Word [\n" <>
      "  Content         : " <> showText content <> "\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (TEX.ControlSymbol content sourceText sourcePos) =
      "Control Symbol [\n" <>
      "  Content         : " <> showChar content <> "\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (TEX.ControlSpace sourceText sourcePos) =
      "Control Space [\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (TEX.Parameter number sourceText sourcePos) =
      "Parameter [\n" <>
      "  Content         : " <> showNumber number <> "\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (TEX.Skipped sourceText sourcePos) =
      "Skipped Characters [\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"
    showLexeme (TEX.Comment content sourceText sourcePos) =
      "Comment [\n" <>
      "  Content         : " <> showText content <> "\n" <>
      "  Source Text     : " <> showText sourceText <> "\n" <>
      "  Source Position : " <> showPos sourcePos <> "\n" <>
      "]"

showCatCode :: TEX.CatCode -> Text
showCatCode TEX.EscapeCharCat = "Escape character (0)"
showCatCode TEX.BeginGroupCat = "Begin group character (1)"
showCatCode TEX.EndGroupCat = "End group character (2)"
showCatCode TEX.MathShiftCat = "Math shift character (3)"
showCatCode TEX.AlignTabCat = "Alignment tab (4)"
showCatCode TEX.EndOfLineCat = "Line break (5)"
showCatCode TEX.ParamCharCat = "Parameter character (6)"
showCatCode TEX.SuperscriptCat = "Superscript character (7)"
showCatCode TEX.SubscriptCat = "Subscript character (8)"
showCatCode TEX.IgnoredCat = "Ignored character (9)"
showCatCode TEX.SpaceCat = "Horizontal space (10)"
showCatCode TEX.LetterCat = "Letter (11)"
showCatCode TEX.OtherCat = "Other character (12)"
showCatCode TEX.ActiveCat = "Active character (13)"
showCatCode TEX.CommentPrefixCat = "Comment Prefix (14)"
showCatCode TEX.InvalidCat = "Invalid character (15)"
showCatCode TEX.UnknownCat = "Unknown character"

showText :: Text -> Text
showText = Text.pack . show . Text.unpack

showChar :: Char -> Text
showChar c = Text.pack . show $ [c]

showNumber :: Int -> Text
showNumber = Text.pack . show
