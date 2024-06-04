-- |
-- Creator: Marcel Schütz (2024)
--
-- Splitter.

{-# LANGUAGE OverloadedStrings #-}

module Flex.Split (
  LineBreakType,
  splitText
) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text


-- * Splitting the Input Text

-- | Supported types of line breaks.
data LineBreakType =
    CR    -- ^ Carriage return (@\\r@)
  | LF    -- ^ Line feed (@\\n@)
  | CRLF  -- ^ Carriage return + line feed (@\\r\\n@)

-- | @splitText catCodeMap lineBreakType text@ splits a text @text@ in
-- 1. the content of its first line, without the line break
--    character(s) (which are determined  by @lineBreakType@),
-- 2. all trailing spaces and the line break character(s), and
-- 3. the rest of the text.
splitText :: LineBreakType -> Text -> (Text, Text, Text)
splitText lineBreakType text =
  let lineBreak = case lineBreakType of
        CR -> "\r"
        LF -> "\n"
        CRLF -> "\r\n"
      (line, rest) = Text.breakOn lineBreak text
      rest' = Text.drop (Text.length lineBreak) rest
  in (line, lineBreak, rest')
