-- |
-- Module      : FTLex.Base
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Abstract lexer type.

module FTLex.Base (
  Lexer,
  runLexer,
  Encoding(..),
  decode,
  LineBreakType(..),
  splitText,
  removeBom,
  UnicodeBlock(..),
  isInUnicodeBlock,
  showCodeBlocks,
  charsOf
) where

import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as Encoding
import Data.Text (Text)
import Data.Text qualified as Text
import Control.Monad.Trans.State.Strict (evalState, State)
import Text.Megaparsec hiding (State, Pos, label)
import Data.Set (Set)
import Data.Set qualified as Set


type Lexer errorType stateType resultType = ParsecT errorType Text (State stateType) resultType

-- | Run a lexer and pass the result (either a list of lexemes or an error)
-- to a given function or error handler.
runLexer :: (Monad m)
         => Lexer errorType stateType resultType
         -- ^ Lexer to run
         -> stateType
         -- ^ Initial lexing state
         -> Text
         -- ^ Input text
         -> (ParseErrorBundle Text errorType -> m resultType)
         -- ^ Error handler
         -> m resultType
runLexer lexer initState text e =
  case evalState (runParserT lexer "" text) initState of
    Left err -> e err
    Right lexemes -> pure lexemes


-- * Text Encodings

data Encoding =
    UTF8
  | UTF16LE
  | UTF16BE
  | UTF32LE
  | UTF32BE

-- Decode a byte string wrt. a given character encoding.
decode :: Encoding -> ByteString -> Text
decode UTF8 = Encoding.decodeUtf8
decode UTF16LE = removeBom . Encoding.decodeUtf16LE
decode UTF16BE = removeBom . Encoding.decodeUtf16BE
decode UTF32LE = removeBom . Encoding.decodeUtf32LE
decode UTF32BE = removeBom . Encoding.decodeUtf32BE

-- | Remove a leading BOM from a text.
removeBom :: Text -> Text
removeBom text = case Text.uncons text of
  Just ('\xFEFF', rest) -> rest
  _ -> text


-- * Splitting the Input Text

-- | Supported types of line breaks.
data LineBreakType =
    CR    -- ^ Carriage return (@\\r@)
  | LF    -- ^ Line feed (@\\n@)
  | CRLF  -- ^ Carriage return + line feed (@\\r\\n@)

-- | @splitText lineBreakType text@ splits a text @text@ in
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


-- * Unicode Blocks

-- | Unicode blocks.
data UnicodeBlock =
    BasicLatin        -- ^ Basic Latin characters
  | Latin1Supplement  -- ^ Latin-1 Supplement characters
  | LatinExtendedA    -- ^ Latin Extended-A
  | LatinExtendedB    -- ^ Latin Extended-B
  | IPAExtensions     -- ^ IPA Extensions
  deriving (Eq, Ord)

-- | List all elements of a Set of Unicode blocks.
showCodeBlocks :: Set UnicodeBlock -> Text
showCodeBlocks blocks = Text.intercalate ", " (map showBlock $ Set.toList blocks)
  where
    showBlock BasicLatin = "Basic Latin"
    showBlock Latin1Supplement = "Latin-1 Supplement"
    showBlock LatinExtendedA = "Latin Extended-A"
    showBlock LatinExtendedB = "Latin Extended-B"
    showBlock IPAExtensions = "IPA Extensions"

-- | Check whether a character is contained in a Unicode block.
isInUnicodeBlock :: Char -> UnicodeBlock -> Bool
isInUnicodeBlock c block = c `elem` charsOf block

-- | All characters of a Unicode block.
charsOf :: UnicodeBlock -> [Char]
charsOf BasicLatin = ['\x0000' .. '\x007F']
charsOf Latin1Supplement = ['\x0080' .. '\x00FF']
charsOf LatinExtendedA = ['\x0100' .. '\x017F']
charsOf LatinExtendedB = ['\x0180' .. '\x024F']
charsOf IPAExtensions = ['\x0250' .. '\x02AF']
