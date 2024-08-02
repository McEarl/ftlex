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
  Lines(..),
  splitText,
  UnicodeBlock(..),
  isInUnicodeBlock,
  showCodeBlocks,
  charsOf
) where

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


-- * Splitting the Input Text

data Lines =
    MiddleLine Text Text Lines
    -- ^ A line that is *not* the last line of a text, its subsequent line break
    -- and its following lines.
  | LastLine Text
    -- ^ The last line of a text

-- | Split a text into lines
splitText :: Text -> Lines
splitText text =
  let (beforeBreak, rest) = Text.break (`elem` ['\r', '\n']) text
  in case Text.uncons rest of
    Just ('\r', rest') -> case Text.uncons rest' of
      Just ('\n', rest'') -> MiddleLine beforeBreak "\r\n" $ splitText rest''
      _ -> MiddleLine beforeBreak "\r" $ splitText rest'
    Just ('\n', rest') -> MiddleLine beforeBreak "\n" $ splitText rest'
    _ -> LastLine beforeBreak


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
