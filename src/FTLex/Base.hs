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
  allowedChars
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Control.Monad.Trans.State.Strict (evalState, State)
import Text.Megaparsec hiding (State, Pos, label)


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

-- | Characters that are allowed to occur in the input text.
allowedChars :: [Char]
allowedChars = ['\x0000' .. '\x00FF']
