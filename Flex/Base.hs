-- |
-- Creator: Marcel Schütz (2024)
--
-- Abstract lexer type.

module Flex.Base (Lexer, runLexer) where

import Data.Text.Lazy (Text)
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
         -> String
         -- ^ Label (e.g. file name)
         -> (ParseErrorBundle Text errorType -> m resultType)
         -- ^ Error handler
         -> m resultType
runLexer lexer initState text label e =
  case evalState (runParserT lexer label text) initState of
    Left err -> e err
    Right lexemes -> pure lexemes
