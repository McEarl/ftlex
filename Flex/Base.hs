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
runLexer :: Lexer errorType stateType resultType
         -- ^ lexer to run
         -> stateType
         -- ^ initial state
         -> Text
         -- ^ input text
         -> String
         -- ^ label of input text (e.g. its file name)
         -> (resultType -> a)
         -- ^ function to be applied to resulting lexemes when lexing succeeds
         -> (ParseErrorBundle Text errorType -> a)
         -- ^ function to be applied to resulting error when lexing fails
         -> a
runLexer lexer initState text label f e =
  case evalState (runParserT lexer label text) initState of
    Left err -> e err
    Right lexemes -> f lexemes
