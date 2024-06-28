-- |
-- Module      : Ftlex.Error
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Lexing errors.

{-# LANGUAGE OverloadedStrings #-}

module Ftlex.Error (
  handleError
) where

import Text.Megaparsec.Error
import Data.Text.Lazy (Text)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.NonEmpty as NonEmpty
import Ftlex.Message
import Ftlex.Position


-- | Report a lexing error.
handleError :: (Msg p m) => (error -> LocatedMsg p) -> ParseErrorBundle Text error -> m a
handleError errorHandler errors = do
  let (errorMsg, errorPos) = showError errors errorHandler
  errorLexer errorPos errorMsg

-- | Return an error message and the position of the first error that occured
-- during lexing.
showError :: (Pos p) => ParseErrorBundle Text error -> (error -> LocatedMsg p) -> LocatedMsg p
showError (ParseErrorBundle parseErrors _) errorHandler = case NonEmpty.head parseErrors of
  TrivialError{} -> unknownError
  FancyError _ errs -> properError errorHandler errs

-- | Located error message for an error that is not handled as a custom error
-- of type "Error" during lexing.
unknownError :: (Pos p) => LocatedMsg p
unknownError =
  let msg =
        "Unknown lexing error. " <>
        "This is likely to be a bug in FTLex. " <>
        "Please file an issue if it has not been reported yet."
      pos = noPos
  in (msg, pos)

-- | Turn a set of lexing errors into a located error message.
properError :: (Pos p) => (error -> LocatedMsg p) -> Set (ErrorFancy error) -> LocatedMsg p
properError errorHandler errs =
  case Set.elems errs of
    [] -> unknownError
    err : _ -> fancyError errorHandler err

-- | Turn a lexing error into a located error message.
fancyError :: (Pos p) =>  (error -> LocatedMsg p) -> ErrorFancy error -> LocatedMsg p
fancyError errorHandler err = case err of
  ErrorFail{} -> unknownError
  ErrorIndentation{} -> unknownError
  ErrorCustom err -> errorHandler err
