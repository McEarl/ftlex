-- |
-- Creator: Marcel Schütz (2024)
--
-- Message type class

{-# LANGUAGE MultiParamTypeClasses #-}

module Flex.Message (LocatedMsg, Msg(..)) where

import Data.Text.Lazy (Text)
import Flex.Position


type LocatedMsg p = (Text, p)

class (Pos p, Monad m) => Msg p m where
  errorLexer :: p -> Text -> m a
  -- ^ Handler for lexing errors
