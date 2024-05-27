-- |
-- Creator: Marcel Schütz (2024)
--
-- Message type class

{-# LANGUAGE MultiParamTypeClasses #-}

module Flex.Message (LocatedMsg, Msg(..)) where

import Flex.Position


type LocatedMsg p = (String, p)

class (Pos p, Monad m) => Msg p m where
  errorLexer :: p -> String -> m a
  -- ^ Handler for lexing errors
