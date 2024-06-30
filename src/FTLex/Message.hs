-- |
-- Module      : FTLex.Message
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Message type class

module FTLex.Message (LocatedMsg, Msg(..)) where

import Data.Text (Text)
import FTLex.Position


type LocatedMsg p = (Text, p)

class (Pos p, Monad m) => Msg p m where
  errorLexer :: p -> Text -> m a
  -- ^ Handler for lexing errors
