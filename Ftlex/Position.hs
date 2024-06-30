-- |
-- Module      : Ftlex.Position
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Position type class

module Ftlex.Position (Pos(..)) where

import Data.Text (Text)

class (Ord p) => Pos p where
  noPos :: p
  -- ^ No position
  getNextPos :: Text -> p -> p
  -- ^ Take a string together with its starting position and return the ending
  -- position of that string
  getPosOf :: Text -> p -> p
  -- ^ Take a string together with its starting position and return the position
  -- of the whole string
  showPos :: p -> Text
  -- ^ Show a position
