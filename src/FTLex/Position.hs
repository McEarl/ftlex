-- |
-- Module      : FTLex.Position
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Position type class

module FTLex.Position (
  Pos(..),
  SimplePosition(..)
) where

import Data.Text (Text)
import Data.Text qualified as Text

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

-- | A simple implementation of the @Pos@ type class.
data SimplePosition = SimplePosition {
    line :: Int,
    column :: Int
  }
  deriving (Eq, Ord)

instance Pos SimplePosition where
  noPos :: SimplePosition
  noPos = SimplePosition 0 0

  getNextPos :: Text -> SimplePosition -> SimplePosition
  getNextPos text pos =
    let
      lineBreakNo = Text.count "\n" text
      newLine = line pos + fromIntegral lineBreakNo
      (_, lastCol) = Text.breakOnEnd "\n" text
      newCol = if lineBreakNo > 0
        then (fromIntegral . Text.length) lastCol + 1
        else column pos + (fromIntegral . Text.length) lastCol
    in SimplePosition newLine newCol

  getPosOf :: Text -> SimplePosition -> SimplePosition
  getPosOf _ pos = pos

  showPos :: SimplePosition -> Text
  showPos (SimplePosition line col) =
    "(line " <> showNumber line <> ", " <> "column " <> showNumber col <> ")"
    where
      showNumber = Text.pack . show
