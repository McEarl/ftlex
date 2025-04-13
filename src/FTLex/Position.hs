-- |
-- Module      : FTLex.Position
-- Copyright   : (c) 2024-2025, Marcel Schütz
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

class (Show p, Ord p) => Pos p where
  noPos :: p
  -- ^ No position
  getNextPos :: Text -> p -> p
  -- ^ Take a string together with its starting position and return the ending
  -- position of that string
  getPosOf :: Text -> p -> p
  -- ^ Take a string together with its starting position and return the position
  -- of the whole string

-- | A simple implementation of the @Pos@ type class.
data SimplePosition = SimplePosition {
    line :: Int,
    column :: Int
  }
  deriving (Eq, Ord)

instance Show SimplePosition where
  show :: SimplePosition -> String
  show (SimplePosition line col) =
    "(line " ++ show line ++ ", " ++ "column " ++ show col ++ ")"

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
