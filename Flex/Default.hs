-- |
-- Creator: Marcel Schütz (2024)
--
-- Default FTL Lexer

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Flex.Default (
  runLexer
) where

import Flex.Ftl qualified as Ftl
import Text.Megaparsec.Pos (SourcePos(..), mkPos)
import Text.Megaparsec.Pos qualified as Megaparsec
import Data.Text.Lazy (Text)
import Data.Char qualified as Char
import Data.Map qualified as Map
import Data.List qualified as List


-- * Default Command Line Lexer

runLexer :: (Monad m) => FilePath -> Text -> m [Lexeme DefaultPos]
runLexer fileName fileContent =
  Ftl.runLexer
    (defaultInitPos fileName)
    fileContent
    fileName
    Ftl.defaultCatCodes
    pure


-- * Default Position Instance

newtype DefaultPos = DefaultPos SourcePos deriving (Eq, Ord)

fromDefPos :: DefaultPos -> SourcePos
fromDefPos (DefaultPos pos) = pos

instance Pos DefaultPos where
  noPos :: DefaultPos
  noPos = DefaultPos $ SourcePos "" (mkPos 0) (mkPos 0)

  explodeString :: String -> DefaultPos -> DefaultPos
  explodeString str pos =
    let file = sourceName (fromDefPos pos)
        lines = length . List.lines $ str in
    if lines < 1
      then pos
      else let cols = length . last . List.lines $ str in
        DefaultPos $ SourcePos file (mkPos lines) (mkPos cols)

  getStringPos :: String -> DefaultPos -> DefaultPos
  getStringPos _ pos = pos

defaultInitPos :: FilePath -> DefaultPos
defaultInitPos fileName = DefaultPos $ SourcePos fileName (mkPos 1) (mkPos 1)


-- * Default Message Instance

instance (Monad m) => Msg DefaultPos m where
  errorLexer :: DefaultPos -> String -> m a
  errorLexer pos msg = error $
    "Error in " ++ show (Megaparsec.sourceName (fromDefPos pos)) ++ ", " ++
    "line" ++ show (Megaparsec.sourceLine (fromDefPos pos)) ++ ", " ++
    "column " ++ show (Megaparsec.sourceLine (fromDefPos pos)) ++ ":\n" ++
    msg
