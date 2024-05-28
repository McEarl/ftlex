-- |
-- Creator: Marcel Schütz (2024)
--
-- Default FTL Lexer

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Flex.Default (
  runDefaultFtlLexer,
  defaultCatCodes
) where

import Flex.Lexer
import Text.Megaparsec.Pos (SourcePos(..), mkPos)
import Text.Megaparsec.Pos qualified as Megaparsec
import Data.Text.Lazy (Text)
import Data.Char qualified as Char
import Data.Map qualified as Map
import Data.List qualified as List


-- * Default Command Line Lexer

runDefaultFtlLexer :: (Monad m) => FilePath -> Text -> m [Lexeme DefaultPos]
runDefaultFtlLexer fileName fileContent =
  runFtlLexer
    (defaultInitPos fileName)
    fileContent
    fileName
    defaultCatCodes
    defaultModifier


-- * Default Category Codes

-- | Default category codes.
defaultCatCodes :: CatCodeMap
defaultCatCodes = Map.fromAscList
  [(c, initCatCode c) | c <- ['\NUL' .. '\DEL']]
  where
    initCatCode :: Char -> CatCode
    initCatCode ' ' = SpaceCat
    initCatCode '\n' = LineBreakCat
    initCatCode c
      | Char.isAsciiUpper c = AlphaNumCat
      | Char.isAsciiLower c = AlphaNumCat
      | Char.isDigit c = AlphaNumCat
    initCatCode c
      | '\x21' <= c && c <= '\x22' = SymbolCat -- ! "
      | '\x24' <= c && c <= '\x2f' = SymbolCat -- $ % & ' ( ) * + , - . /
      | '\x3a' <= c && c <= '\x40' = SymbolCat -- : ; < = > ? @
      | '\x5b' <= c && c <= '\x60' = SymbolCat -- [ \ ] ^ _ `
      | '\x7b' <= c && c <= '\x7e' = SymbolCat -- { | } ~
    initCatCode '#' = CommentPrefixCat
    initCatCode _ = InvalidCat


-- * Default Modifiers 

defaultModifier ::(Monad m) => [Lexeme p] -> m [Lexeme p]
defaultModifier = pure


-- * Default Positions

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


-- * Default Message

instance (Monad m) => Msg DefaultPos m where
  errorLexer :: DefaultPos -> String -> m a
  errorLexer pos msg = error $
    "Error in " ++ show (Megaparsec.sourceName (fromDefPos pos)) ++ ", " ++
    "line" ++ show (Megaparsec.sourceLine (fromDefPos pos)) ++ ", " ++
    "column " ++ show (Megaparsec.sourceLine (fromDefPos pos)) ++ ":\n" ++
    msg
