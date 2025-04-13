-- |
-- Module      : FTLex.Lexer.FTL.Lexemes
-- Copyright   : (c) 2024-2025, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Lexemes.

module FTLex.Lexer.FTL.Lexemes (
  Lexeme(..),
  isSymbolLexeme,
  isWordLexeme,
  isSpaceLexeme,
  isCommentLexeme
) where

import Data.Text (Text)
import FTLex.Position


-- * Lexemes

data (Pos p) => Lexeme p =
    Symbol{
      symbolContent :: Char,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ A symbol
  | Word{
      wordContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ A sequence of alphanumeric characters
  | Space{
      sourceText :: Text,
      sourcePos :: p
    } -- ^ A sequence of (horizontal and vertical) white space characters
  | Comment{
      commentContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ A comment
  deriving (Eq, Ord)

isSymbolLexeme :: (Pos p) => Lexeme p -> Bool
isSymbolLexeme Symbol{} = True
isSymbolLexeme _ = False

isWordLexeme :: (Pos p) => Lexeme p -> Bool
isWordLexeme Word{} = True
isWordLexeme _ = False

isSpaceLexeme :: (Pos p) => Lexeme p -> Bool
isSpaceLexeme Space{} = True
isSpaceLexeme _ = False

isCommentLexeme :: (Pos p) => Lexeme p -> Bool
isCommentLexeme Comment{} = True
isCommentLexeme _ = False


instance (Pos p) => Show (Lexeme p) where
  show :: Pos p => Lexeme p -> String
  show (Symbol content sourceText sourcePos) =
    "Symbol {\n" ++
    "    Content         : " ++ show content ++ "\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (Word content sourceText sourcePos) =
    "Word {\n" ++
    "    Content         : " ++ show content ++ "\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (Space sourceText sourcePos) =
    "Space {\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
  show (Comment content sourceText sourcePos) =
    "Comment {\n" ++
    "    Content         : " ++ show content ++ "\n" ++
    "    Source Text     : " ++ show sourceText ++ "\n" ++
    "    Source Position : " ++ show sourcePos ++ "\n" ++
    "}"
