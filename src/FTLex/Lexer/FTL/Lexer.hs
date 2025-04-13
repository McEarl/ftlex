-- |
-- Module      : FTLex.Lexer.FTL.Lexer
-- Copyright   : (c) 2024-2025, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Lexing the input text.

module FTLex.Lexer.FTL.Lexer (
  runLexer
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Control.Monad.State.Class (get, put)
import Text.Megaparsec hiding (Pos)
import FTLex.Lexer.FTL.Lexemes
import FTLex.Lexer.FTL.Characters
import FTLex.Position
import FTLex.Error
import FTLex.Message
import FTLex.Lexer.Base (Lines(..))
import FTLex.Lexer.Base qualified as Base
import FTLex.Helpers


-- * Errors

-- | A lexing error.
data (Pos p) => LexingError p =
    InvalidChar Char p
  | UnknownChar Char p
  deriving (Eq, Ord)

-- | Turn an error into a located error 
makeErrMsg :: (Pos p) => LexingError p -> LocatedMsg p
makeErrMsg (InvalidChar char pos) =
  let msg = "Invalid character " <> codePoint char <> "."
  in (msg, pos)
makeErrMsg (UnknownChar char pos) =
  let msg = "Unknown character " <> codePoint char <> ".\n" <>
            "Only characters from the Unicode blocks \"Basic Latin\" and " <>
            "\"Latin-1 Supplement\", and Isabelle symbols are allowed."
  in (msg, pos)


-- * Lexer Type

type FtlLexer resultType p = Base.Lexer (LexingError p) p resultType


-- * Running a Lexer

runLexer :: (Msg p m)
         => p             -- ^ Initial position
         -> Text          -- ^ Input text
         -> m [Lexeme p]
runLexer pos input =
  let lines = Base.splitText input
  in runLexer' pos lines
  where
    runLexer' pos (MiddleLine line lineBreak rest) = do
      (lexemes, newPos) <- Base.runLexer
        ftlLine
        pos
        line
        (handleError makeErrMsg)
      let lineBreakPos = getPosOf lineBreak newPos
          newPos' = getNextPos lineBreak newPos
          lineBreakLexeme = singleton Space{
              sourceText = lineBreak,
              sourcePos = lineBreakPos
            }
      restLexemes <- runLexer' newPos' rest
      return $ lexemes ++ lineBreakLexeme ++ restLexemes
    runLexer' pos (LastLine line) = do
      (lexemes, _) <- Base.runLexer
        ftlLine
        pos
        line
        (handleError makeErrMsg)
      return lexemes


-- * Lexer Combinators

-- | A single line of a ForTheL text in the FTL dialect.
ftlLine :: (Pos p) => FtlLexer ([Lexeme p], p) p
ftlLine = do
  lexemes <- many $ choice [
      comment,
      space,
      word,
      symbol,
      catchUnknownChar
    ]
  eof
  state <- get
  return (lexemes, state)

-- | A line comment: Starts with '#' and ends at the next line break.
comment :: (Pos p) => FtlLexer (Lexeme p) p
comment = do
  pos <- get
  prefix <- satisfy isCommentPrefixChar
  -- Consume as many characters as possible until either an invalid character,
  -- a vertical space or the end of input is reached:
  commentBody <- Text.pack <$> many (satisfy $ disjunction [
      isSpaceChar,
      isAlphanumChar,
      isSymbolChar,
      isCommentPrefixChar
    ])
  let comment = Text.cons prefix commentBody
      newPos = getNextPos comment pos
  -- If an unknown character is reached, chatch it (at the position that has
  -- been reached during the execution of the last line):
  optional $ catchUnknownCharAt newPos
  -- If no invalid character is reached, expect the end of input:
  eof
  let commentPosition = getPosOf comment pos
  put newPos
  return $ Comment{
      commentContent = commentBody,
      sourceText = comment,
      sourcePos = commentPosition
    }


-- | White space: Longest possible string of ASCII space characters.
space :: (Pos p) => FtlLexer (Lexeme p) p
space = do
  pos <- get
  space <- Text.pack <$> some (satisfy isSpaceChar)
  let newPos = getNextPos space pos
      spacePos = getPosOf space pos
  put newPos
  return $ Space{
      sourceText = space,
      sourcePos = spacePos
    }

-- | A lexeme: Longest possible string of alpha-numeric ASCII characters.
word :: (Pos p) => FtlLexer (Lexeme p) p
word = do
  pos <- get
  lexeme <- Text.pack <$> some (satisfy isAlphanumChar)
  let newPos = getNextPos lexeme pos
      lexemePos = getPosOf lexeme pos
  put newPos
  return $ Word{
      wordContent = lexeme,
      sourceText = lexeme,
      sourcePos = lexemePos
    }


-- | A symbol: Any singleton ASCII symbol character.
symbol :: (Pos p) => FtlLexer (Lexeme p) p
symbol = do
  pos <- get
  symbol <- satisfy isSymbolChar
  let symbolText = Text.singleton symbol
      newPos = getNextPos symbolText pos
      symbolPos = getPosOf symbolText pos
  put newPos
  return $ Symbol{
      symbolContent = symbol,
      sourceText = symbolText,
      sourcePos = symbolPos
    }

-- | Catch an unknown character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownCharAt :: (Pos p) => p -> FtlLexer a p
catchUnknownCharAt pos = do
  char <- satisfy isUnknownChar
  let charPos = getPosOf (Text.singleton char) pos
      err = UnknownChar char charPos
  customFailure err

-- | Catch an unknown character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownChar :: (Pos p) => FtlLexer a p
catchUnknownChar = get >>= catchUnknownCharAt
