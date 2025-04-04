-- |
-- Module      : FTLex.Ftl
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- FTL Lexer

module FTLex.Ftl (
  CatCode(..),
  CatCodeMap,
  defaultCatCodes,
  initState,
  runLexer,
  LexingState(..),
  Lexeme(..),
  isSymbolLexeme,
  isIsabelleSymbolLexeme,
  isWordLexeme,
  isSpaceLexeme,
  isCommentLexeme
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Control.Monad.State.Class (get, put, gets)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char (char)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import FTLex.Position
import FTLex.Error
import FTLex.Message
import FTLex.Base (Lines(..), allowedChars)
import FTLex.Base qualified as Base
import FTLex.Helpers


-- * Category Codes

-- | TeX-like Category codes.
data CatCode =
    SpaceCat          -- ^ Horizontal space
  | LetterCat         -- ^ Letter
  | DigitCat          -- ^ Digit
  | SymbolCat         -- ^ Symbol
  | CommentPrefixCat  -- ^ Comment prefix
  | InvalidCat        -- ^ Invalid character
  deriving Eq

-- | A map that assigns a character a category code. Any character not contained
-- in that map is supposed to throw an "unknown character" error during lexing.
type CatCodeMap = Map Char CatCode

-- | Checks whether a character is a space wrt. a given category code mapping.
isSpace :: CatCodeMap -> Char -> Bool
isSpace catCodeMap c =
  Map.lookup c catCodeMap == Just SpaceCat

-- | Checks whether a character is a letter wrt. a given  category code mapping.
isLetterChar :: CatCodeMap -> Char -> Bool
isLetterChar catCodeMap c =
  Map.lookup c catCodeMap == Just LetterCat

-- | Checks whether a character is a digit wrt. a given category code mapping.
isDigitChar :: CatCodeMap -> Char -> Bool
isDigitChar catCodeMap c =
  Map.lookup c catCodeMap == Just DigitCat

-- | Checks whether a character is an alpha-numeric character wrt. a given 
-- category code mapping.
isAlphanumChar :: CatCodeMap -> Char -> Bool
isAlphanumChar catCodeMap c =
  Map.lookup c catCodeMap `elem` [Just LetterCat, Just DigitCat]

-- | Checks whether a character is a symbol wrt. a given category code mapping.
isSymbol :: CatCodeMap -> Char -> Bool
isSymbol catCodeMap c =
  Map.lookup c catCodeMap == Just SymbolCat

-- | Checks whether a character is a comment prefix character wrt. a given
-- category code mapping.
isCommentChar :: CatCodeMap -> Char -> Bool
isCommentChar catCodeMap c =
  Map.lookup c catCodeMap == Just CommentPrefixCat

-- | Checks whether a character is invalid wrt. a given category code mapping.
isInvalidChar :: CatCodeMap -> Char -> Bool
isInvalidChar catCodeMap c =
     Map.lookup c catCodeMap == Just InvalidCat


-- * Default Category Codes

-- | Default category code mapping for FTL documents.
defaultCatCodes :: CatCodeMap
defaultCatCodes = Map.fromAscList [(c, defCatCode c) | c <- allowedChars]

-- | The default category code of a character.
defCatCode :: Char -> CatCode
-- Spaces:
defCatCode ' ' = SpaceCat
defCatCode '\r' = SpaceCat
defCatCode '\n' = SpaceCat
defCatCode '\t' = SpaceCat
defCatCode '\x00A0' = SpaceCat  -- Non-breakable space
-- Comment prefixes:
defCatCode '#' = CommentPrefixCat
-- Digits:
defCatCode c | c `elem` ['0' .. '9'] = DigitCat
-- Basic Latin letters:
defCatCode c | c `elem` ['A' .. 'Z'] = LetterCat
defCatCode c | c `elem` ['a' .. 'z'] = LetterCat
-- Latin-1 Supplement letters:
defCatCode c | c `elem` ['\x00C0' .. '\x00D6'] = LetterCat
defCatCode c | c `elem` ['\x00D8' .. '\x00F6'] = LetterCat
defCatCode c | c `elem` ['\x00F8' .. '\x00FF'] = LetterCat
-- Basic Latin symbols:
defCatCode c | c `elem` ['\x0021' .. '\x0022'] = SymbolCat
defCatCode c | c `elem` ['\x0024' .. '\x002F'] = SymbolCat
defCatCode c | c `elem` ['\x003A' .. '\x0040'] = SymbolCat
defCatCode c | c `elem` ['\x005B' .. '\x0060'] = SymbolCat
defCatCode c | c `elem` ['\x007B' .. '\x007E'] = SymbolCat
-- Latin-1 Supplement symbols:
defCatCode c | c `elem` ['\x00A1' .. '\x00BF'] = SymbolCat
defCatCode '\x00D7' = SymbolCat
defCatCode '\x00F7' = SymbolCat
-- Invalid characters:
defCatCode _ = InvalidCat


-- * Lexemes

data (Pos p) => Lexeme p =
    Symbol{
      symbolContent :: Char,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ A symbol
  | IsabelleSymbol{
      isabelleSymbolContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ An "Isabelle symbol"
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
  deriving (Eq, Ord, Show)

isSymbolLexeme :: (Pos p) => Lexeme p -> Bool
isSymbolLexeme Symbol{} = True
isSymbolLexeme _ = False

isIsabelleSymbolLexeme :: (Pos p) => Lexeme p -> Bool
isIsabelleSymbolLexeme IsabelleSymbol{} = True
isIsabelleSymbolLexeme _ = False

isWordLexeme :: (Pos p) => Lexeme p -> Bool
isWordLexeme Word{} = True
isWordLexeme _ = False

isSpaceLexeme :: (Pos p) => Lexeme p -> Bool
isSpaceLexeme Space{} = True
isSpaceLexeme _ = False

isCommentLexeme :: (Pos p) => Lexeme p -> Bool
isCommentLexeme Comment{} = True
isCommentLexeme _ = False


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
            "Only characters from the Unicode blocks \"Basic Latin\" and \"Latin-1 Supplement\" are allowed."
  in (msg, pos)


-- * Lexer Type

type FtlLexer resultType p = Base.Lexer (LexingError p) (LexingState p) resultType


-- * Lexer State

-- | The current lexing state.
data (Pos p) => LexingState p = LexingState{
    position :: p,
    -- ^ Current position
    catCodes :: CatCodeMap
    -- ^ Current category codes
  }

initState :: (Pos p) => p -> LexingState p
initState pos = LexingState{
    position = pos,
    catCodes = defaultCatCodes
  }


-- * Running a Lexer

runLexer :: (Msg p m)
         => p             -- ^ Initial position
         -> Text          -- ^ Input text
         -> LexingState p -- ^ Lexing state
         -> m [Lexeme p]
runLexer pos input state =
  let lines = Base.splitText input
  in runLexer' pos lines state
  where
    runLexer' pos (MiddleLine line lineBreak rest) state = do
      (lexemes, newState) <- Base.runLexer
        ftlLine
        state
        line
        (handleError makeErrMsg)
      let newPos = position newState
          lineBreakPos = getPosOf lineBreak newPos
          newPos' = getNextPos lineBreak newPos
          lineBreakLexeme = singleton Space{
              sourceText = lineBreak,
              sourcePos = lineBreakPos
            }
          newState' = newState{position = newPos'}
      restLexemes <- runLexer' newPos' rest newState'
      return $ lexemes ++ lineBreakLexeme ++ restLexemes
    runLexer' pos (LastLine line) state = do
      (lexemes, _) <- Base.runLexer
        ftlLine
        state
        line
        (handleError makeErrMsg)
      return lexemes


-- * Lexer Combinators

-- | A single line of a ForTheL text in the FTL dialect.
ftlLine :: (Pos p) => FtlLexer ([Lexeme p], LexingState p) p
ftlLine = do
  lexemes <- many $ choice [
      comment,
      space,
      word,
      try isabelleSymbol,
      symbol,
      catchInvalidChar,
      catchUnknownChar
    ]
  eof
  state <- get
  return (lexemes, state)

-- | A line comment: Starts with '#' and ends at the next line break.
comment :: (Pos p) => FtlLexer (Lexeme p) p
comment = do
  state <- get
  let pos = position state
      cats = catCodes state
  prefix <- satisfy $ isCommentChar cats
  -- Consume as many characters as possible until either an invalid character,
  -- a vertical space or the end of input is reached:
  commentBody <- Text.pack <$> many (satisfy $ disjunction [
      isSpace cats,
      isAlphanumChar cats,
      isSymbol cats,
      isCommentChar cats
    ])
  let comment = Text.cons prefix commentBody
      newPos = getNextPos comment pos
  -- If an invalid character is reached, chatch it (at the position that has
  -- been reached during the execution of the last line):
  optional $ catchInvalidCharAt newPos
  -- If no invalid character is reached, expect the end of input:
  eof
  let commentPosition = getPosOf comment pos
  put state{
      position = newPos
    }
  return $ Comment{
      commentContent = commentBody,
      sourceText = comment,
      sourcePos = commentPosition
    }


-- | White space: Longest possible string of ASCII space characters.
space :: (Pos p) => FtlLexer (Lexeme p) p
space = do
  state <- get
  let pos = position state
      cats = catCodes state
  space <- Text.pack <$> some (satisfy $ isSpace cats)
  let newPos = getNextPos space pos
      spacePos = getPosOf space pos
  put state{
      position = newPos
    }
  return $ Space{
      sourceText = space,
      sourcePos = spacePos
    }

-- | A lexeme: Longest possible string of alpha-numeric ASCII characters.
word :: (Pos p) => FtlLexer (Lexeme p) p
word = do
  state <- get
  let pos = position state
      cats = catCodes state
  lexeme <- Text.pack <$> some (satisfy $ isAlphanumChar cats)
  let newPos = getNextPos lexeme pos
      lexemePos = getPosOf lexeme pos
  put state{
      position = newPos
    }
  return $ Word{
      wordContent = lexeme,
      sourceText = lexeme,
      sourcePos = lexemePos
    }

-- | An "Isabelle symbol": A string of the form @\<identifier>@, where
-- @identifier@ is an element of @isabelleSymbols@.
isabelleSymbol :: (Pos p) => FtlLexer (Lexeme p) p
isabelleSymbol = do
  state <- get
  let pos = position state
      cats = catCodes state
  char '\\'
  char '<'
  identifier <- Text.pack <$> some (satisfy (\c -> c == '^' || isLetterChar cats c))
  char '>'
  let sourceText = "\\<" <> identifier <> ">"
      newPos = getNextPos sourceText pos
      isabelleSymbolPos = getPosOf sourceText pos
  put state{
      position = newPos
    }
  return $ IsabelleSymbol{
      isabelleSymbolContent = identifier,
      sourceText = sourceText,
      sourcePos = isabelleSymbolPos
    }


-- | A symbol: Any singleton ASCII symbol character.
symbol :: (Pos p) => FtlLexer (Lexeme p) p
symbol = do
  state <- get
  let pos = position state
      cats = catCodes state
  symbol <- satisfy $ isSymbol cats
  let symbolText = Text.singleton symbol
      newPos = getNextPos symbolText pos
      symbolPos = getPosOf symbolText pos
  put state{
      position = newPos
    }
  return $ Symbol{
      symbolContent = symbol,
      sourceText = symbolText,
      sourcePos = symbolPos
    }

-- | Catch an invalid character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidCharAt :: (Pos p) => p -> FtlLexer a p
catchInvalidCharAt pos = do
  state <- get
  let cats = catCodes state
  char <- satisfy $ isInvalidChar cats
  let charPos = getPosOf (Text.singleton char) pos
      err = InvalidChar char charPos
  customFailure err

-- | Catch an invalid character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidChar :: (Pos p) => FtlLexer a p
catchInvalidChar = do
  pos <- gets position
  catchInvalidCharAt pos

-- | Catch an unknown character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownCharAt :: (Pos p) => p -> FtlLexer a p
catchUnknownCharAt pos = do
  state <- get
  let cats = catCodes state
  char <- satisfy $ \c -> Map.notMember c cats
  let charPos = getPosOf (Text.singleton char) pos
      err = UnknownChar char charPos
  customFailure err

-- | Catch an unknown character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownChar :: (Pos p) => FtlLexer a p
catchUnknownChar = do
  pos <- gets position
  catchUnknownCharAt pos
