-- |
-- Creator: Marcel Schütz (2024)
--
-- FTL Lexer

{-# LANGUAGE OverloadedStrings #-}

module Flex.Ftl (
  CatCode(..),
  CatCodeMap,
  defaultCatCodes,
  initState,
  runLexer,
  LexingState(..),
  Lexeme(..)
) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Control.Monad.State.Class (get, put, gets)
import Text.Megaparsec hiding (Pos)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Flex.Position
import Flex.Error
import Flex.Message
import Flex.Base (LineBreakType(..), UnicodeBlock(..))
import Flex.Base qualified as Base
import Flex.Helpers


-- * Category Codes

-- | TeX-like Category codes.
data CatCode =
    SpaceCat          -- ^ Horizontal space
  | AlphaNumCat       -- ^ Alphanumeric charcter
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

-- | Checks whether a character is an alpha-numeric character wrt. a given 
-- category code mapping.
isAlphanumChar :: CatCodeMap -> Char -> Bool
isAlphanumChar catCodeMap c =
  Map.lookup c catCodeMap == Just AlphaNumCat

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
defaultCatCodes :: Set UnicodeBlock -> CatCodeMap
defaultCatCodes blocks =
  let blocksWithBasicLatin = Set.insert BasicLatin blocks
  in Map.fromAscList [(c, defCatCode c) |
      c <- concatMap Base.charsOf (Set.toList blocksWithBasicLatin),
      c `isInSomeBlockOf` blocksWithBasicLatin
    ]
  where
    isInSomeBlockOf c = not . Set.null . Set.filter (Base.isInUnicodeBlock c)

-- | The default category code of a character.
defCatCode :: Char -> CatCode
defCatCode c
  | isDefSpace c = SpaceCat
  | isDefAlphaNum c = AlphaNumCat
  | isDefSymbol c = SymbolCat
defCatCode '\x0023' = CommentPrefixCat  -- '#'
defCatCode _ = InvalidCat

-- | Default space characters.
isDefSpace :: Char -> Bool
isDefSpace c = elem c $
     ['\x0020']  -- Space
  -- Latin-1 Supplement:
  ++ ['\x00A0'] -- Non-breakable space

-- | Default alphanumeric characters.
isDefAlphaNum :: Char -> Bool
isDefAlphaNum c = elem c $
  -- Basic Latin:
     ['\x0041' .. '\x005A'] -- 'A' – 'Z'
  ++ ['\x0061' .. '\x007A'] -- 'a' – 'z'
  ++ ['\x0030' .. '\x0039'] -- '0' – '9'
  -- Latin-1 Supplement:
  ++ ['\x00C0' .. '\x00D6'] -- 'À' – 'Ö'
  ++ ['\x00D8' .. '\x00F6'] -- 'Ø' – 'ö'
  ++ ['\x00F8' .. '\x00FF'] -- 'ø' – 'ÿ'
  -- Latin Extended-A:
  ++ ['\x0100' .. '\x017F'] -- All of Latin Extended-A
  -- Latin Extended-B:
  ++ ['\x0180' .. '\x024F'] -- All of Latin Extended-B
  -- IPA Extensions:
  ++ ['\x0250' .. '\x02AF'] -- All of IPA Extensions

-- | Default symbol characters.
isDefSymbol :: Char -> Bool
isDefSymbol c = elem c $
  -- Basic Latin:
     ['\x0021' .. '\x0022'] -- '!' – '"'
  ++ ['\x0024' .. '\x002F'] -- '$' – '/'
  ++ ['\x003A' .. '\x0040'] -- ':' – '@'
  ++ ['\x005B' .. '\x0060'] -- '[' – '`'
  ++ ['\x007B' .. '\x007E'] -- '{' – '~'
  -- Latin-1 Supplement:
  ++ ['\x00A1' .. '\x00BF'] -- '¡' – '¿'
  ++ ['\x00D7']             -- '×'
  ++ ['\x00F7']             -- '÷'


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


-- * Errors

-- | A lexing error.
data (Pos p) => LexingError p =
    InvalidChar Char p
  | UnknownChar Char p (Set UnicodeBlock)
  deriving (Eq, Ord)

-- | Turn an error into a located error 
makeErrMsg :: (Pos p) => LexingError p -> LocatedMsg p
makeErrMsg (InvalidChar char pos) =
  let msg = "Invalid character " <> codePoint char <> "."
  in (msg, pos)
makeErrMsg (UnknownChar char pos blocks) =
  let msg = "Unknown character " <> codePoint char <> ".\n" <>
            "Only characters from the following Unicode blocks are allowed: " <>
            Base.showCodeBlocks (Set.insert BasicLatin blocks)
  in (msg, pos)


-- * Lexer Type

type FtlLexer resultType p = Base.Lexer (LexingError p) (LexingState p) resultType


-- * Lexer State

-- | The current lexing state.
data (Pos p) => LexingState p = LexingState{
    position :: p,
    -- ^ Current position
    catCodes :: CatCodeMap,
    -- ^ Current category codes
    unicodeBlocks :: Set UnicodeBlock
    -- ^ Unicode blocks whose characters are allowed in the input text
  }

initState :: (Pos p) => p -> Set UnicodeBlock -> LexingState p
initState pos blocks = LexingState{
    position = pos,
    catCodes = defaultCatCodes blocks,
    unicodeBlocks = blocks
  }


-- * Running a Lexer

runLexer :: (Msg p m)
         => p             -- ^ Initial position
         -> Text          -- ^ Input text
         -> LexingState p -- ^ Lexing state
         -> LineBreakType -- ^ Line break type
         -> m [Lexeme p]
runLexer pos text state lineBreakType =
  runLexer' pos (Base.removeBom text) state lineBreakType
  where
    runLexer' pos text state lineBreakType = do
      -- Split the input text at the first linebreak:
      let (line, lineBreak, rest) = Base.splitText lineBreakType text
      -- Lex the first line of the input text:
      (lexemes, newState) <- Base.runLexer
        ftlLine
        state
        line
        (handleError makeErrMsg)
      -- Turn the line break into a space lexeme:
      let newPos = position newState
          lineBreakPos = getPosOf lineBreak newPos
          newPos' = getNextPos lineBreak newPos
          lineBreakLexeme = singleton Space{
              sourceText = lineBreak,
              sourcePos = lineBreakPos
            }
          newState' = newState{position = newPos'}
      -- Repeat the procedure for the remainder of the input text:
      restLexemes <- if Text.null rest
        then pure []
        else runLexer newPos' rest newState' lineBreakType
      return $ lexemes ++ lineBreakLexeme ++ restLexemes


-- * Lexer Combinators

-- | A single line of a ForTheL text in the FTL dialect.
ftlLine :: (Pos p) => FtlLexer ([Lexeme p], LexingState p) p
ftlLine = do
  lexemes <- many $ choice [
      comment,
      space,
      word,
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
      blocks = unicodeBlocks state
  char <- satisfy $ \c -> Map.notMember c cats
  let charPos = getPosOf (Text.singleton char) pos
      err = UnknownChar char charPos blocks
  customFailure err

-- | Catch an unknown character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownChar :: (Pos p) => FtlLexer a p
catchUnknownChar = do
  pos <- gets position
  catchUnknownCharAt pos
