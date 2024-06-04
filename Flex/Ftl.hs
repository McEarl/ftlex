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
import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Flex.Position
import Flex.Error
import Flex.Message
import Flex.Base qualified as Base
import Flex.Helpers
import Flex.Split


-- * Category Codes

data CatCode =
    SpaceCat          -- ^ Horizontal space
  | AlphaNumCat       -- ^ Alphanumeric charcter
  | SymbolCat         -- ^ Symbol
  | CommentPrefixCat  -- ^ Comment prefix
  | InvalidCat        -- ^ Invalid character
  deriving Eq

type CatCodeMap = Map.Map Char CatCode

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
  || isNothing (Map.lookup c catCodeMap)

-- | Default category code mapping for FTL documents.
defaultCatCodes :: CatCodeMap
defaultCatCodes = Map.fromAscList
  [(c, initCatCode c) | c <- ['\NUL' .. '\DEL']]
  where
    initCatCode :: Char -> CatCode
    initCatCode ' ' = SpaceCat
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
  deriving (Eq, Ord)

-- | Turn an error into a located error 
makeErrMsg :: (Pos p) => LexingError p -> LocatedMsg p
makeErrMsg (InvalidChar char pos) =
  let msg = "Invalid character " <> codePoint char <> "."
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
         -> Text.Text     -- ^ Input text
         -> LexingState p -- ^ Lexing state
         -> LineBreakType -- ^ Line break type
         -> m [Lexeme p]
runLexer pos text state lineBreakType = do
  -- Split the input text at the first linebreak:
  let (line, lineBreak, rest) = splitText lineBreakType text
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
      lineBreakLexeme = Space{
          sourceText = lineBreak,
          sourcePos = lineBreakPos
        }
      newState' = newState{position = newPos'}
  -- Repeat the procedure for the remainder of the input text:
  restLexemes <- if Text.null rest
    then pure []
    else runLexer newPos' rest newState' lineBreakType
  return $ lexemes ++ [lineBreakLexeme] ++ restLexemes


-- * Lexer Combinators

-- | A ForTheL text in the FTL dialect: Arbitrary many tokens, interspersed with
-- optional white space, until the end of the input text is reached.
ftlLine :: (Pos p) => FtlLexer ([Lexeme p], LexingState p) p
ftlLine = do
  lexemes <- many $ choice [
      comment,
      space,
      word,
      symbol,
      catchInvalidChar
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
