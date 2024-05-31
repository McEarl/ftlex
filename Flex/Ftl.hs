-- |
-- Creator: Marcel Schütz (2024)
--
-- FTL Lexer

{-# LANGUAGE OverloadedStrings #-}

module Flex.Ftl (
  CatCode(..),
  CatCodeMap,
  defaultCatCodes,
  runLexer,
  LexingState(..),
  LineBreakType(..),
  Lexeme(..)
) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Control.Monad.State.Class (get, put, gets)
import Text.Megaparsec hiding (Pos)
import Data.Char qualified as Char
import Numeric (showHex)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Flex.Position
import Flex.Error
import Flex.Message
import Flex.Base qualified as Base
import Flex.Helpers


-- * Category Codes

data CatCode =
    SpaceCat          -- ^ Horizontal space
  | LineBreakCat      -- ^ Line break
  | AlphaNumCat       -- ^ Alphanumeric charcter
  | SymbolCat         -- ^ Symbol
  | CommentPrefixCat  -- ^ Comment prefix
  | InvalidCat        -- ^ Invalid character
  deriving Eq

type CatCodeMap = Map.Map Char CatCode

-- | Checks whether a character is a space wrt. a given category code mapping
-- (default: @ @).
isSpace :: CatCodeMap -> Char -> Bool
isSpace catCodeMap c =
  Map.lookup c catCodeMap == Just SpaceCat

-- | Checks whether a character is a line break character wrt. a given category 
-- code mapping (default: @\\n@).
isLineBreak :: CatCodeMap -> Char -> Bool
isLineBreak catCodeMap c =
  Map.lookup c catCodeMap == Just LineBreakCat

-- | Checks whether a character is an alpha-numeric character wrt. a given 
-- category code mapping (default: any ASCII letter or ASCII digit).
isAlphanumChar :: CatCodeMap -> Char -> Bool
isAlphanumChar catCodeMap c =
  Map.lookup c catCodeMap == Just AlphaNumCat

-- | Checks whether a character is a symbol wrt. a given category code mapping
-- (default: any ASCII character that is neither a control character, nor a
-- letter, nor a digit, nor a space, nor @#@).
isSymbol :: CatCodeMap -> Char -> Bool
isSymbol catCodeMap c =
  Map.lookup c catCodeMap == Just SymbolCat

-- | Checks whether a character is a comment prefix character wrt. a given
-- category code mapping (default: @%@).
isCommentChar :: CatCodeMap -> Char -> Bool
isCommentChar catCodeMap c =
  Map.lookup c catCodeMap == Just CommentPrefixCat

-- | Checks whether a character is invalid wrt. a given category code mapping
-- (default: any non-ASCII character and any ASCII control character except
-- @\\CR@).
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
    initCatCode '\r' = LineBreakCat
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
    Symbol Char p
    -- ^ A symbol
  | Word String p
    -- ^ A sequence of alphanumeric characters
  | Space p
    -- ^ A sequence of (horizontal and vertical) white space characters
  | Comment String p
    -- ^ A comment
  | EOF p
    -- ^ End of file


-- * Errors

-- | A lexing errors.
data (Pos p) => LexingError p =
    InvalidChar !Char p
  deriving (Eq, Ord)

-- | Turn an error into a located error 
makeErrMsg :: (Pos p) => LexingError p -> LocatedMsg p
makeErrMsg (InvalidChar char pos) =
  let msg = "Invalid character U+" ++ codePoint char ++ "."
  in (msg, pos)
  where
    codePoint c = let hex = showHex (Char.ord c) "" in
      replicate (max (4 - length hex) 0) '0' ++ hex
      -- justifyRight 4 '0' $ showHex (Char.ord c) ""


-- * Splitting the Input Text

-- | Supported types of line breaks.
data LineBreakType =
    CR    -- ^ Carriage return (@\\r@)
  | LF    -- ^ Line feed (@\\n@)
  | CRLF  -- ^ Carriage return + line feed (@\\r\\n@)

-- | @splitText catCodeMap lineBreakType text@ splits a text @text@
-- in
-- 1. the content of its first line, without trailing spaces and the line break
--    character(s) (where the former are determined by @catCodeMap@ and the
--    latter by @lineBreakType@),
-- 2. all trailing spaces and the line break character(s), and
-- 3. the rest of the text.
splitText :: CatCodeMap -> LineBreakType -> Text -> (Text, Text, Text)
splitText catCodeMap lineBreakType text =
  let lineBreak = case lineBreakType of
        CR -> "\r"
        LF -> "\n"
        CRLF -> "\r\n"
      (line, rest) = Text.breakOn lineBreak text
      rest' = Text.drop (Text.length lineBreak) rest
  in (line, lineBreak, rest')


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


-- * Running a Lexer

runLexer :: (Msg p m)
         => p             -- ^ Initial position
         -> Text          -- ^ Input text
         -> String        -- ^ Label (e.g. file name)
         -> LexingState p -- ^ Lexing state
         -> LineBreakType -- ^ Line break type
         -> m [Lexeme p]
runLexer pos text label state lineBreakType = do
  -- Split the input text at the first linebreak:
  let (line, lineBreak, rest) = splitText
        (catCodes state)
        lineBreakType
        text
  -- Lex the first line of the input text:
  (lexemes, newState) <- Base.runLexer
    ftlLine
    state
    line
    ""
    (handleError makeErrMsg)
  -- Repeat the procedure for the remainder of the input text and append an EOF
  -- lexeme:
  let newPos = (position newState)
      lineBreakPos = getStringPos (Text.unpack lineBreak) newPos
      newPos' = explodeString (Text.unpack lineBreak) newPos
      lineBreakLexeme = Space lineBreakPos
      newState' = newState{position = newPos'}
  restLexemes <- if Text.null rest
    then pure [EOF newPos']
    else runLexer newPos' rest "" newState' lineBreakType
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
  commentBody <- many $ satisfy $ disjunction [
      isSpace cats,
      isAlphanumChar cats,
      isSymbol cats,
      isCommentChar cats
    ]
  -- If an invalid character is reached, chatch it (at the position that has
  -- been reached during the execution of the last line):
  optional $ catchInvalidCharAt (explodeString (prefix : commentBody) pos)
  -- If no invalid character is reached, expect the end of input:
  eof
  let comment = prefix : commentBody
      newPos = explodeString comment pos
      commentPosition = getStringPos comment pos
  put state{
      position = newPos
    }
  return $ Comment commentBody commentPosition


-- | White space: Longest possible string of ASCII space characters.
space :: (Pos p) => FtlLexer (Lexeme p) p
space = do
  state <- get
  let pos = position state
      cats = catCodes state
  space <- some (satisfy $ disjunction [isSpace cats, isLineBreak cats])
  let newPos = explodeString space pos
      spacePos = getStringPos space pos
  put state{
      position = newPos
    }
  return $ Space spacePos

-- | A lexeme: Longest possible string of alpha-numeric ASCII characters.
word :: (Pos p) => FtlLexer (Lexeme p) p
word = do
  state <- get
  let pos = position state
      cats = catCodes state
  lexeme <- some (satisfy $ isAlphanumChar cats)
  let newPos = explodeString lexeme pos
      lexemePos = getStringPos lexeme pos
  put state{
      position = newPos
    }
  return $ Word lexeme lexemePos

-- | A symbol: Any singleton ASCII symbol character.
symbol :: (Pos p) => FtlLexer (Lexeme p) p
symbol = do
  state <- get
  let pos = position state
      cats = catCodes state
  symbol <- satisfy $ isSymbol cats
  let newPos = explodeString [symbol] pos
      symbolPos = getStringPos [symbol] pos
  put state{
      position = newPos
    }
  return $ Symbol symbol symbolPos

-- | Catch an invalid character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidCharAt :: (Pos p) => p -> FtlLexer a p
catchInvalidCharAt pos = do
  state <- get
  let cats = catCodes state
  char <- satisfy $ isInvalidChar cats
  let charPos = getStringPos [char] pos
      err = InvalidChar char charPos
  customFailure err

-- | Catch an invalid character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidChar :: (Pos p) => FtlLexer a p
catchInvalidChar = do
  pos <- gets position
  catchInvalidCharAt pos
