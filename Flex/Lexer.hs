-- |
-- Creator: Marcel Schütz (2024)
--
-- FTL Lexer

module Flex.Lexer (
  runFtlLexer,
  Lexeme,
) where

import Data.Text.Lazy (Text)
import Control.Monad.State.Class (get, put, gets)
import Control.Monad (void)
import Text.Megaparsec hiding (Pos)
import Data.Char (ord)
import Numeric (showHex)

import Flex.Position
import Flex.Error
import Flex.Message
import Flex.Base
import Flex.CatCode hiding (CatCode(..))


-- * Lexemes

data (Pos p) => Lexeme p =
    Symbol Char p
    -- ^ A symbol
  | Word String p
    -- ^ An alpha-numeric string
  | Space p
    -- ^ A sequence of (horizontal and vertical) white space characters
  | Comment p
    -- ^ A comment
  | EOF p
    -- ^ End of file


-- * Errors

-- | A lexing errors.
data (Pos p) => LexingError p =
    InvalidChar !Char p
  deriving (Eq, Ord)

-- | Turn an error into a located error message.
makeErrMsg :: (Pos p) => LexingError p -> LocatedMsg p
makeErrMsg (InvalidChar char pos) =
  let msg = "Invalid character '" ++ [char] ++ "' " ++
            "(U+" ++ codePoint char ++ ")."
  in (msg, pos)
  where
    codePoint c = let hex = showHex (ord c) "" in
      replicate (min (4 - length hex) 0) '0' ++ hex
      -- justifyRight 4 '0' $ showHex (Char.ord c) ""


-- * Lexer Type

type FtlLexer resultType p = Lexer (LexingError p) (LexingState p) resultType


-- * Lexer State

-- | The current lexing state.
data (Pos p) => LexingState p = LexingState{
    position :: p,
    -- ^ Current position
    catCodes :: CatCodeMap
    -- ^ Current category codes
  }

-- | The initial lexing state.
initLexingState :: (Pos p) => p -> CatCodeMap -> LexingState p
initLexingState pos catCodes = LexingState{
    position = pos,
    catCodes = catCodes
  }


-- * Running a Lexer

runFtlLexer :: (Msg p m) => p -> Text -> String -> CatCodeMap -> ([Lexeme p] -> m [Lexeme p]) -> m [Lexeme p]
runFtlLexer initPos inputText inputTextLabel initCatCodes modifier = runLexer
  ftlText
  (initLexingState initPos initCatCodes)
  initPos
  inputText
  inputTextLabel
  modifier
  (handleError makeErrMsg)


-- * Lexer Combinators

-- | A ForTheL text in the FTL dialect: Arbitrary many tokens, interspersed with
-- optional white space, until the end of the input text is reached.
ftlText :: (Pos p) => FtlLexer [Lexeme p] p
ftlText = do
  lexemes <- many $ choice [
      comment,
      space,
      word,
      symbol,
      catchInvalidChar
    ]
  eofLexeme <- endOfInput
  return (lexemes ++ [eofLexeme])

-- | A line comment: Starts with '#' and ends at the next line break.
comment :: (Pos p) => FtlLexer (Lexeme p) p
comment = do
  state <- get
  let pos = position state
      cats = catCodes state
  prefix <- satisfy $ isCommentChar cats
  -- Consume as many characters as possible until either an invalid character,
  -- a vertical space or the end of input is reached:
  commentBody <- many $ satisfy $ \c ->
       isSpace cats c
    || isAlphanumChar cats c
    || isSymbol cats c
    || isCommentChar cats c
  -- If an invalid character is reached, chatch it (at the position that has
  -- been reached during the execution of the last line):
  optional $ catchInvalidCharAt (explodeString (prefix : commentBody) pos)
  -- If no invalid character is reached, expect a vertical space or the end of
  -- input:
  void (satisfy $ isLineBreak cats) <|> lookAhead eof
  let comment = prefix : commentBody ++ "\n"
      newPos = explodeString comment pos
      commentPosition = getStringPos comment pos
  put state{
      position = newPos
    }
  return $ Comment commentPosition

-- | White space: Longest possible string of ASCII space characters.
space :: (Pos p) => FtlLexer (Lexeme p) p
space = do
  state <- get
  let pos = position state
      cats = catCodes state
  space <- some (satisfy $ \c -> isSpace cats c || isLineBreak cats c)
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

-- | The end of the input text.
endOfInput :: (Pos p) => FtlLexer (Lexeme p) p
endOfInput = do
  currentPos <- gets position
  eof
  return $ EOF currentPos
