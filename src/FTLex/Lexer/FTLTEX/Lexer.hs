-- |
-- Module      : FTLex.Lexer.FTLTEX.Lexer
-- Copyright   : (c) 2024-2025, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- Lexing the input text.

module FTLex.Lexer.FTLTEX.Lexer (
  runLexer
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Control.Monad.State.Class (get, put, gets)
import Text.Megaparsec hiding (Pos)
import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import FTLex.Lexer.TEX.Characters
import FTLex.Lexer.TEX.Lexemes
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

type TexLexer resultType p = Base.Lexer (LexingError p) (LexingState p) resultType


-- * Lexer State

data InputState =
    NewLine
  | SkippingSpaces
  | MiddleOfLine

-- | The current lexing state.
data (Pos p) => LexingState p = LexingState{
    position :: p,
    -- ^ Current position
    catCodes :: CatCodeMap,
    -- ^ Current category codes
    inputState :: InputState,
    -- ^ Current input state
    endlineChar :: Maybe Char
    -- ^ Current value of @\\endlinechar@
  }


-- * Running a Lexer

runLexer :: (Msg p m)
         => p             -- ^ Initial position
         -> Text          -- ^ Input text
         -> m [Lexeme p]
runLexer pos input =
  let state = LexingState{
        position = pos,
        catCodes = defaultCatCodes,
        inputState = NewLine,
        endlineChar = Just '\r'
      }
      lines = Base.splitText input
  in runLexer' pos lines state
  where
    runLexer' pos (MiddleLine line lineBreak rest) state = do
      -- Remove all trailing spaces from the first line:
      let trimmedLine = Text.dropWhileEnd (isSpace (catCodes state)) line
          trailingSpaces = Text.takeWhileEnd (isSpace (catCodes state)) line
      -- Lex the first line of the input text:
      (lexemes, newState) <- Base.runLexer
        texLine
        state{inputState = NewLine}
        trimmedLine
        (handleError makeErrMsg)
      -- Skip the trailing spaces:
      let newPos = position newState
          trailingSpacesPos = getPosOf trailingSpaces newPos
          newPos' = getNextPos trailingSpaces newPos
          spaceLexeme = if Text.null trailingSpaces
            then []
            else singleton Skipped{
                sourceText = trailingSpaces,
                sourcePos = trailingSpacesPos
              }
      -- Turn the line break into a line break character lexeme (or skip it if there
      -- is no @\\endline@ character):
      let lineBreakPos = getPosOf lineBreak newPos'
          newPos'' = getNextPos lineBreak newPos'
          (lineBreakLexeme, newState') = case endlineChar state of
            Nothing -> pair
              Skipped{
                sourceText = lineBreak,
                sourcePos = lineBreakPos
              }
              newState{
                position = newPos''
              }
            Just c -> case inputState newState of
              -- In state N, i.e. if the line so far contained at most spaces, insert
              -- a @\\par@ token:
              NewLine -> pair
                ControlWord{
                  ctrlWordContent = "par",
                  sourceText = lineBreak,
                  sourcePos = lineBreakPos
                }
                newState{
                  position = newPos'',
                  inputState = NewLine
                }
              -- In state S, insert nothing:
              SkippingSpaces -> pair
                Skipped{
                  sourceText = lineBreak,
                  sourcePos = lineBreakPos
                }
                newState{
                  position = newPos'',
                  inputState = NewLine
                }
              -- In state M, insert a space token:
              MiddleOfLine -> pair
                Character{
                  charContent = ' ', 
                  charCatCode = SpaceCat,
                  sourceText = lineBreak,
                  sourcePos = lineBreakPos
                }
                newState{
                  position = newPos'',
                  inputState = NewLine
                }
      restLexemes <- runLexer' newPos'' rest newState'
      return $ lexemes ++ spaceLexeme ++ [lineBreakLexeme] ++ restLexemes
    runLexer' pos (LastLine line) state = do
      (lexemes, _) <- Base.runLexer
        texLine
        state{inputState = NewLine}
        line
        (handleError makeErrMsg)
      return lexemes


-- * Lexer Combinators

-- | A single line of a ForTheL text in the TeX dialect.
texLine :: (Pos p) => TexLexer ([Lexeme p], LexingState p) p
texLine = do
  lexemes <- many $ choice [
      comment,
      controlSequence,
      character,
      parameter,
      space,
      endOfLine,
      ignoredCharacter,
      catchInvalidChar,
      catchUnknownChar
    ]
  eof
  state <- get
  return (concat lexemes, state)


-- ** Control Sequences

controlSequence :: (Pos p) => TexLexer [Lexeme p] p
controlSequence = try controlWord <|> try controlSymbol <|> try controlSpace <|> controlSpace'

controlWord :: (Pos p) => TexLexer [Lexeme p] p
controlWord = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  (escapeChar, escapeCharSourceText) <- satisfy' $ isEscapeChar cats
  (word, wordSourceText) <- satisfySome' $ isLetter cats
  let sourceText = escapeCharSourceText <> wordSourceText
      newPos = getNextPos sourceText pos
      commandPos = getPosOf sourceText pos
  put state{
      position = newPos,
      inputState = SkippingSpaces
    }
  return $ singleton ControlWord{
      ctrlWordContent = word,
      sourceText = sourceText,
      sourcePos = commandPos
    }

controlSymbol :: (Pos p) => TexLexer [Lexeme p] p
controlSymbol = do
  state <- get
  cats <- gets catCodes
  let pos = position state
      isAllowedChar = disjunction [
          isEscapeChar cats,
          isBeginGroupChar cats,
          isEndGroupChar cats,
          isMathShiftChar cats,
          isAlignTab cats,
          isEndOfLine cats,
          isParamChar cats,
          isSuperscriptChar cats,
          isSubscriptChar cats,
          isIgnoredChar cats,
          isOtherChar cats,
          isActiveChar cats,
          isCommentPrefix cats,
          isInvalidChar cats -- Yes, an invalid character is acceptable.
        ]
  (escapeChar, escapeCharSourceText) <- satisfy' $ isEscapeChar cats
  (symbol, symbolSourceText) <- satisfy' isAllowedChar
  let sourceText = escapeCharSourceText <> symbolSourceText
      newPos = getNextPos sourceText pos
      commandPos = getPosOf sourceText pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton ControlSymbol{
      ctrlSymbolContent = symbol,
      sourceText = sourceText,
      sourcePos = commandPos
    }

controlSpace :: (Pos p) => TexLexer [Lexeme p] p
controlSpace = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  (escapeChar, escapeCharSourceText) <- satisfy' $ isEscapeChar cats
  (symbol, symbolSourceText) <- satisfy' $ isSpace cats
  let sourceText = escapeCharSourceText <> symbolSourceText
      newPos = getNextPos sourceText pos
      commandPos = getPosOf sourceText pos
  put state{
      position = newPos,
      inputState = SkippingSpaces
    }
  return $ singleton ControlSpace{
      sourceText = sourceText,
      sourcePos = commandPos
    }

-- | A backslash without any following character, i.e. a backslash followed by a
-- line break.
controlSpace' :: (Pos p) => TexLexer [Lexeme p] p
controlSpace' = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  (escapeChar, escapeCharSourceText) <- satisfy' $ isEscapeChar cats
  lookAhead eof
  let sourceText = escapeCharSourceText
      newPos = getNextPos sourceText pos
      commandPos = getPosOf sourceText pos
  put state{
      position = newPos,
      inputState = SkippingSpaces
    }
  return $ singleton ControlSpace{
      sourceText = sourceText,
      sourcePos = commandPos
    }


-- ** "Normal" Characters

-- | A character with one of the following category codes:
-- *  1 (begin group)
-- *  2 (end group)
-- *  3 (math shift)
-- *  4 (alignment tab)
-- *  7 (superscript)
-- *  8 (subscript)
-- * 11 (letter)
-- * 12 (other)
-- * 13 (active)
character :: (Pos p) => TexLexer [Lexeme p] p
character = do
  state <- get
  let cats = catCodes state
      pos = position state
      isAllowedChar = disjunction [
          isBeginGroupChar cats,
          isEndGroupChar cats,
          isMathShiftChar cats,
          isAlignTab cats,
          isSuperscriptChar cats,
          isSubscriptChar cats,
          isLetter cats,
          isOtherChar cats,
          isActiveChar cats
        ]
  -- Consume a single character that makes a character token:
  (char, sourceText) <- satisfy' isAllowedChar
  let catCode = fromMaybe InvalidCat (Map.lookup char cats)
      charPos = getPosOf sourceText pos
      newPos = getNextPos sourceText pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton Character{
      charContent = char,
      charCatCode = catCode,
      sourceText = sourceText,
      sourcePos = charPos
    }


-- ** Parameter

parameter :: (Pos p) => TexLexer [Lexeme p] p
parameter = try param <|> consecParamChars

-- | A parameter character.
param :: (Pos p) => TexLexer [Lexeme p] p
param = do
  state <- get
  let cats = catCodes state
      pos = position state
  (paramChar, paramCharSourceText) <- satisfy' (isParamChar cats)
  (digit, digitSourceText) <- satisfy' Char.isDigit
  let sourceText = paramCharSourceText <> digitSourceText
      paramPos = getPosOf sourceText pos
      newPos = getNextPos sourceText pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton Parameter{
      paramNumber = Char.digitToInt digit,
      sourceText = sourceText,
      sourcePos = paramPos
    }

-- | Two consecutive parameter character.
consecParamChars :: (Pos p) => TexLexer [Lexeme p] p
consecParamChars = do
  state <- get
  let cats = catCodes state
      pos = position state
  (fstParamChar, fstParamCharSourceText) <- satisfy' (isParamChar cats)
  (sndParamChar, sndParamCharSourceText) <- satisfy' (isParamChar cats)
  let sourceText = fstParamCharSourceText <> sndParamCharSourceText
      paramPos = getPosOf sourceText pos
      newPos = getNextPos sourceText pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton Character{
      charContent = sndParamChar,
      charCatCode = ParamCharCat,
      sourceText = sourceText,
      sourcePos = paramPos
    }


-- ** End of Line

-- | An end-of-line character:
endOfLine :: (Pos p) => TexLexer [Lexeme p] p
endOfLine = do
  state <- get
  let cats = catCodes state
      pos = position state
  -- Consume an end-of-line character:
  (char, sourceText) <- satisfy' $ isEndOfLine cats
  let charPos = getPosOf sourceText pos
      newPos = getNextPos sourceText pos
  -- Consume the rest of the line (which will be skipped):
  rest <- Text.pack <$> many (satisfy (negation (isUnknownChar cats)))
  let newPos' = getNextPos rest newPos
      restPos = getPosOf rest newPos
  -- Throw an error if there appears an unknown character in the rest of the
  -- line:
  optional $ catchUnknownCharAt newPos
  -- Turn the line break character into a token (which depends on the current
  -- input state):
  let lexeme = case inputState state of
        -- In state N, i.e. if the line so far contained at most spaces, insert
        -- a @\\par@ token:
        NewLine -> ControlWord{
            ctrlWordContent = "par",
            sourceText = sourceText,
            sourcePos = charPos
          }
        -- In state S, insert nothing:
        SkippingSpaces -> Skipped{
            sourceText = sourceText,
            sourcePos = charPos
          }
        -- In state M, insert a space token:
        MiddleOfLine -> Character{
            charContent = ' ', 
            charCatCode = SpaceCat,
            sourceText = sourceText,
            sourcePos = charPos
          }
  -- Skip the rest of the line:
  let skippedLexeme = Skipped{
          sourceText = rest,
          sourcePos = restPos
        }
  put state{
      position = newPos',
      inputState = NewLine
    }
  return [lexeme, skippedLexeme]


-- ** Space

-- | A space character.
space :: (Pos p) => TexLexer [Lexeme p] p
space = do
  state <- get
  let cats = catCodes state
      pos = position state
  (spaceChar, sourceText) <- satisfy' (isSpace cats)
  let spacePos = getPosOf sourceText pos
      newPos = getNextPos sourceText pos
      (newInputState, lexeme) = case inputState state of
        -- In state N, skip the space and stay in state N:
        NewLine -> pair NewLine Skipped{
            sourceText = sourceText,
            sourcePos = spacePos
          }
        -- In state S, skip the space and stay in state S:
        SkippingSpaces -> pair SkippingSpaces Skipped{
            sourceText = sourceText,
            sourcePos = spacePos
          }
        -- In state M, insert a space token and go into state S:
        MiddleOfLine -> pair SkippingSpaces Character{
            charContent = ' ', 
            charCatCode = SpaceCat,
            sourceText = sourceText,
            sourcePos = spacePos
          }
  put state{
      position = newPos,
      inputState = newInputState
    }
  return [lexeme]


-- ** Comments

-- | A comment.
comment :: (Pos p) => TexLexer [Lexeme p] p
comment = do
  state <- get
  let cats = catCodes state
      pos = position state
  (prefix, prefixSourceText) <- satisfy' (isCommentPrefix cats)
  commentBody <- takeWhileP Nothing (neitherNor [
      isUnknownChar cats,
      isEndOfLine cats
    ])
  let sourceText = prefixSourceText <> commentBody
      commentPos = getPosOf sourceText pos
      newPos = getNextPos sourceText pos
  -- Throw an error if there appears an unknown character in the rest of the
  -- line:
  optional $ catchUnknownCharAt newPos
  --  Otherwise, insert a comment lexeme:
  let commentLexeme = Comment{
      commentContent = commentBody,
      sourceText = sourceText,
      sourcePos = commentPos
    }
  put state{
      position = newPos,--newPos'
      inputState = SkippingSpaces
    }
  return [commentLexeme]--, skippedLineBreak]


-- ** Ignored Characters

-- | An ignored character.
ignoredCharacter :: (Pos p) => TexLexer [Lexeme p] p
ignoredCharacter = do
  state <- get
  let cats = catCodes state
      pos = position state
  (char, sourceText) <- satisfy' (isIgnoredChar cats)
  let charPos = getPosOf sourceText pos
      newPos = getNextPos sourceText pos
  put state{
      position = newPos
    }
  return $ singleton Skipped{
      sourceText = sourceText,
      sourcePos = charPos
    }


-- ** Invalid Characters

-- | Catch an invalid character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidCharAt :: (Pos p) => p -> TexLexer a p
catchInvalidCharAt pos = do
  state <- get
  let cats = catCodes state
  (char, sourceText) <- satisfy' $ isInvalidChar cats
  let charPos = getPosOf sourceText pos
      err = InvalidChar char charPos
  customFailure err

-- | Catch an invalid character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidChar :: (Pos p) => TexLexer a p
catchInvalidChar = do
  pos <- gets position
  catchInvalidCharAt pos


-- ** Unknown Characters

-- | Catch an unknown character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownCharAt :: (Pos p) => p -> TexLexer a p
catchUnknownCharAt pos = do
  state <- get
  let cats = catCodes state
  char <- satisfy $ \c -> Map.notMember c cats
  let charPos = getPosOf (Text.singleton char) pos
      err = UnknownChar char charPos
  customFailure err

-- | Catch an unknown character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownChar :: (Pos p) => TexLexer a p
catchUnknownChar = do
  pos <- gets position
  catchUnknownCharAt pos


-- ** Single Characters

-- | An expression of the form @xyab@, where @x@ and @y@ are two identical
-- superscript characters and @a@ and @b@ are two lower-case hexadecimal
-- digits. Moreover, the character @char@ with (hexadecimal) ASCII code @ab@
-- must satisfy a given predicate. Returns @char@ and the source text @xyab@.
doubleSuperscript :: (Pos p) => (Char -> Bool) -> TexLexer (Char, Text) p
doubleSuperscript pred = do
  state <- get
  let cats = catCodes state
  fstSupChar <- satisfy (isSuperscriptChar cats)
  sndSupChar <- satisfy (== fstSupChar)
  fstHexDigit <- satisfy isLowerHex
  sndHexDigit <- satisfy isLowerHex
  let char = Char.chr . fromHex . Text.pack $ [fstHexDigit, sndHexDigit]
      sourceText = Text.singleton fstSupChar <> Text.singleton sndSupChar <> Text.singleton fstHexDigit <> Text.singleton sndHexDigit
  if pred char
    then return (char, sourceText)
    else empty
  where
    isLowerHex c = Char.isDigit c || ('a' <= c && c <= 'f')

-- | A single character @char@ that satisfies a given predicate. Returns @char@,
-- and the source text @xyab@.
charSatisfyingPred :: (Pos p) => (Char -> Bool) -> TexLexer (Char, Text) p
charSatisfyingPred pred = do
  char <- satisfy pred
  let sourceText = Text.singleton char
  return (char, sourceText)

-- | A (possibly double-superscrip-escaped) character that satisfies a given
-- predicate.
satisfy' :: (Pos p) => (Char -> Bool) -> TexLexer (Char, Text) p
satisfy' pred = try (doubleSuperscript pred) <|> charSatisfyingPred pred

-- | A non-empty sequence of (possibly double-superscrip-escaped) characters
-- that satisfies a given predicate.
satisfySome' :: (Pos p) => (Char -> Bool) -> TexLexer (Text, Text) p
satisfySome' pred = do
  result <- some $ satisfy' pred
  let text = Text.pack $ map fst result
      sourceText = Text.concat $ map snd result
  return (text, sourceText)
