-- |
-- Module      : FTLex.Tex
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- TeX Lexer

module FTLex.Tex (
  CatCode(..),
  CatCodeMap,
  defaultCatCodes,
  runLexer,
  LexingState(..),
  initState,
  Lexeme(..),
  isCharacterLexeme,
  isControlWordLexeme,
  isControlSymbolLexeme,
  isControlSpaceLexeme,
  isParameterLexeme,
  isSkippedLexeme,
  isCommentLexeme,
  isEscapeCharLexeme,
  isBeginGroupCharLexeme,
  isEndGroupCharLexeme,
  isMathShiftCharLexeme,
  isAlignTabCharLexeme,
  isEndOfLineCharLexeme,
  isParamCharLexeme,
  isSuperscriptCharLexeme,
  isSubscriptCharLexeme,
  isIgnoredCharLexeme,
  isSpaceCharLexeme,
  isLetterCharLexeme,
  isOtherCharLexeme,
  isActiveCharLexeme,
  isCommentPrefixCharLexeme,
  isInvalidCharLexeme
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Control.Monad.State.Class (get, put, gets)
import Text.Megaparsec hiding (Pos)
import Data.Char qualified as Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, fromMaybe)
import FTLex.Position
import FTLex.Error
import FTLex.Message
import FTLex.Base (Lines(..), allowedChars)
import FTLex.Base qualified as Base
import FTLex.Helpers


-- * Category Codes

-- | Category Codes.
data CatCode =
    EscapeCharCat     -- ^  0: Escape character
  | BeginGroupCat     -- ^  1: Begin group character
  | EndGroupCat       -- ^  2: End group character
  | MathShiftCat      -- ^  3: Math shift character
  | AlignTabCat       -- ^  4: Alignment tab
  | EndOfLineCat      -- ^  5: Line break
  | ParamCharCat      -- ^  6: Parameter character
  | SuperscriptCat    -- ^  7: Superscript character
  | SubscriptCat      -- ^  8: Subscript character
  | IgnoredCat        -- ^  9: Ignored character
  | SpaceCat          -- ^ 10: Horizontal space
  | LetterCat         -- ^ 11: Letter
  | OtherCat          -- ^ 12: Other character
  | ActiveCat         -- ^ 13: Active character
  | CommentPrefixCat  -- ^ 14: Comment Prefix
  | InvalidCat        -- ^ 15: Invalid character
  | UnknownCat        -- ^     Unknown character
  deriving (Eq, Ord, Show)

-- | A map that assigns a character a category code. Any character not contained
-- in that map is supposed to throw an "unknown character" error during lexing.
type CatCodeMap = Map Char CatCode

-- | Checks whether a character is an escape character wrt. a given category
-- code mapping (default: @\\@).
isEscapeChar :: CatCodeMap -> Char -> Bool
isEscapeChar catCodeMap c =
  Map.lookup c catCodeMap == Just EscapeCharCat

-- | Checks whether a character is a begin group character wrt. a given
-- category code mapping (default: @{@).
isBeginGroupChar :: CatCodeMap -> Char -> Bool
isBeginGroupChar catCodeMap c =
  Map.lookup c catCodeMap == Just BeginGroupCat

-- | Checks whether a character is an end group character wrt. a given category
-- code mapping (default: @}@).
isEndGroupChar :: CatCodeMap -> Char -> Bool
isEndGroupChar catCodeMap c =
  Map.lookup c catCodeMap == Just EndGroupCat

-- | Checks whether a character is a math shift character wrt. a given category
-- code mapping (default: @$@).
isMathShiftChar :: CatCodeMap -> Char -> Bool
isMathShiftChar catCodeMap c =
  Map.lookup c catCodeMap == Just MathShiftCat

-- | Checks whether a character is an alignment tab wrt. a given category code
-- mapping (default: @&@).
isAlignTab :: CatCodeMap -> Char -> Bool
isAlignTab catCodeMap c =
  Map.lookup c catCodeMap == Just AlignTabCat

-- | Checks whether a character is a line break character wrt. a given category
-- code mapping (default: @\\r@).
isEndOfLine :: CatCodeMap -> Char -> Bool
isEndOfLine catCodeMap c =
  Map.lookup c catCodeMap == Just EndOfLineCat

-- | Checks whether a character is a parameter character wrt. a given category
-- code mapping (default: @#@).
isParamChar :: CatCodeMap -> Char -> Bool
isParamChar catCodeMap c =
  Map.lookup c catCodeMap == Just ParamCharCat

-- | Checks whether a character is a superscript character wrt. a given
-- category code mapping (default: @^@).
isSuperscriptChar :: CatCodeMap -> Char -> Bool
isSuperscriptChar catCodeMap c =
  Map.lookup c catCodeMap == Just SuperscriptCat

-- | Checks whether a character is a subscript character wrt. a given category
-- code mapping (default: @_@).
isSubscriptChar :: CatCodeMap -> Char -> Bool
isSubscriptChar catCodeMap c =
  Map.lookup c catCodeMap == Just SubscriptCat

-- | Checks whether a character is an ignored character wrt. a given category
-- code mapping (default: @\\NUL@).
isIgnoredChar :: CatCodeMap -> Char -> Bool
isIgnoredChar catCodeMap c =
  Map.lookup c catCodeMap == Just IgnoredCat

-- | Checks whether a character is a space wrt. a given category code mapping
-- (default: @ @).
isSpace :: CatCodeMap -> Char -> Bool
isSpace catCodeMap c =
  Map.lookup c catCodeMap == Just SpaceCat

-- | Checks whether a character is a letter wrt. a given category code mapping
-- (default: any ASCII letter).
isLetter :: CatCodeMap -> Char -> Bool
isLetter catCodeMap c =
  Map.lookup c catCodeMap == Just LetterCat

-- | Checks whether a character is an other character wrt. a given category
-- code mapping (default: any ASCII characterthat is not part of any other
-- character category).
isOtherChar :: CatCodeMap -> Char -> Bool
isOtherChar catCodeMap c =
  Map.lookup c catCodeMap == Just OtherCat

-- | Checks whether a character is an active character wrt. a given category
-- code mapping (default: @~@).
isActiveChar :: CatCodeMap -> Char -> Bool
isActiveChar catCodeMap c =
  Map.lookup c catCodeMap == Just ActiveCat

-- | Checks whether a character is a comment prefix wrt. a given category code
-- mapping (default: @%@).
isCommentPrefix :: CatCodeMap -> Char -> Bool
isCommentPrefix catCodeMap c =
  Map.lookup c catCodeMap == Just CommentPrefixCat

-- | Checks whether a character is an invalid character wrt. a given category
-- code mapping (default: @\\DEL@).
isInvalidChar :: CatCodeMap -> Char -> Bool
isInvalidChar catCodeMap c =
     Map.lookup c catCodeMap == Just InvalidCat

-- | Checks whether a character is an invalid character wrt. a given category
-- code mapping (default: any non-ASCII character).
isUnknownChar :: CatCodeMap -> Char -> Bool
isUnknownChar catCodeMap c = isNothing (Map.lookup c catCodeMap)

-- | Default category code mapping for TeX documents.
defaultCatCodes :: CatCodeMap
defaultCatCodes = Map.fromAscList [(c, defCatCode c) | c <- allowedChars]

-- | The default category code of a character.
defCatCode :: Char -> CatCode
defCatCode '\\' = EscapeCharCat
defCatCode '{' = BeginGroupCat
defCatCode '}' = EndGroupCat
defCatCode '$' = MathShiftCat
defCatCode '&' = AlignTabCat
defCatCode '\r' = EndOfLineCat
defCatCode '#' = ParamCharCat
defCatCode '^' = SuperscriptCat
defCatCode '_' = SubscriptCat
defCatCode '\t' = SpaceCat
defCatCode ' ' = SpaceCat
defCatCode c | c `elem` ['A' .. 'Z'] ++ ['a' .. 'z'] = LetterCat
defCatCode '~' = ActiveCat
defCatCode '%' = CommentPrefixCat
defCatCode '\NUL' = InvalidCat
defCatCode _ = OtherCat


-- * Lexemes

-- | TeX lexemes: TeX tokens (with additional source text and source position
-- information) plus two additional constructors for skipped characters and
-- comments.
data (Pos p) => Lexeme p =
    Character{
      charContent :: Char,
      charCatCode :: CatCode,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Character token
  | ControlWord{
      ctrlWordContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Control word
  | ControlSymbol{
      ctrlSymbolContent :: Char,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Control symbol
  | ControlSpace{
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Control space
  | Parameter{
      paramNumber :: Int,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Parameter token
  | Skipped{
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Skipped characters
  | Comment{
      commentContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Comment
  deriving (Eq, Ord, Show)

isCharacterLexeme :: (Pos p) => Lexeme p -> Bool
isCharacterLexeme Character{} = True
isCharacterLexeme _ = False

isControlWordLexeme :: (Pos p) => Lexeme p -> Bool
isControlWordLexeme ControlWord{} = True
isControlWordLexeme _ = False

isControlSymbolLexeme :: (Pos p) => Lexeme p -> Bool
isControlSymbolLexeme ControlSymbol{} = True
isControlSymbolLexeme _ = False

isControlSpaceLexeme :: (Pos p) => Lexeme p -> Bool
isControlSpaceLexeme ControlSpace{} = True
isControlSpaceLexeme _ = False

isParameterLexeme :: (Pos p) => Lexeme p -> Bool
isParameterLexeme Parameter{} = True
isParameterLexeme _ = False

isSkippedLexeme :: (Pos p) => Lexeme p -> Bool
isSkippedLexeme Skipped{} = True
isSkippedLexeme _ = False

isCommentLexeme :: (Pos p) => Lexeme p -> Bool
isCommentLexeme Comment{} = True
isCommentLexeme _ = False

isEscapeCharLexeme :: (Pos p) => Lexeme p -> Bool
isEscapeCharLexeme Character{charCatCode = catCode}
  | catCode == EscapeCharCat = True
isEscapeCharLexeme _ = False

isBeginGroupCharLexeme :: (Pos p) => Lexeme p -> Bool
isBeginGroupCharLexeme Character{charCatCode = catCode}
  | catCode == BeginGroupCat = True
isBeginGroupCharLexeme _ = False

isEndGroupCharLexeme :: (Pos p) => Lexeme p -> Bool
isEndGroupCharLexeme Character{charCatCode = catCode}
  | catCode == EndGroupCat = True
isEndGroupCharLexeme _ = False

isMathShiftCharLexeme :: (Pos p) => Lexeme p -> Bool
isMathShiftCharLexeme Character{charCatCode = catCode}
  | catCode == MathShiftCat = True
isMathShiftCharLexeme _ = False

isAlignTabCharLexeme :: (Pos p) => Lexeme p -> Bool
isAlignTabCharLexeme Character{charCatCode = catCode}
  | catCode == AlignTabCat = True
isAlignTabCharLexeme _ = False

isEndOfLineCharLexeme :: (Pos p) => Lexeme p -> Bool
isEndOfLineCharLexeme Character{charCatCode = catCode}
  | catCode == EndOfLineCat = True
isEndOfLineCharLexeme _ = False

isParamCharLexeme :: (Pos p) => Lexeme p -> Bool
isParamCharLexeme Character{charCatCode = catCode}
  | catCode == ParamCharCat = True
isParamCharLexeme _ = False

isSuperscriptCharLexeme :: (Pos p) => Lexeme p -> Bool
isSuperscriptCharLexeme Character{charCatCode = catCode}
  | catCode == SuperscriptCat = True
isSuperscriptCharLexeme _ = False

isSubscriptCharLexeme :: (Pos p) => Lexeme p -> Bool
isSubscriptCharLexeme Character{charCatCode = catCode}
  | catCode == SubscriptCat = True
isSubscriptCharLexeme _ = False

isIgnoredCharLexeme :: (Pos p) => Lexeme p -> Bool
isIgnoredCharLexeme Character{charCatCode = catCode}
  | catCode == IgnoredCat = True
isIgnoredCharLexeme _ = False

isSpaceCharLexeme :: (Pos p) => Lexeme p -> Bool
isSpaceCharLexeme Character{charCatCode = catCode}
  | catCode == SpaceCat = True
isSpaceCharLexeme _ = False

isLetterCharLexeme :: (Pos p) => Lexeme p -> Bool
isLetterCharLexeme Character{charCatCode = catCode}
  | catCode == LetterCat = True
isLetterCharLexeme _ = False

isOtherCharLexeme :: (Pos p) => Lexeme p -> Bool
isOtherCharLexeme Character{charCatCode = catCode}
  | catCode == OtherCat = True
isOtherCharLexeme _ = False

isActiveCharLexeme :: (Pos p) => Lexeme p -> Bool
isActiveCharLexeme Character{charCatCode = catCode}
  | catCode == ActiveCat = True
isActiveCharLexeme _ = False

isCommentPrefixCharLexeme :: (Pos p) => Lexeme p -> Bool
isCommentPrefixCharLexeme Character{charCatCode = catCode}
  | catCode == CommentPrefixCat = True
isCommentPrefixCharLexeme _ = False

isInvalidCharLexeme :: (Pos p) => Lexeme p -> Bool
isInvalidCharLexeme Character{charCatCode = catCode}
  | catCode == InvalidCat = True
isInvalidCharLexeme _ = False


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

-- | The initial lexing state.
initState :: (Pos p) => p -> LexingState p
initState pos = LexingState{
    position = pos,
    catCodes = defaultCatCodes,
    inputState = NewLine,
    endlineChar = Just '\r'
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
