-- |
-- Module      : Ftlex.Tex
-- Copyright   : (c) 2024, Marcel Schütz
-- License     : LGPL-3
-- Maintainer  : marcel.schuetz@fau.de
--
-- TeX Lexer

{-# LANGUAGE OverloadedStrings #-}

module Ftlex.Tex (
  CatCode(..),
  CatCodeMap,
  defaultCatCodes,
  runLexer,
  initState,
  Lexeme(..)
) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Control.Monad.State.Class (get, put, gets)
import Text.Megaparsec hiding (Pos)
import Data.Char qualified as Char
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, fromMaybe)
import Ftlex.Position
import Ftlex.Error
import Ftlex.Message
import Ftlex.Base (LineBreakType(..), UnicodeBlock(..))
import Ftlex.Base qualified as Base
import Ftlex.Helpers


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
  deriving Eq

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
defCatCode '\x005C' = EscapeCharCat     -- '\\'
defCatCode '\x007B' = BeginGroupCat     -- '{'
defCatCode '\x007D' = EndGroupCat       -- '}'
defCatCode '\x0024' = MathShiftCat      -- '$'
defCatCode '\x0026' = AlignTabCat       -- '&'
defCatCode '\x000D' = EndOfLineCat      -- Carriage Return
defCatCode '\x0023' = ParamCharCat      -- '#'
defCatCode '\x005E' = SuperscriptCat    -- '^'
defCatCode '\x005F' = SubscriptCat      -- '_'
defCatCode '\x0000' = IgnoredCat        -- Null Character
defCatCode c
  | isDefSpace c = SpaceCat
  | isDefLetter c = LetterCat
  | isDefOther c = OtherCat
defCatCode '\x007E' = ActiveCat         -- '~'
defCatCode '\x0025' = CommentPrefixCat  -- '%'
defCatCode _ = InvalidCat

-- | Default space characters.
isDefSpace :: Char -> Bool
isDefSpace c = elem c $
     ['\x0020']  -- Space
  -- Latin-1 Supplement:
  ++ ['\x00A0'] -- Non-breakable space

-- | Default alphanumeric characters.
isDefLetter :: Char -> Bool
isDefLetter c = elem c $
  -- Basic Latin:
     ['\x0041' .. '\x005A'] -- 'A' – 'Z'
  ++ ['\x0061' .. '\x007A'] -- 'a' – 'z'
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

-- | Default other characters.
isDefOther :: Char -> Bool
isDefOther c = elem c $
  -- Basic Latin:
     ['\x0001' .. '\x000C'] -- C0 Controls (before Carriage Return)
  ++ ['\x000E' .. '\x001F'] -- C0 Controls (after Carriage Return)
  ++ ['\x0021' .. '\x0022'] -- '!' – '"'
  ++ ['\x0027' .. '\x002F'] -- '\'' – '/'
  ++ ['\x0030' .. '\x0039'] -- '0' – '9'
  ++ ['\x003A' .. '\x0040'] -- ':' – '@'
  ++ ['\x005B']             -- '['
  ++ ['\x005D' .. '\x0060'] -- ']' – '`'
  ++ ['\x007C']             -- '|'
  -- Latin-1 Supplement:
  ++ ['\x0080' .. '\x00BF'] -- C1 Controls 
  ++ ['\x00A1' .. '\x00F6'] -- '¡' – '¿'
  ++ ['\x00D7']             -- '×'
  ++ ['\x00F7']             -- '÷'


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
    endlineChar :: Maybe Char,
    -- ^ Current value of @\\endlinechar@
    unicodeBlocks :: Set UnicodeBlock
    -- ^ Unicode blocks whose characters are allowed in the input text
  }

-- | The initial lexing state.
initState :: (Pos p) => p -> Set UnicodeBlock -> LexingState p
initState pos blocks = LexingState{
    position = pos,
    catCodes = defaultCatCodes blocks,
    inputState = NewLine,
    endlineChar = Just '\r',
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
          spaceLexeme =
            if Text.null trailingSpaces
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
                position = newPos'
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
                  position = newPos',
                  inputState = NewLine
                }
              -- In state S, insert nothing:
              SkippingSpaces -> pair
                Skipped{
                  sourceText = lineBreak,
                  sourcePos = lineBreakPos
                }
                newState{
                  position = newPos',
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
                  position = newPos',
                  inputState = NewLine
                }
      -- Repeat the procedure for the remainder of the input text:
      restLexemes <- if Text.null rest
        then pure []
        else runLexer newPos'' rest newState' lineBreakType
      return $ lexemes ++ spaceLexeme ++ [lineBreakLexeme] ++ restLexemes


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
controlSequence = try controlWord <|> try controlSymbol <|> controlSpace

controlWord :: (Pos p) => TexLexer [Lexeme p] p
controlWord = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  escapeChar <- satisfy' $ isEscapeChar cats
  word <- Text.pack <$> some (satisfy' (isLetter cats))
  let command = Text.cons escapeChar word
      newPos = getNextPos command pos
      commandPos = getPosOf command pos
  put state{
      position = newPos,
      inputState = SkippingSpaces
    }
  return $ singleton ControlWord{
      ctrlWordContent = word,
      sourceText = command,
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
  escapeChar <- satisfy' $ isEscapeChar cats
  symbol <- satisfy' isAllowedChar
  let command = Text.cons escapeChar (Text.singleton symbol)
      newPos = getNextPos command pos
      commandPos = getPosOf command pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton ControlSymbol{
      ctrlSymbolContent = symbol,
      sourceText = command,
      sourcePos = commandPos
    }

controlSpace :: (Pos p) => TexLexer [Lexeme p] p
controlSpace = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  escapeChar <- satisfy' (isEscapeChar cats)
  symbol <- satisfy' (isSpace cats)
  let command = Text.cons escapeChar (Text.singleton symbol)
      newPos = getNextPos command pos
      commandPos = getPosOf command pos
  put state{
      position = newPos,
      inputState = SkippingSpaces
    }
  return $ singleton ControlSpace{
      sourceText = command,
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
  char <- satisfy' isAllowedChar
  let catCode = fromMaybe InvalidCat (Map.lookup char cats)
      charText = Text.singleton char
      charPos = getPosOf charText pos
      newPos = getNextPos charText pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton Character{
      charContent = char,
      charCatCode = catCode,
      sourceText = charText,
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
  paramChar <- satisfy' (isParamChar cats)
  digit <- satisfy' Char.isDigit
  let paramText = Text.pack [paramChar, digit]
      paramPos = getPosOf paramText pos
      newPos = getNextPos paramText pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton Parameter{
      paramNumber = Char.digitToInt digit,
      sourceText = paramText,
      sourcePos = paramPos
    }

-- | Two consecutive parameter character.
consecParamChars :: (Pos p) => TexLexer [Lexeme p] p
consecParamChars = do
  state <- get
  let cats = catCodes state
      pos = position state
  fstParamChar <- satisfy' (isParamChar cats)
  sndParamChar <- satisfy' (isParamChar cats)
  let paramText = Text.pack [fstParamChar, sndParamChar]
      paramPos = getPosOf paramText pos
      newPos = getNextPos paramText pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ singleton Character{
      charContent = sndParamChar,
      charCatCode = ParamCharCat,
      sourceText = paramText,
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
  char <- satisfy' $ isEndOfLine cats
  let charText = Text.singleton char
      charPos = getPosOf charText pos
      newPos = getNextPos charText pos
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
            sourceText = charText,
            sourcePos = charPos
          }
        -- In state S, insert nothing:
        SkippingSpaces -> Skipped{
            sourceText = charText,
            sourcePos = charPos
          }
        -- In state M, insert a space token:
        MiddleOfLine -> Character{
            charContent = ' ', 
            charCatCode = SpaceCat,
            sourceText = charText,
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
  spaceChar <- satisfy' (isSpace cats)
  let spaceText = Text.singleton spaceChar
      spacePos = getPosOf spaceText pos
      newPos = getNextPos spaceText pos
      (newInputState, lexeme) = case inputState state of
        -- In state N, skip the space and stay in state N:
        NewLine -> pair NewLine Skipped{
            sourceText = spaceText,
            sourcePos = spacePos
          }
        -- In state S, skip the space and stay in state S:
        SkippingSpaces -> pair SkippingSpaces Skipped{
            sourceText = spaceText,
            sourcePos = spacePos
          }
        -- In state M, insert a space token and go into state S:
        MiddleOfLine -> pair SkippingSpaces Character{
            charContent = ' ', 
            charCatCode = SpaceCat,
            sourceText = spaceText,
            sourcePos = spacePos
          }
  put state{
      position = newPos,
      inputState = newInputState
    }
  return $ [lexeme]


-- ** Comments

-- | A comment.
comment :: (Pos p) => TexLexer [Lexeme p] p
comment = do
  state <- get
  let cats = catCodes state
      pos = position state
  prefix <- satisfy' (isCommentPrefix cats)
  commentBody <- takeWhileP Nothing (neitherNor [
      isUnknownChar cats,
      isEndOfLine cats
    ])
  let commentText = Text.cons prefix commentBody
      commentPos = getPosOf commentText pos
      newPos = getNextPos commentText pos
  -- Throw an error if there appears an unknown character in the rest of the
  -- line:
  optional $ catchUnknownCharAt newPos
  --  Otherwise, insert a comment lexeme:
  let commentLexeme = Comment{
      commentContent = commentBody,
      sourceText = commentText,
      sourcePos = commentPos
    }
  -- Skip the following end-of-line character:
  lineBreakChar <- satisfy' $ isEndOfLine cats
  let lineBreakText = Text.singleton lineBreakChar
      lineBreakPos = getPosOf lineBreakText newPos
      newPos' = getNextPos lineBreakText newPos
      skippedLineBreak = Skipped{
          sourceText = lineBreakText,
          sourcePos = lineBreakPos
        }
  put state{
      position = newPos'
    }
  return [commentLexeme, skippedLineBreak]


-- ** Ignored Characters

-- | An ignored character.
ignoredCharacter :: (Pos p) => TexLexer [Lexeme p] p
ignoredCharacter = do
  state <- get
  let cats = catCodes state
      pos = position state
  char <- satisfy' (isIgnoredChar cats)
  let charText = Text.singleton char
      charPos = getPosOf charText pos
      newPos = getNextPos charText pos
  put state{
      position = newPos
    }
  return $ singleton Skipped{
      sourceText = charText,
      sourcePos = charPos
    }


-- ** Invalid Characters

-- | Catch an invalid character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidCharAt :: (Pos p) => p -> TexLexer a p
catchInvalidCharAt pos = do
  state <- get
  let cats = catCodes state
  char <- satisfy' $ isInvalidChar cats
  let charPos = getPosOf (Text.singleton char) pos
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
      blocks = unicodeBlocks state
  char <- satisfy $ \c -> Map.notMember c cats
  let charPos = getPosOf (Text.singleton char) pos
      err = UnknownChar char charPos blocks
  customFailure err

-- | Catch an unknown character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchUnknownChar :: (Pos p) => TexLexer a p
catchUnknownChar = do
  pos <- gets position
  catchUnknownCharAt pos


-- ** Double-superscript-escaped characters

-- | An expression of the form @xyab@, where @x@ and @y@ are two identical
-- superscript characters and @a@ and @b@ are two lower-case hexadecimal
-- digits. Moreover, the character @char@ with (hexadecimal) ASCII code @ab@
-- must satisfy a given predicate. Returns @char@.
doubleSuperscript :: (Pos p) => (Char -> Bool) -> TexLexer Char p
doubleSuperscript pred = do
  state <- get
  -- Consumes a chunk @chunk@ that satisfies
  -- @isDoubleSuperscriptExpr state chunk ""@ (the argument @""@ is irrelevant;
  -- we could have chosen any other string as well):
  expr <- tokens (isDoubleSuperscriptExpr state) ""
  -- @expr@ is of the form @xyab@, where @x@ and @y@ are two identical
  -- superscript characters and @a@ and @b@ are two lower-case hexadecimal
  -- digits. Thus @char@ is the hexadecimal number @ab@:
  let charCode = Text.drop 2 expr
      -- @char@ is the character with (hexadecimal) ASCII code @ab@:
      char = (Char.chr . fromHex) charCode
  return char
  where
    isDoubleSuperscriptExpr :: (Pos p) => LexingState p -> Text -> Text -> Bool
    isDoubleSuperscriptExpr state txt _ = case Text.unpack txt of
      [x, y, a, b] -> and [
          -- @x@ must be a superscript character:
          isSuperscriptChar (catCodes state) x,
          -- @y@ must be identical with @x@ (and hence in particular also a
          -- superscript character):
          y == x,
          -- @a@ and @b@ must be lower-case hexadecimal digits:
          isLowerHex a,
          isLowerHex b,
          -- The character with (hexadecimal) ASCII code @ab@ must satisfy the
          -- predicate @pred@:
          pred $ (Char.chr . fromHex . Text.pack) [a, b]
        ]
      _ -> False
    isLowerHex c = Char.isDigit c || ('a' <= c && c <= 'f')


-- * Helpers

-- | A (possibly double-superscrip-escaped) character that satisfies a given
-- predicate.
satisfy' :: (Pos p) => (Char -> Bool) -> TexLexer Char p
satisfy' pred = try (doubleSuperscript pred) <|> satisfy pred
