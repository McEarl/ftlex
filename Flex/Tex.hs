-- |
-- Creator: Marcel Schütz (2024)
--
-- TeX Lexer

{-# LANGUAGE OverloadedStrings #-}

module Flex.Tex (
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
    EscapeCharCat     -- ^ Escape character
  | BeginGroupCat     -- ^ Begin group character
  | EndGroupCat       -- ^ End group character
  | MathShiftCat      -- ^ Math shift character
  | AlignTabCat       -- ^ Alignment tab
  | EndOfLineCat      -- ^ Line break
  | ParamCharCat      -- ^ Parameter character
  | SuperscriptCat    -- ^ Superscript character
  | SubscriptCat      -- ^ Subscript character
  | IgnoredCat        -- ^ Ignored character
  | SpaceCat          -- ^ Horizontal space
  | LetterCat         -- ^ Letter
  | OtherCat          -- ^ Other character
  | ActiveCat         -- ^ Active character
  | CommentPrefixCat  -- ^ Comment Prefix
  | InvalidCat        -- ^ Ignored character
  deriving Eq

type CatCodeMap = Map.Map Char CatCode

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
-- code mapping (default: any non-ASCII character).
isInvalidChar :: CatCodeMap -> Char -> Bool
isInvalidChar catCodeMap c =
     Map.lookup c catCodeMap == Just InvalidCat
  || isNothing (Map.lookup c catCodeMap)

-- | Default category code mapping for TeX documents.
defaultCatCodes :: CatCodeMap
defaultCatCodes = Map.fromAscList [(c, initCatCode c) | c <- ['\NUL' .. '\DEL']]
  where
    initCatCode :: Char -> CatCode
    initCatCode '\\' = EscapeCharCat
    initCatCode '{' = BeginGroupCat
    initCatCode '}' = EndGroupCat
    initCatCode '$' = MathShiftCat
    initCatCode '&' = AlignTabCat
    initCatCode '\r' = EndOfLineCat
    initCatCode '#' = ParamCharCat
    initCatCode '^' = SuperscriptCat
    initCatCode '_' = SubscriptCat
    initCatCode '\NUL' = IgnoredCat
    initCatCode ' ' = SpaceCat
    initCatCode c
      | Char.isAsciiUpper c = LetterCat
      | Char.isAsciiLower c = LetterCat
    initCatCode '~' = ActiveCat
    initCatCode '%' = CommentPrefixCat
    initCatCode c
      | c <= '\DEL' = OtherCat
    initCatCode _ = InvalidCat


-- * Lexemes

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
  | Space{
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Space
  | Comment{
      commentContent :: Text,
      sourceText :: Text,
      sourcePos :: p
    } -- ^ Comment


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
         -> LineBreakType -- ^ Line break type
         -> m [Lexeme p]
runLexer pos text state lineBreakType = do
  -- Split the input text at the first linebreak:
  let (line, lineBreak, rest) = splitText lineBreakType text
  -- Remove all trailing spaces from the first line:
  let trimmedLine = Text.dropWhileEnd (isSpace (catCodes state)) line
      trailingSpaces = Text.takeWhileEnd (isSpace (catCodes state)) line
  -- Lex the first line of the input text:
  (lexemes, newState) <- Base.runLexer
    texLine
    state{inputState = NewLine}
    trimmedLine
    (handleError makeErrMsg)
  -- Turn the trailing spaces into a space lexeme:
  let newPos = position newState
      trailingSpacesPos = getPosOf trailingSpaces newPos
      newPos' = getNextPos trailingSpaces newPos
      spaceLexeme = if Text.null trailingSpaces
        then singleton Space{
            sourceText = trailingSpaces,
            sourcePos = trailingSpacesPos
          }
        else []
  -- Turn the line break into a line break character lexeme (or a space lexeme
  -- if there is no @\\endline@ character):
  let lineBreakPos = getPosOf lineBreak newPos'
      newPos'' = getNextPos lineBreak newPos'
      lineBreakLexeme = case endlineChar state of
        Nothing -> singleton Space{
            sourceText = lineBreak,
            sourcePos = lineBreakPos
          }
        Just c -> singleton Character{
            charContent = c,
            charCatCode = EndOfLineCat,
            sourceText = lineBreak,
            sourcePos = lineBreakPos
          }
      newState' = newState{position = newPos''}
  -- Repeat the procedure for the remainder of the input text:
  restLexemes <- if Text.null rest
    then pure []
    else runLexer newPos'' rest newState'{inputState = NewLine} lineBreakType
  return $ lexemes ++ spaceLexeme ++ lineBreakLexeme ++ restLexemes


-- * Lexer Combinators

-- | A single line of a ForTheL text in the TeX dialect.
texLine :: (Pos p) => TexLexer ([Lexeme p], LexingState p) p
texLine = undefined


-- ** Control Sequences

controlSequence :: (Pos p) => TexLexer (Lexeme p) p
controlSequence = try controlWord <|> try controlSymbol <|> controlSpace

controlWord :: (Pos p) => TexLexer (Lexeme p) p
controlWord = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  escapeChar <- satisfy $ isEscapeChar cats
  word <- Text.pack <$> some (choice [
      satisfy (isLetter cats),
      doubleSuperscript (isLetter cats)
    ])
  let command = Text.cons escapeChar word
      newPos = getNextPos command pos
      commandPos = getPosOf command pos
  put state{
      position = newPos,
      inputState = SkippingSpaces
    }
  return $ ControlWord{
      ctrlWordContent = word,
      sourceText = command,
      sourcePos = commandPos
    }

controlSymbol :: (Pos p) => TexLexer (Lexeme p) p
controlSymbol = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  escapeChar <- satisfy $ isEscapeChar cats
  symbol <- choice [
      doubleSuperscript (isSymbolChar cats),
      satisfy (isSymbolChar cats)
    ]
  let command = Text.cons escapeChar (Text.singleton symbol)
      newPos = getNextPos command pos
      commandPos = getPosOf command pos
  put state{
      position = newPos,
      inputState = MiddleOfLine
    }
  return $ ControlSymbol{
      ctrlSymbolContent = symbol,
      sourceText = command,
      sourcePos = commandPos
    }
  where
    isSymbolChar cats = disjunction [
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

controlSpace :: (Pos p) => TexLexer (Lexeme p) p
controlSpace = do
  state <- get
  cats <- gets catCodes
  let pos = position state
  escapeChar <- satisfy $ isEscapeChar cats
  symbol <- choice [
      doubleSuperscript (isSpace cats),
      satisfy (isSpace cats)
    ]
  let command = Text.cons escapeChar (Text.singleton symbol)
      newPos = getNextPos command pos
      commandPos = getPosOf command pos
  put state{
      position = newPos,
      inputState = SkippingSpaces
    }
  return $ ControlSpace{
      sourceText = command,
      sourcePos = commandPos
    }


-- ** Invalid Characters

-- | Catch an invalid character and report it together with a given position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidCharAt :: (Pos p) => p -> TexLexer a p
catchInvalidCharAt pos = do
  state <- get
  let cats = catCodes state
  char <- satisfy $ isInvalidChar cats
  let charPos = getPosOf (Text.singleton char) pos
      err = InvalidChar char charPos
  customFailure err

-- | Catch an invalid character and report it together with the current position
-- (which is intended to be the position of that character) as a lexing error.
catchInvalidChar :: (Pos p) => TexLexer a p
catchInvalidChar = do
  pos <- gets position
  catchInvalidCharAt pos


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
