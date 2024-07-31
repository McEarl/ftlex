module Main where

import Prelude hiding (putStrLn, getLine)
import System.Exit (exitFailure, exitSuccess)
import System.IO (openFile, IOMode(..), hClose, hFlush, stdout)
import System.Environment (getArgs)
import Data.Text.IO (putStrLn, getLine, hGetContents)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Set (Set)
import Data.Set qualified as Set
import FTLex.Ftl qualified as FTL
import FTLex.Tex qualified as TEX
import FTLex.Position
import FTLex.Message
import FTLex.Base
import FTLex.Debug


main :: IO ()
main = do
  args <- getArgs
  (fileArg, lexerArg, lineBreaksArg) <- case args of
    file : lexer : lineBreaks : _ -> pure (file, lexer, lineBreaks)
    _ -> failWith $ "\nInvalid number of arguments.\n" <> usageInfo
  lineBreaks <- case lineBreaksArg of
    "CR" -> pure CR
    "LF" -> pure LF
    "CRLF" -> pure CRLF
    _ -> failWith $ "\nUnknown line break type \"" <> Text.pack lineBreaksArg <> "\".\n" <> usageInfo
  inputH <- openFile fileArg ReadMode
  input <- hGetContents inputH
  hClose inputH
  debugOutput <- case lexerArg of
    "FTL" -> showFtlLexemes <$> FTL.runLexer initPos input (FTL.initState initPos initUnicodeBlocks) lineBreaks
    "TEX" -> showTexLexemes <$> TEX.runLexer initPos input (TEX.initState initPos initUnicodeBlocks) lineBreaks
    _ -> failWith $ "\nUnknown lexer \"" <> Text.pack lexerArg <> "\".\n" <> usageInfo
  putStrLn $ "\n" <> debugOutput
  putStr "\nIs the above output correct? (y/n) "
  hFlush stdout
  answer <- getLine
  hFlush stdout
  case answer of
    "y" -> succeedWith "Test passed.\n"
    "n" -> failWith "Test failed.\n"
    _ -> failWith "Invalid answer.\nTest failed.\n"

initUnicodeBlocks :: Set UnicodeBlock
initUnicodeBlocks = Set.fromList [Latin1Supplement]

usageInfo :: Text
usageInfo =
  "Usage: cabal test --test-options=\"<file> <lexer> <line break type>\"\n" <>
  "Where:\n" <>
  "  * <file> is a path to a ForTheL document\n" <>
  "  * <lexer> = \"FTL\" | \"TEX\"\n" <>
  "  * <line break type> = \"CR\" | \"LF\" | \"CRLF\""

failWith :: Text -> IO a
failWith msg = putStrLn msg >> hFlush stdout >> exitFailure

succeedWith :: Text -> IO a
succeedWith msg = putStrLn msg>> hFlush stdout >> exitSuccess


-- * Position

data Position = Position {
    pLine :: Int,
    pColumn :: Int
  }
  deriving (Eq, Ord)

initPos :: Position
initPos = Position 1 1

instance Pos Position where
  noPos :: Position
  noPos = Position 0 0

  getNextPos :: Text -> Position -> Position
  getNextPos text pos =
    let
      lineBreakNo = Text.count "\n" text
      newLine = pLine pos + fromIntegral lineBreakNo
      (_, lastCol) = Text.breakOnEnd "\n" text
      newCol = if lineBreakNo > 0
        then (fromIntegral . Text.length) lastCol + 1
        else pColumn pos + (fromIntegral . Text.length) lastCol
    in Position newLine newCol

  getPosOf :: Text -> Position -> Position
  getPosOf _ pos = pos

  showPos :: Position -> Text
  showPos (Position line col) =
    "(line " <> showNumber line <> ", " <> "column " <> showNumber col <> ")"
    where
      showNumber = Text.pack . show


-- * Message

instance Msg Position IO where
  errorLexer :: Position -> Text -> IO a
  errorLexer pos msg = do
    putStrLn $ "Lexing error " <> showPos pos <> ": " <> msg
    putStrLn "\nIs the above error intended? (y/n) "
    answer <- getLine
    case answer of
      "y" -> exitSuccess
      "n" -> exitFailure
      _ -> failWith "Invalid answer.\n"
