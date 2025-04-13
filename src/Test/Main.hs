module Main where

import Prelude hiding (getLine)
import System.Exit (exitFailure, exitSuccess)
import System.IO (openFile, IOMode(..), hClose, hFlush, stdout)
import System.Environment (getArgs)
import System.FilePath
import Data.Text.IO ( getLine, hGetContents)
import Data.Text (Text)
import Data.Text qualified as Text
import FTLex.Lexer.FTL.Lexer qualified as FTL
import FTLex.Lexer.FTLTEX.Lexer qualified as FTLTEX
import FTLex.Position
import FTLex.Message


main :: IO ()
main = do
  args <- getArgs
  fileArg <- case args of
    [file] -> pure file
    _ -> failWith $ "\nInvalid number of arguments.\n" <> usageInfo
  inputH <- openFile fileArg ReadMode
  input <- hGetContents inputH
  hClose inputH
  debugOutput <- case splitExtensions fileArg of
    (_, ".ftl") -> show <$> FTL.runLexer initPos input
    (_, ".ftl.tex") -> show <$> FTLTEX.runLexer initPos input
    (_, ext) -> failWith $
      "\nInvalid file name extension \"" ++ ext ++ "\". " ++
      "Only \".ftl\" and \".ftl.tex\" are allowed.\n" ++
      usageInfo
  putStrLn $ "\n" <> debugOutput
  putStr "\nIs the above output correct? (y/n) "
  hFlush stdout
  answer <- getLine
  hFlush stdout
  case answer of
    "y" -> succeedWith "Test passed.\n"
    "n" -> failWith "Test failed.\n"
    _ -> failWith "Invalid answer.\nTest failed.\n"
  where
    initPos = SimplePosition 1 1

usageInfo :: String
usageInfo =
  "Usage: cabal test --test-options=\"<path to (.ftl|.ftl.tex|.tex) file>\"\n"

failWith :: String -> IO a
failWith msg = putStrLn msg >> hFlush stdout >> exitFailure

succeedWith :: String -> IO a
succeedWith msg = putStrLn msg >> hFlush stdout >> exitSuccess


-- * Message

instance Msg SimplePosition IO where
  errorLexer :: SimplePosition -> Text -> IO a
  errorLexer pos msg = do
    putStrLn $ "Lexing error " ++ show pos ++ ": " ++ Text.unpack msg
    putStrLn "\nIs the above error intended? (y/n) "
    answer <- getLine
    case answer of
      "y" -> exitSuccess
      "n" -> exitFailure
      _ -> failWith "Invalid answer.\n"
