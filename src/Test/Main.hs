module Main where

import Prelude hiding (putStrLn, getLine)
import System.Exit (exitFailure, exitSuccess)
import System.IO (openFile, IOMode(..), hClose, hFlush, stdout)
import System.Environment (getArgs)
import System.FilePath
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
  fileArg <- case args of
    [file] -> pure file
    _ -> failWith $ "\nInvalid number of arguments.\n" <> usageInfo
  inputH <- openFile fileArg ReadMode
  input <- hGetContents inputH
  hClose inputH
  debugOutput <- case splitExtensions fileArg of
    (_, ".ftl") -> showFtlLexemes <$> FTL.runLexer initPos input (FTL.initState initPos initUnicodeBlocks)
    (_, ".ftl.tex") -> showTexLexemes <$> TEX.runLexer initPos input (TEX.initState initPos initUnicodeBlocks)
    (_, ext) -> failWith $ "\nInvalid file name extension \"" <> Text.pack ext <> "\". Only \".ftl\" or \".ftl.tex\" are allowed.\n" <> usageInfo
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

initUnicodeBlocks :: Set UnicodeBlock
initUnicodeBlocks = Set.fromList [Latin1Supplement]

usageInfo :: Text
usageInfo =
  "Usage: cabal test --test-options=\"<path to ForTheL file> (FTL|TEX)\"\n"

failWith :: Text -> IO a
failWith msg = putStrLn msg >> hFlush stdout >> exitFailure

succeedWith :: Text -> IO a
succeedWith msg = putStrLn msg>> hFlush stdout >> exitSuccess


-- * Message

instance Msg SimplePosition IO where
  errorLexer :: SimplePosition -> Text -> IO a
  errorLexer pos msg = do
    putStrLn $ "Lexing error " <> showPos pos <> ": " <> msg
    putStrLn "\nIs the above error intended? (y/n) "
    answer <- getLine
    case answer of
      "y" -> exitSuccess
      "n" -> exitFailure
      _ -> failWith "Invalid answer.\n"
