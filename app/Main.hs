module Main where

import Control.Exception (Exception (displayException), SomeException (SomeException), try)
import Data.Either (fromRight)
import Document qualified
import Help (displayHelp)
import RevisionInfo (RevisionInfo (RevisionInfo), get)
import Settings (HelpType (Main), Settings (..))
import Shared.CmdArgs qualified as CmdArgs (get)
import Shared.Parser (runParser)
import Shared.Util (justOrThrow, rightOrThrow, rightOrThrowEx)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Main entry point, performs exception handling for the actual code.
main :: IO ()
main = do
  res <- try main'
  case res of
    Right () -> pure ()
    Left (err :: SomeException) -> do
      hPutStrLn stderr $ "Error: " <> displayException err
      displayHelp Main
      exitFailure

-- | Actual application.
main' :: IO ()
main' = do
  -- Determine settings.
  args <- getArgs
  sets <- rightOrThrow "Error parsing commandline parameters" $ CmdArgs.get args

  case showHelp sets of
    Just help -> displayHelp help
    Nothing -> do
      -- Determine input text.
      input <- determineInText sets
      doc <- justOrThrow "Error parsing document." $ fst <$> runParser Document.parser input

      -- Get revision info.
      revInfo <- RevisionInfo.get (tagFilter sets)

      -- Compile output text.
      let outText = Document.compile revInfo doc

      -- Write out output text
      case outFile sets of
        Just fp -> writeFile' fp outText
        Nothing -> putStr outText

-- | Determines which text to work on, from a file, a parameter, or stdin, depending on the settings.
determineInText :: Settings -> IO String
determineInText sets =
  case (inFile sets, inText sets) of
    (Just fp, _) -> readFile' fp
    (Nothing, Just v) -> pure v
    (Nothing, Nothing) -> getContents

-- | Helper for reading a file with friendly error message.
readFile' :: FilePath -> IO String
readFile' fp = do
  res <- try $ readFile fp
  rightOrThrowEx ("Error reading from file '" ++ fp ++ "'.") res

-- | Helper for writing a file with friendly error message.
writeFile' :: FilePath -> String -> IO ()
writeFile' fp txt = do
  res <- try $ writeFile fp txt
  rightOrThrowEx ("Error writing to file '" ++ fp ++ "'.") res
