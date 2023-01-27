module RevisionInfo (RevisionInfo (..), get) where

import Control.Exception (try)
import Control.Monad (when)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Time (LocalTime, ZonedTime, getZonedTime)
import GHC.Exception (SomeException)
import Shared.Util (filterMaybe, rightToMaybe)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..), readProcessWithExitCode)
import Text.Regex.PCRE ((=~))

-- | Information about the current git revirion.
data RevisionInfo = RevisionInfo
  { inRepository :: Bool
  , tag :: Maybe String
  , revisionHash :: String
  , tagDistance :: Int
  , dirty :: Bool
  , currentTime :: ZonedTime
  }
  deriving (Show)

-- | The default values for revisionInfo, given the current time.
defaultRevisionInfo :: ZonedTime -> RevisionInfo
defaultRevisionInfo now =
  RevisionInfo
    { inRepository = False
    , tag = Nothing
    , revisionHash = ""
    , tagDistance = 0
    , dirty = False
    , currentTime = now
    }

-- Retrieves the current revision info.
get :: Maybe String -> IO RevisionInfo
get tagFilter =
  let
    -- Get data from the describe command.
    getDescribe :: String -> String -> (Maybe String, Maybe Int, Bool)
    getDescribe commitHash describeOutput =
      let
        describeRegex :: String = "\\A(.+?)(-(\\d+)-(\\w+))?(-dirty)?\\Z"
        (_, _, _, [tag, _, dist, hash, dirty]) =
          describeOutput =~ describeRegex :: (String, String, String, [String])
        tagFound = not $ tag == take 7 commitHash
       in
        ( if tagFound then Just tag else Nothing
        , read <$> filterMaybe (not . null) dist
        , dirty == "-dirty"
        )
   in
    do
      revParse <- execute "git" ["rev-parse", "HEAD"]
      now <- getZonedTime
      case revParse of
        Right (commitHash : _) -> do
          let ri = (defaultRevisionInfo now){inRepository = True, revisionHash = commitHash}
          -- We expect describe to succeed, since the previous command also succeeded.
          let match = fromMaybe "*" tagFilter
          describeOutput <- head <$> execute_ "git" ["describe", "--always", "--dirty", "--match", match]
          pure $ case getDescribe commitHash describeOutput of
            (Nothing, _, dirty) -> ri{dirty = dirty}
            (Just tag, dist, dirty) -> ri{tag = Just tag, tagDistance = fromMaybe 0 dist, dirty = dirty}

        -- If this fails we're not in a git repository, return the default revision info.
        _ -> pure $ defaultRevisionInfo now

-- | Like execute, except assumes the result is succesful and fails otherwise.
execute_ :: String -> [String] -> IO [String]
execute_ cmd args = do
  res <- execute cmd args
  case res of
    Left stdErr -> errorWithoutStackTrace $ unlines ["Did not expect command to fail.", "Received error:", stdErr]
    Right stdOut -> pure stdOut

{- | Executes a command. If the process returns a success exit code, the StdOut output is returned as Right.
 | If it returns a failure exit code, the StdErr output is returned as Left.
 | Throws an exception if git could not be found (or the command resulted in another unexpected exception).
-}
execute :: String -> [String] -> IO (Either String [String])
execute cmd args = do
  cmdResult <- try @SomeException $ readProcessWithExitCode cmd args ""
  case cmdResult of
    Left ex ->
      errorWithoutStackTrace $
        unlines
          [ "Unable to find git executable. Are you sure it is installed and in the path?"
          , "Received stdError:"
          , show ex
          ]
    Right (exitCode, stdOut, stdErr) ->
      case exitCode of
        ExitSuccess -> pure $ Right $ lines stdOut
        ExitFailure _ -> pure $ Left stdErr
