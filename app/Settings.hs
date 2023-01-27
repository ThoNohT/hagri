module Settings (Settings (..), HelpType (..)) where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Char (toLower)
import Data.List (elemIndex, find)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member)
import Shared.CmdArgs (SettingsArgs (..))
import Shared.Util (maybeToEither, (!?))

-- The type of help to show.
data HelpType = Main | Topic String deriving (Eq, Show)

-- | Settings to be used.
data Settings = Settings
  { inFile :: Maybe String
  , outFile :: Maybe String
  , inText :: Maybe String
  , tagFilter :: Maybe String
  , showHelp :: Maybe HelpType
  }
  deriving (Eq, Show)

-- | The available options.
data Setting = InFile | OutFile | InText | Filter | ShowHelp deriving (Eq, Show, Ord)

instance SettingsArgs Setting Settings where
  allSettings =
    [ ("i", "in-file", InFile)
    , ("o", "out-file", OutFile)
    , ("v", "value", InText)
    , ("f", "tag-filter", Filter)
    , ("h", "help", ShowHelp)
    ]

  valueIsOptional ShowHelp = True
  valueIsOptional _ = False

  defaultSettings = Settings Nothing Nothing Nothing Nothing Nothing

  applySetting ShowHelp Nothing settings = settings{showHelp = Just Main}
  applySetting ShowHelp (Just topic) settings = settings{showHelp = Just $ Topic topic}
  applySetting InFile (Just val) settings = settings{inFile = Just $ val}
  applySetting OutFile (Just val) settings = settings{outFile = Just $ val}
  applySetting InText (Just val) settings = settings{inText = Just $ val}
  applySetting Filter (Just val) settings = settings{tagFilter = Just $ val}
  applySetting _ _ settings = settings

  validateSettings sets@Settings{inFile, inText}
    | isJust inFile && isJust inText =
        Just "--in-file and --value cannot both be specified."
    | otherwise = Nothing
