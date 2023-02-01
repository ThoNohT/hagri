module Shared.CmdArgs (SettingsArgs (..), get) where

import Control.Monad (foldM)
import Data.Bifunctor (Bifunctor)
import Data.Char (toLower)
import Data.List (elem, find)
import Shared.Util (maybeToEither)

-----
-- The type class needed for implementing settings filled by command line arguments.
-----

-- | A class for types for command line arguments, that can be used to make settings.
class Ord s => SettingsArgs s ss | s -> ss, ss -> s where
  -- | The list of valid parameters. First string is the short notation (prefixed by '-'), the second is the long
  -- | notation (prefixed by '--'). The bool indicates whether the value after it is optional.
  allSettings :: [(String, String, s)]

  -- | Indicates whether a value is optional for this setting.
  valueIsOptional :: s -> Bool

  -- | The default settings to use when no parameters are provided.
  defaultSettings :: ss

  -- | Apply a single setting with a value to the settings.
  -- | This function is never called with Nothing for the value for a setting that does not have an optional
  -- | value. Therefore that case can simply be handled by not changing the settings.
  applySetting :: s -> Maybe String -> ss -> ss

  -- | Provides error messages based on a finished settings object.
  -- | If Some is returned, the settings creation will fail.
  validateSettings :: ss -> Maybe String

-----
-- The state machine.
-----

-- | The states the settings processor can be in.
data SpState s = LookingForArg | ProcessingArg (s, String)

-- | Contains settings, and the set of settings that are already applied.
data Sets s ss = Sets ss [s]

-- | Functor instance for Sets, so we can easily update the settings.
instance Functor (Sets s) where
  fmap f (Sets sets applied) = Sets (f sets) applied

-----
-- Implementation of getting settings.
-----

-- | Gets the settings from the provided command line arguments, given an implementation of the SettingsArgs typeclass.
get :: forall s ss. SettingsArgs s ss => [String] -> Either String ss
get args = do
  processedArgs <- foldM nextParam (Sets defaultSettings [], LookingForArg) args
  Sets finished _ <- finishProcessing processedArgs
  case validateSettings finished of
    Nothing -> pure finished
    Just err -> Left err
 where
  finishProcessing :: forall s ss. SettingsArgs s ss => (Sets s ss, SpState s) -> Either String (Sets s ss)
  finishProcessing = \case
    (sets, ProcessingArg (set, arg)) | valueIsOptional set -> markSet arg set $ applySetting set Nothing <$> sets
    (_, ProcessingArg (set, arg)) -> Left $ "Command-line arguments ended while processing argument '" ++ arg ++ "'."
    (sets, LookingForArg) -> pure sets

  nextParam :: (Sets s ss, SpState s) -> String -> Either String (Sets s ss, SpState s)
  nextParam (sets, LookingForArg) param = (sets,) . ProcessingArg <$> getSetting param
  nextParam (sets, ProcessingArg (set, arg)) param =
    if isParamName param && valueIsOptional set
      then -- If the parameter was optional and the parameter looks like a parameter name, fill the value as empty,
      -- and use the value for the next step in the state machine.
        applySetting' arg set Nothing sets >>= \sets -> nextParam (sets, LookingForArg) param
      else -- If the value is not optional, or it does look like a value, use whatever we got as value.
        (,LookingForArg) <$> applySetting' arg set (Just param) sets
   where
    -- Performs the applySet from the SettingsArgs class but also marks is at set, so it cannot be set multiple times.
    applySetting' :: String -> s -> Maybe String -> Sets s ss -> Either String (Sets s ss)
    applySetting' arg set value sets = markSet arg set $ applySetting set value <$> sets

-- If something starts with one (or two) dashes, it is considered the name of a parameter.
isParamName :: String -> Bool
isParamName ('-' : _) = True
isParamName _ = False

{- | Attempts to get a setting based on a parameter, by looking up its short or long name, depending on the prefix.
 | Also returns the name that was passed while getting this setting.
-}
getSetting :: forall s ss. SettingsArgs s ss => String -> Either String (s, String)
getSetting ('-' : '-' : name) =
  let error = "Invalid parameter long name: '" ++ name ++ "'."
   in maybeToEither error $ (\(_, _, s) -> (s, "--" ++ name)) <$> find (\(_, ln, _) -> ln == (toLower <$> name)) allSettings
getSetting ('-' : name) =
  let error = "Invalid parameter short name: '" ++ name ++ "'."
   in maybeToEither error $ (\(_, _, s) -> (s, "-" ++ name)) <$> find (\(sn, _, _) -> sn == (toLower <$> name)) allSettings
getSetting arg = Left $ "Expected a parameter name, but got '" ++ arg ++ "'."

-- | Marks a setting as set. Fails if it was already set.
markSet :: SettingsArgs s ss => String -> s -> Sets s ss -> Either String (Sets s ss)
markSet arg set (Sets sets applied)
  | set `elem` applied = Left $ "Setting '" ++ arg ++ "' is set more than once."
  | otherwise = pure $ Sets sets (set : applied)
