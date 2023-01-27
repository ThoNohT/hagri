module Shared.Util (
  (<<$>>),
  takeLast,
  filterMaybe,
  rightToMaybe,
  maybeToEither,
  (!?),
  rightOrThrow,
  justOrThrow,
  rightOrThrowEx,
) where

import Control.Exception (SomeException)
import Data.Fixed (div')

infixl 4 <<$>>

(<<$>>) :: forall f g a b. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

-- | Takes the last n elements from a list.
takeLast :: forall a. Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

-- | Wraps a value in Maybe, Just if the check returns True, Nothing otherwise.
filterMaybe :: forall a. (a -> Bool) -> a -> Maybe a
filterMaybe f x
  | f x = Just x
  | otherwise = Nothing

-- | Converts an Either to a Maybe by taking the Right value or Nothing.
rightToMaybe :: forall a b. Either a b -> Maybe b
rightToMaybe (Right x) = Just x
rightToMaybe (Left _) = Nothing

-- | Converts a Maybe to an Either, with the provided value as Left in case of Nothing.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing = Left a

-- | Safe version of !! for List.
(!?) :: [a] -> Int -> Maybe a
ls !? i | i < 0 = Nothing
ls !? i | i >= length ls = Nothing
ls !? i = Just $ ls !! i

-- | Extracts an Either to its value, by throwing an exception if it is Left.
rightOrThrow :: String -> Either String a -> IO a
rightOrThrow msg (Left err) = errorWithoutStackTrace $ msg ++ ": " ++ err
rightOrThrow _ (Right x) = pure x

-- | Extracts a Maybe to its value, by throwing an exception if it is Nothing.
justOrThrow :: String -> Maybe a -> IO a
justOrThrow msg Nothing = errorWithoutStackTrace msg
justOrThrow _ (Just x) = pure x

-- | Extracts an Either to its value, by throwing an exception if it is Left.
rightOrThrowEx :: String -> Either SomeException a -> IO a
rightOrThrowEx msg (Left err) = errorWithoutStackTrace msg
rightOrThrowEx _ (Right x) = pure x
