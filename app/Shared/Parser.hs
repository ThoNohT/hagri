module Shared.Parser (
  -- Parser definition.
  Parser (..),
  -- Combinators.
  check,
  peek,
  choice,
  -- String parsers.
  pString,
  pSubString,
  pChar,
  escaped,
  char,
  end,
) where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (void)
import Data.Char (isAlphaNum, toLower)
import System.IO.Unsafe (unsafePerformIO)

-----
-- Parser definition.
-----

-- | A parser.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \i -> Just (a, i)
  liftA2 f (Parser a) (Parser b) = Parser $ \i ->
    case a i of
      Nothing -> Nothing
      Just (a', i') ->
        case b i' of
          Nothing -> Nothing
          Just (b', i'') -> Just (f a' b', i'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \i -> a i <|> b i

instance Monad Parser where
  (Parser a) >>= pf = Parser $ \i ->
    case a i of
      Nothing -> Nothing
      Just (a', i') -> (runParser $ pf a') i'

-----
-- Combinators.
-----

-- | Takes a parser, and adds a check to the result, failing if the check fails.
check :: (a -> Bool) -> Parser a -> Parser a
check check (Parser p) = Parser $ \i -> case p i of
  Just (a, i') | check a -> Just (a, i')
  _ -> Nothing

-- | Takes a parser, and changes it such that it does not consume any input.
peek :: Parser a -> Parser a
peek (Parser p) = Parser $ \i -> case p i of
  Nothing -> Nothing
  Just (a, _) -> Just (a, i)

-- | Returns the result of the first parser that succeeds. Or fails if all parsers fail.
choice :: [Parser a] -> Parser a
choice [] = empty
choice (x : xs) = x <|> choice xs

-----
-- String parsers.
-----

-- | Parses a specific string.
pString :: String -> Parser String
pString = mapM pChar

-- | Parses a substring of the provided string. succeeds even if no character matched.
pSubString :: String -> Parser String
pSubString [] = pure []
pSubString (x : xs) = liftA2 (:) (pChar x) (pSubString xs) <|> pure []

-- | Parses an escaped character.
escaped :: Parser Char
escaped = pChar '\\' *> char

-- | Parsers a specific char.
pChar :: Char -> Parser Char
pChar c = check (== c) char

-- | A parser consumes and returns the next character. Fails if there is no input left.
char :: Parser Char
char = Parser $ \case
  x : xs -> Just (x, xs)
  [] -> Nothing

-- | A parser that succeeds at the end of the input.
end :: Parser ()
end = Parser $ \case
  x : xs -> Nothing
  [] -> Just ((), "")
