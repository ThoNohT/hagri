module Document (
  Document (..),
  PlaceholderType (..),
  DocumentElement (..),
  ParamElem (..),
  Parameter (..),
  parser,
  compile,
) where

import Control.Applicative (empty, many, (<|>))
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import RevisionInfo (RevisionInfo (..))
import Shared.Parser (Parser, char, check, choice, end, escaped, pChar, pString, peek)
import Shared.Util ((<<$>>))
import Text.Read (readMaybe)
import TimeFormat qualified

-- | A full document being processed.
newtype Document = Document [DocumentElement]

-- | The available types of placeholders.
data PlaceholderType = Tag | Distance | Hash | Dirty | Date

-- | A document element can either be simple text, or a placeholder that can be replaced in.
data DocumentElement
  = Text Char
  | Placeholder {placeholderType :: PlaceholderType, parameters :: [Parameter]}

-- | Elements in the value of a parameter.
data ParamElem = Wildcard | Escaped Char | Verbatim Char

-- | A parameter in a placholder.
data Parameter = Parameter {paramName :: String, paramValue :: [ParamElem]}

-- | Parses a document.
parser :: Parser Document
parser = Document <$> (many (placeholder <|> Text <$> char) <* end)
 where
  -- Parses a placeholder.
  placeholder :: Parser DocumentElement
  placeholder = do
    void $ pString "[hagri:" -- Start of placeholder
    phType <- placeholderType
    params <- many param
    void $ pChar ']' -- End of placeholder
    pure Placeholder{placeholderType = phType, parameters = params}
   where
    placeholderTypeFromName :: String -> Parser PlaceholderType
    placeholderTypeFromName "tag" = pure Tag
    placeholderTypeFromName "dist" = pure Distance
    placeholderTypeFromName "hash" = pure Hash
    placeholderTypeFromName "dirty" = pure Dirty
    placeholderTypeFromName "date" = pure Date
    placeholderTypeFromName _ = empty

    placeholderType :: Parser PlaceholderType
    placeholderType = do
      phName <- choice $ pString <$> ["tag", "dist", "hash", "dirty", "date"]
      placeholderTypeFromName phName

  -- Parses a parameter.
  param :: Parser Parameter
  param = do
    void $ pChar ':' -- Start of param
    name <- paramName
    void $ pChar '=' -- Name and value separator
    value <- many $ choice [Wildcard <$ wildcard, Escaped <$> escaped, Verbatim <$> nonTerminatingChar]
    void $ peek (pChar ']' <|> pChar ':') -- Param terminator
    pure Parameter{paramName = name, paramValue = value}
   where
    nonTerminatingChar :: Parser Char
    nonTerminatingChar = check (\c -> c /= ':' && c /= ']') char

    paramName :: Parser String
    paramName = do
      first <- alpha
      rest <- many alphaNum
      pure $ first : rest

    alphaNum :: Parser Char
    alphaNum = check isAlphaNum char

    alpha :: Parser Char
    alpha = check isAlpha char

    wildcard :: Parser ParamElem
    wildcard = Wildcard <$ pChar '*'

-- | The possible ways in which wildcards can be replaced.
data ReplaceAction = Replace String | NoReplace

-- | Compiles a document, using the provided revision information.
compile :: RevisionInfo -> Document -> String
compile ri (Document elems) = concatMap (compileDocumentElement ri) elems
 where
  -- Compiles a single document element.
  compileDocumentElement :: RevisionInfo -> DocumentElement -> String
  compileDocumentElement _ (Text c) = [c]
  compileDocumentElement ri (Placeholder{placeholderType, parameters}) =
    case placeholderType of
      Tag ->
        let tag' = calculateSubstring parameters <$> tag ri
         in case (inRepository ri, tag', tagDistance ri == 0) of
              (False, _, _) ->
                -- If not in a repository, use the 'nv' value, otherwise the 'nt' value, otherwise "NOT_VERSIONED"
                let nt = getParamValue parameters "nt" NoReplace "NOT_VERSIONED"
                 in getParamValue parameters "nv" NoReplace nt
              (True, Nothing, _) ->
                -- If not on a tag, use the 'nt' value, otherwise "".
                getParamValue parameters "nt" NoReplace ""
              (True, Just tag'', False) ->
                -- If after a tag, use the 'at' value, otherwise the plain tag.
                getParamValue parameters "at" (Replace tag'') tag''
              (True, Just tag'', True) ->
                -- If on a tag, use the 'ot' value, otherwise the 'at' value, otherwise the plain tag.
                let at = getParamValue parameters "at" (Replace tag'') tag''
                 in getParamValue parameters "ot" (Replace tag'') at
      Distance ->
        let dist = show (tagDistance ri)
         in case (inRepository ri, tag ri, tagDistance ri == 0) of
              (False, _, _) ->
                -- If not in a repository, use the 'nv' value, otherwise the 'nt' value, otherwise "NOT_VERSIONED"
                let nt = getParamValue parameters "nt" NoReplace "NOT_VERSIONED"
                 in getParamValue parameters "nv" NoReplace nt
              (True, Nothing, _) ->
                -- If not on a tag, use the 'nt' value, otherwise "".
                getParamValue parameters "nt" NoReplace ""
              (True, Just _, False) ->
                -- If after a tag, use the 'at' value, otherwise the plain distance.
                getParamValue parameters "at" (Replace dist) dist
              (True, Just _, True) ->
                -- If on a tag, use the 'ot' value, otherwise the 'at' value, otherwise the plain distance.
                let at = getParamValue parameters "at" (Replace dist) dist
                 in getParamValue parameters "ot" (Replace dist) at
      Hash ->
        let hash = calculateSubstring parameters (revisionHash ri)
            nt = getParamValue parameters "nt" NoReplace hash
            at = getParamValue parameters "at" (Replace hash) hash
            ot = getParamValue parameters "ot" (Replace hash) at
         in case (inRepository ri, tag ri, tagDistance ri == 0) of
              (False, _, _) ->
                -- If not in a repository, use the 'nv' value, otherwise "NOT_VERSIONED"
                getParamValue parameters "nv" NoReplace "NOT_VERSIONED"
              (True, Nothing, _) ->
                -- If not on a tag, use the 'nt' value, otherwise the hash itself
                nt
              (True, Just _, False) ->
                -- If after a tag, use the 'at' value, otherwise the hash itself.
                at
              (True, Just _, True) ->
                -- If on a tag, use the 'ot' value, otherwise the 'at' value, otherwise the 'nt' value,
                -- otherwise the plain distance.
                ot
      Dirty ->
        let no = getParamValue parameters "no" NoReplace "clean"
         in case (inRepository ri, dirty ri) of
              (False, _) ->
                -- If not in a repository, use the 'nv' value, otherwise the 'no' value, otherwise "clean".
                getParamValue parameters "nv" NoReplace no
              (_, False) ->
                -- If clean, use the 'no' value, otherwise "clean".
                no
              _ ->
                -- If dirty, use the 'yes' value, otherwise "dirty"
                getParamValue parameters "yes" NoReplace "dirty"
      Date ->
        let
          fmt = getParamValue parameters "format" NoReplace "{yyyy}-{MM}-{dd}"
          compiled = TimeFormat.parse fmt
         in
          TimeFormat.compile (currentTime ri) compiled
   where
    -- Uses the 'start' and 'len' parameters to take a substring of any string.
    calculateSubstring :: [Parameter] -> String -> String
    calculateSubstring params input =
      let start = clamp (0, length input) $ readParamValue parameters "start" NoReplace 0
          input' = drop start input
          len = clamp (0, length input') $ readParamValue parameters "len" NoReplace (length input')
       in take len input'

  -- \| Gets a parameter value with a type that can be read, using the specified replacement action to converte ParamElems
  --   | to a String, and the specified default value if the parameter was not found.
  --
  readParamValue :: forall a. Read a => [Parameter] -> String -> ReplaceAction -> a -> a
  readParamValue params name repl def =
    let maybeValue = concat <$> (replace repl <<$>> tryGetParamValue params name)
     in fromMaybe def $ readMaybe =<< maybeValue

  -- Gets a String parameter value, using the specified replacement action to convert ParamElems to a String, and
  -- the specified default value if the parameter was not found.
  getParamValue :: [Parameter] -> String -> ReplaceAction -> String -> String
  getParamValue params name repl def =
    fromMaybe def $ concat <$> (replace repl <<$>> tryGetParamValue params name)

  -- Attempts to get the value for a parameter with a specific name in a list of parameters.
  tryGetParamValue :: [Parameter] -> String -> Maybe [ParamElem]
  tryGetParamValue params name =
    (\Parameter{paramValue} -> paramValue) <$> find (\(Parameter{paramName}) -> paramName == name) params

  -- Converts a ParamElem to a String by replacing a wildcard with the specified value, and leaving
  -- escaped and verbatim values the same.
  replace :: ReplaceAction -> ParamElem -> String
  replace NoReplace Wildcard = "*"
  replace (Replace replacement) Wildcard = replacement
  replace _ (Escaped c) = [c]
  replace _ (Verbatim c) = [c]
