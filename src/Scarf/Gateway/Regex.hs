module Scarf.Gateway.Regex
  ( Regex,
    fromText,
    toText,
    variables,
    match,
    match',
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (withText)
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Regex.PCRE.Light qualified as PCRE

-- | A regular expression.
data Regex = Regex
  { compiledRegex :: PCRE.Regex,
    displayRegex :: Text
  }

instance Show Regex where
  show regex = unpack (displayRegex regex)

instance Eq Regex where
  re1 == re2 =
    -- Might be imprecise but fine for now
    displayRegex re1 == displayRegex re2

instance Ord Regex where
  a `compare` b = displayRegex a `compare` displayRegex b

instance ToJSON Regex where
  toJSON = toJSON . toText

instance FromJSON Regex where
  parseJSON = withText "regex" $ \input ->
    case fromText input of
      Nothing ->
        fail "invalid regex"
      Just regex ->
        pure regex

-- | Parses a template from a 'Text' value.
fromText :: Text -> Maybe Regex
fromText input =
  case PCRE.compileM (encodeUtf8 stripped) [PCRE.utf8] of
    Left _error -> Nothing
    Right regex ->
      Just
        Regex
          { compiledRegex = regex,
            displayRegex = input
          }
  where
    stripped = strip input

toText :: Regex -> Text
toText = displayRegex

-- | Returns the names of the capture groups.
variables :: Regex -> [Text]
variables regex =
  [decodeUtf8 name | (name, _index) <- PCRE.captureNames (compiledRegex regex)]

-- | Match a Regex against an input 'String'. Returns the named capture groups and their match.
-- Returns @Nothing@ in case the regex didn't match.
match :: Regex -> String -> Maybe [(Text, Text)]
match regex input =
  case PCRE.match (compiledRegex regex) (encodeUtf8 (pack input)) [PCRE.exec_notempty] of
    -- The first element in the list is the whole, matched string. The capture groups start
    -- at the second element.
    Just (_matchedPart : xs) ->
      Just $
        catMaybes
          [ do
              value <- List.lookup index (zip [0 ..] xs)
              pure (decodeUtf8 name, decodeUtf8 value)
            | (name, index) <- PCRE.captureNames (compiledRegex regex)
          ]
    _ ->
      Nothing

match' :: Regex -> String -> Maybe [Text]
match' regex input =
  case PCRE.match (compiledRegex regex) (encodeUtf8 (pack input)) [PCRE.exec_notempty] of
    Just xs -> Just (map decodeUtf8 xs)
    Nothing -> Nothing
