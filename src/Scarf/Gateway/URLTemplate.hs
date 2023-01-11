{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scarf.Gateway.URLTemplate
  ( URLTemplate,
    toText,
    fromText,
    ValidationError (..),
    validate,
    variables,
    expand,
    match,
  )
where

import Burrito qualified
import Burrito.Internal.Type.Value (Value (String))
import Control.Monad.Writer.Strict (execWriter, tell)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Foldable (foldl')
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyTextWith)
import Network.URI (URI (..), parseURI)

-- | A type representing an URL Template.
data URLTemplate = URLTemplate
  { -- Textual representation, this is what the user typed
    -- and what is shown to the user.
    urlTemplateText :: !Text,
    -- This is the internal representation of a URLTemplate. It might
    -- differ slightly as Burrito might apply optimizations that help
    -- do things quicker.
    urlTemplate :: Burrito.Template
  }
  deriving (Eq, Show)

instance Ord URLTemplate where
  a `compare` b = templateParts b `compare` templateParts a
    where
      templateParts :: URLTemplate -> [Text]
      templateParts urlTemplate = List.filter (/= "") . Text.split (== '/') $ url
        where
          url :: Text
          url = fromJust $ expand (const $ Just "") urlTemplate

instance FromJSON URLTemplate where
  parseJSON = withText "URLTemplate" $ \s ->
    case fromText s of
      Nothing -> fail "invalid url template"
      Just template -> pure template

instance ToJSON URLTemplate where
  toJSON = toJSON . urlTemplateText

-- | Parses a template from a Text value.
fromText :: Text -> Maybe URLTemplate
fromText input = do
  template <- Burrito.parse (Text.unpack input)
  pure
    URLTemplate
      { urlTemplateText = input,
        urlTemplate = template
      }

toText :: URLTemplate -> Text
toText = urlTemplateText

-- | Error as a result of validating the URLTemplate.
data ValidationError
  = InvalidTemplate
  | InvalidURL
  | InvalidScheme Text
  deriving (Show)

-- | Checks whether the template will expand to a proper URL.
validate :: URLTemplate -> Either ValidationError URLTemplate
validate input =
  let expandsProperly :: URLTemplate -> Either ValidationError URLTemplate
      expandsProperly template
        | Just _ <- expand Just template =
            Right template
        | otherwise =
            Left InvalidTemplate

      urlCheck :: URLTemplate -> Either ValidationError URLTemplate
      urlCheck template
        | Just url <- expand Just template,
          Just URI {uriScheme} <- parseURI (Text.unpack url) =
            if uriScheme `elem` ["http:", "https:"]
              then Right template
              else Left (InvalidScheme (Text.pack uriScheme))
        | otherwise =
            Left InvalidURL
   in foldl' (>>=) (Right input) [expandsProperly, urlCheck]

-- | Extracts the variables contained in a template.
variables :: URLTemplate -> [Text]
variables URLTemplate {urlTemplate} =
  let variable x = do
        tell [x]
        pure Nothing
   in execWriter (Burrito.expandWith variable urlTemplate)

-- | Extract the variables from a rendered url.
match :: URLTemplate -> String -> [(Text, Text)]
match URLTemplate {urlTemplate} requestPath =
  case Burrito.match requestPath urlTemplate of
    xs : _ ->
      -- TODO take into account Map values as well
      -- and flatten them.
      [(Text.pack k, v) | (k, String v) <- xs]
    [] ->
      []

expand :: (Text -> Maybe Text) -> URLTemplate -> Maybe Text
expand lookupVariable URLTemplate {urlTemplate} =
  let expander var =
        case lookupVariable var of
          Nothing -> Nothing
          Just value -> Just (Just (String value))
   in fmap
        (toStrict . toLazyTextWith 256)
        (Burrito.expandWith expander urlTemplate)
