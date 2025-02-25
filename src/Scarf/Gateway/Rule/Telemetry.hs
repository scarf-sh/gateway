{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scarf.Gateway.Rule.Telemetry
  ( TelemetryRuleV1 (..),
    matchTelemetryRule,
  )
where

import Control.DeepSeq
import Control.Exception qualified
import Data.Aeson qualified
import Data.Aeson.Decoding qualified
import Data.Aeson.Decoding.ByteString.Lazy qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.ByteString.Char8 qualified
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified
import Data.ByteString.Lazy.Char8 qualified
import Data.Char qualified
import Data.Text (Text)
import Network.Wai qualified
import Scarf.Gateway.Rule.Request (Request (..))
import Scarf.Gateway.Rule.Response (ResponseBuilder (..))

data TelemetryRuleV1 = TelemetryRuleV1
  { -- | Package this rule belongs to, if any.
    rulePackage :: Maybe Text,
    -- | Incoming path, parsed into segments.
    incomingPath :: [Text],
    -- | Hash function to hash the Bearer token in the Authorization
    -- header. Hashing the token on the incoming requests helps us
    -- avoid storing a plain text representation of the access tokens.
    toAccessTokenHash :: ByteString -> ByteString,
    -- | List of access tokens that are authorized to push events to
    -- this telemetry endpoint. The tokens are _not_ stored in plain
    -- text. Instead, they are stored as hash value. See 'toAccessTokenHash'.
    --
    -- Nothing <=> unrestricted access
    -- Just [] <=> no access at all
    -- Just xs <=> access only for the specified tokens
    accessTokens :: Maybe [ByteString],
    -- | Max. size of the request body.
    maxRequestBody :: Int,
    -- | Max. number of events per requests
    maxEvents :: Int
  }

instance Show TelemetryRuleV1 where
  show TelemetryRuleV1 {} =
    "TelemetryRuleV1 {..}"

matchTelemetryRule ::
  TelemetryRuleV1 ->
  Request ->
  ResponseBuilder response ->
  IO (Maybe response)
matchTelemetryRule TelemetryRuleV1 {..} Request {..} ResponseBuilder {..}
  | "POST" <-
      Network.Wai.requestMethod requestWai,
    Network.Wai.pathInfo requestWai == incomingPath =
      withAuthentication $ do
        requestBody <-
          Network.Wai.lazyRequestBody requestWai
        -- Force all the events so that we do not leak the request body.
        telemetryEvents <-
          Control.Exception.evaluate $
            force $
              parseTelemetryEvents requestBody
        pure $
          Just (telemetryResponse rulePackage telemetryEvents)
  | otherwise =
      pure Nothing
  where
    --
    withAuthentication action =
      case accessTokens of
        Nothing ->
          -- Nothing <=> unrestricted access
          action
        Just accessTokens
          | Just authorizationHeader <-
              lookup "Authorization" (Network.Wai.requestHeaders requestWai),
            Just bearerToken <-
              Data.ByteString.stripPrefix "Bearer " authorizationHeader,
            let accessTokenHash =
                  toAccessTokenHash (Data.ByteString.Char8.strip bearerToken),
            accessTokenHash `elem` accessTokens ->
              action
          | otherwise ->
              pure (Just unauthorized)

    --
    parseTelemetryEvents :: LazyByteString -> [Data.Aeson.Object]
    parseTelemetryEvents requestBody =
      take maxEvents $
        parseEvents $
          Data.ByteString.Lazy.take (fromIntegral maxRequestBody) requestBody

    --
    parseEvents :: LazyByteString -> [Data.Aeson.Object]
    parseEvents bytes'
      | Data.ByteString.Lazy.null bytes =
          []
      | otherwise =
          case Data.Aeson.Decoding.toEitherValue
            (Data.Aeson.Decoding.ByteString.Lazy.lbsToTokens bytes) of
            Left _error ->
              []
            Right (value, rest) ->
              case value of
                Data.Aeson.Object object ->
                  object : parseEvents rest
                _ ->
                  parseEvents rest
      where
        bytes =
          Data.ByteString.Lazy.Char8.dropWhile Data.Char.isSpace bytes'
