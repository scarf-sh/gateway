{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scarf.Gateway.Rule.Docker
  ( DockerRuleV1 (..),
    DockerRuleV2 (..),
    matchDockerRuleV1,
    matchDockerRuleV2,
    shouldAlwaysProxy,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.Wai qualified as Wai
import Scarf.Gateway.ImagePattern as ImagePattern
import Scarf.Gateway.Rule.Capture (RuleCapture (..))
import Scarf.Gateway.Rule.Request (Request (..))
import Scarf.Gateway.Rule.Response (ResponseBuilder (..))

-- | Rule that matches a particular image and redirects (or proxies)
-- to a backend registry.
data DockerRuleV1 = DockerRuleV1
  { -- | Image path components matching this rule. e.g. ["library", "hello-world"] and their
    -- respective package ids.
    ruleImages :: !(HashMap [Text] Text),
    -- | Domain of the registry to redirect (or proxy) to.
    ruleBackendRegistry :: !ByteString
  }
  deriving (Eq, Show)

data DockerRuleV2 = DockerRuleV2
  { -- | Images matching this pattern will be considered a rule
    -- match. Images matching a rule will be flagged for auto-creation.
    ruleImagePattern :: !ImagePattern.Pattern,
    -- | Id of the rule in the backend.
    ruleRuleId :: !Text,
    -- | Domain of the registry to redirect (or proxy) to.
    ruleBackendRegistry :: !ByteString
  }
  deriving (Eq, Show)

type Domain = Text

-- | Returns true iff it's fine to always proxy the requests. This is especially important
-- with docker.io as they might rate limit us.
shouldAlwaysProxy :: Domain -> Bool
shouldAlwaysProxy domain =
  -- Always proxy for everybody except DockerHub
  domain
    `notElem` [ "docker.io",
                "registry-1.docker.io"
              ]

-- | Given an image name, determine whether the image is a match.
-- Returns Either PackageId AutoCreationRuleId.
type DockerImageMatcher = [Text] -> Maybe (Either Text Text)

matchDockerRuleV1 :: (MonadMatch m) => DockerRuleV1 -> Request -> ResponseBuilder response -> m (Maybe response)
matchDockerRuleV1 DockerRuleV1 {ruleImages, ruleBackendRegistry} =
  matchDocker
    ( \image ->
        case HashMap.lookup image ruleImages of
          Just packageId -> Just (Left packageId)
          Nothing -> Nothing
    )
    ruleBackendRegistry

matchDockerRuleV2 :: (MonadMatch m) => DockerRuleV2 -> Request -> ResponseBuilder response -> m (Maybe response)
matchDockerRuleV2 DockerRuleV2 {ruleImagePattern, ruleRuleId, ruleBackendRegistry} =
  matchDocker
    ( \image ->
        if ImagePattern.match ruleImagePattern image
          then Just (Right ruleRuleId)
          else Nothing
    )
    ruleBackendRegistry

type MonadMatch m = m ~ IO

-- | Checks whether a 'Request' is a Docker request and determines
-- if the client supports redirects and fallbacks to proxying if not.
--
-- This is the Gateways core functionality, be careful when changing
-- this function.
--
-- See https://docs.docker.com/registry/spec/api/ for reference.
matchDocker ::
  (MonadMatch m) =>
  DockerImageMatcher ->
  -- | Backend registry
  ByteString ->
  Request ->
  ResponseBuilder response ->
  m (Maybe response)
matchDocker matchImage backendRegistry Request {requestWai = request} responseBuilder@ResponseBuilder {..}
  | "/v2/_catalog" <- Wai.rawPathInfo request =
      pure $ Just (notFound emptyCapture)
  | "/v2/" <- Wai.rawPathInfo request =
      pure $ Just $ redirectOrProxy request backendRegistry emptyCapture responseBuilder
  | ("v2" : image, _manifestsOrBlobsOrTags : reference : _) <-
      break
        (\x -> x == "manifests" || x == "blobs" || x == "tags")
        (Wai.pathInfo request),
    Just result <- matchImage image =
      let !capture =
            DockerCapture
              { dockerCaptureImage = image,
                dockerCaptureBackendRegistry = Text.decodeUtf8 backendRegistry,
                dockerCaptureReference = reference,
                dockerCapturePackage = either Just (const Nothing) result,
                dockerCaptureAutoCreate = either (const Nothing) Just result
              }
       in pure $ Just $ redirectOrProxy request backendRegistry capture responseBuilder
  | otherwise =
      pure Nothing
  where
    emptyCapture =
      DockerCapture
        { dockerCaptureImage = [],
          dockerCaptureReference = "",
          dockerCaptureBackendRegistry = Text.decodeUtf8 backendRegistry,
          -- No package identified
          dockerCapturePackage = Nothing,
          -- Nothing to auto-create anyway
          dockerCaptureAutoCreate = Nothing
        }

redirectOrProxy ::
  Wai.Request ->
  ByteString ->
  RuleCapture ->
  ResponseBuilder response ->
  response
redirectOrProxy request domain !capture ResponseBuilder {..}
  | shouldRedirectDockerRequest request =
      -- As with the Host header we have to respect the proxy protocol
      -- when redirecting.
      let !absoluteUrl = "https://" <> domain <> Wai.rawPathInfo request
       in redirectTo capture absoluteUrl
  | otherwise =
      proxyTo (const capture) domain

shouldRedirectDockerRequest :: Wai.Request -> Bool
shouldRedirectDockerRequest request
  | Just userAgent <- Wai.requestHeaderUserAgent request =
      "docker/"
        `ByteString.isPrefixOf` userAgent
        || "Docker-Client"
        `ByteString.isInfixOf` userAgent
        || "containerd/1.4.9"
        `ByteString.isInfixOf` userAgent
        || "containerd/1.5.5"
        `ByteString.isInfixOf` userAgent
        || "containerd/1.6"
        `ByteString.isInfixOf` userAgent
        || "Watchtower (Docker)"
        == userAgent
  | otherwise =
      False
