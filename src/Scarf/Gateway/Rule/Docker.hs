{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scarf.Gateway.Rule.Docker
  ( DockerImageRule (..),
    DockerRuleV1 (..),
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
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types.URI (Query, parseQuery, renderQuery)
import Network.Wai qualified as Wai
import Scarf.Gateway.ImagePattern as ImagePattern
import Scarf.Gateway.Rule.Capture (RuleCapture (..))
import Scarf.Gateway.Rule.Request (Request (..))
import Scarf.Gateway.Rule.Response (DockerProxyRewrite (..), ProxyTarget (..), ResponseBuilder (..))

data DockerImageRule = DockerImageRule
  { dockerImageRulePackage :: !Text,
    dockerImageRuleBackendImage :: ![Text]
  }
  deriving (Eq, Show)

data DockerImageTarget = DockerImageTarget
  { dockerImageTargetBackendImage :: ![Text],
    dockerImageTargetPackage :: !(Maybe Text),
    dockerImageTargetAutoCreate :: !(Maybe Text)
  }

-- | Rule that matches a particular image and redirects (or proxies)
-- to a backend registry.
data DockerRuleV1 = DockerRuleV1
  { -- | Image path components matching this rule. e.g. ["library", "hello-world"] and their
    -- respective package ids and backend repositories.
    ruleImages :: !(HashMap [Text] DockerImageRule),
    -- | Domain of the registry to redirect (or proxy) to.
    ruleBackendRegistry :: !ByteString,
    -- | Flag indicating whether to always proxy and never redirect.
    ruleAlwaysProxy :: !Bool
  }
  deriving (Eq, Show)

data DockerRuleV2 = DockerRuleV2
  { -- | Images matching this pattern will be considered a rule
    -- match. Images matching a rule will be flagged for auto-creation.
    ruleImagePattern :: !ImagePattern.Pattern,
    -- | Id of the rule in the backend.
    ruleRuleId :: !Text,
    -- | Domain of the registry to redirect (or proxy) to.
    ruleBackendRegistry :: !ByteString,
    -- | Flag indicating whether to always proxy and never redirect.
    ruleAlwaysProxy :: !Bool
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
type DockerImageMatcher = [Text] -> Maybe DockerImageTarget

matchDockerRuleV1 :: (MonadMatch m) => DockerRuleV1 -> Request -> ResponseBuilder response -> m (Maybe response)
matchDockerRuleV1 DockerRuleV1 {ruleImages, ruleBackendRegistry, ruleAlwaysProxy} =
  matchDocker
    ( \image ->
        case HashMap.lookup image ruleImages of
          Just DockerImageRule {..} ->
            Just
              DockerImageTarget
                { dockerImageTargetBackendImage = dockerImageRuleBackendImage,
                  dockerImageTargetPackage = Just dockerImageRulePackage,
                  dockerImageTargetAutoCreate = Nothing
                }
          Nothing -> Nothing
    )
    ruleBackendRegistry
    ruleAlwaysProxy

matchDockerRuleV2 :: (MonadMatch m) => DockerRuleV2 -> Request -> ResponseBuilder response -> m (Maybe response)
matchDockerRuleV2 DockerRuleV2 {ruleImagePattern, ruleRuleId, ruleBackendRegistry, ruleAlwaysProxy} =
  matchDocker
    ( \image ->
        if ImagePattern.match ruleImagePattern image
          then
            Just
              DockerImageTarget
                { dockerImageTargetBackendImage = image,
                  dockerImageTargetPackage = Nothing,
                  dockerImageTargetAutoCreate = Just ruleRuleId
                }
          else Nothing
    )
    ruleBackendRegistry
    ruleAlwaysProxy

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
  -- | Flag indicating whether to always proxy and never redirect.
  Bool ->
  Request ->
  ResponseBuilder response ->
  m (Maybe response)
matchDocker matchImage backendRegistry alwaysProxy Request {requestWai = request} responseBuilder@ResponseBuilder {..}
  | "/v2/_catalog" <- Wai.rawPathInfo request =
      pure $ Just (notFound emptyCapture)
  | "/_scarf/docker-token" <- Wai.rawPathInfo request,
    Just publicImage <- tokenRequestImage request,
    Just DockerImageTarget {..} <- matchImage publicImage,
    Just tokenTarget <- dockerTokenProxyTarget backendRegistry publicImage dockerImageTargetBackendImage request =
      pure $ Just $ proxyTo (const emptyCapture) tokenTarget
  | "/_scarf/docker-token" <- Wai.rawPathInfo request,
    Nothing <- tokenRequestImage request,
    Just tokenTarget <- dockerTokenProxyTargetNoScope backendRegistry request =
      pure $ Just $ proxyTo (const emptyCapture) tokenTarget
  | "/v2/" <- Wai.rawPathInfo request =
      pure $
        Just $
          redirectOrProxy
            request
            backendRegistry
            alwaysProxy
            "/v2/"
            False
            (DockerProxyRewrite Nothing Nothing <$ dockerTokenEndpoint backendRegistry)
            emptyCapture
            responseBuilder
  | ("v2" : image, dockerRoute : reference : rest) <-
      break
        isDockerRoute
        (Wai.pathInfo request),
    Just DockerImageTarget {..} <- matchImage image =
      let !backendPath =
            dockerPath (["v2"] <> dockerImageTargetBackendImage <> [dockerRoute, reference] <> rest)
          !isRenamed =
            image /= dockerImageTargetBackendImage
          !dockerRewrite =
            if isRenamed
              then
                Just
                  DockerProxyRewrite
                    { dockerProxyPublicRepository = Just (dockerRepository image),
                      dockerProxyBackendRepository = Just (dockerRepository dockerImageTargetBackendImage)
                    }
              else Nothing
          !capture =
            DockerCapture
              { dockerCaptureImage = image,
                dockerCaptureBackendRegistry = Text.decodeUtf8 backendRegistry,
                dockerCaptureReference = reference,
                dockerCapturePackage = dockerImageTargetPackage,
                dockerCaptureAutoCreate = dockerImageTargetAutoCreate
              }
       in pure $ Just $ redirectOrProxy request backendRegistry alwaysProxy backendPath isRenamed dockerRewrite capture responseBuilder
  | otherwise =
      pure Nothing
  where
    isDockerRoute x =
      x == "manifests" || x == "blobs" || x == "tags" || x == "referrers"

    dockerPath segments =
      "/" <> ByteString.intercalate "/" (Text.encodeUtf8 <$> segments)

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
  -- | Flag indicating whether to always proxy and never redirect.
  Bool ->
  -- | Upstream path to request.
  ByteString ->
  -- | The public and backend repository names differ.
  Bool ->
  Maybe DockerProxyRewrite ->
  RuleCapture ->
  ResponseBuilder response ->
  response
redirectOrProxy request domain alwaysProxy targetPath forceProxy dockerRewrite !capture ResponseBuilder {..}
  | alwaysProxy || forceProxy =
      proxy
  -- when a basic authorization header is present, it will be proxied
  | Just authHeader <- lookup "Authorization" (Wai.requestHeaders request),
    "Basic" `ByteString.isPrefixOf` authHeader =
      proxy
  | shouldRedirectDockerRequest request =
      -- As with the Host header we have to respect the proxy protocol
      -- when redirecting.
      let !absoluteUrl = "https://" <> domain <> targetPath <> Wai.rawQueryString request
       in redirectTo [capture] absoluteUrl
  | otherwise =
      proxy
  where
    proxy =
      proxyTo
        (const capture)
        ProxyTarget
          { proxyTargetDomain = domain,
            proxyTargetPath = targetPath,
            proxyTargetQueryString = Wai.rawQueryString request,
            proxyTargetDockerRewrite =
              case dockerTokenEndpoint domain of
                Just _ -> dockerRewrite
                Nothing -> Nothing
          }

tokenRequestImage :: Wai.Request -> Maybe [Text]
tokenRequestImage request =
  case [image | ("scope", Just scope) <- parseQuery (Wai.rawQueryString request), Just image <- [scopeRepository scope]] of
    image : _ -> Just image
    [] -> Nothing

scopeRepository :: ByteString -> Maybe [Text]
scopeRepository scope = do
  rest <- ByteString.stripPrefix "repository:" scope
  let (repository, actions) = ByteString.break (== 58) rest
  if ByteString.null repository || ByteString.null actions
    then Nothing
    else Just (T.splitOn "/" (Text.decodeUtf8 repository))

dockerTokenProxyTarget :: ByteString -> [Text] -> [Text] -> Wai.Request -> Maybe ProxyTarget
dockerTokenProxyTarget backendRegistry publicImage backendImage request = do
  (tokenDomain, tokenPath) <- dockerTokenEndpoint backendRegistry
  pure
    ProxyTarget
      { proxyTargetDomain = tokenDomain,
        proxyTargetPath = tokenPath,
        proxyTargetQueryString =
          renderQuery True $
            rewriteScopeQuery
              (dockerRepository publicImage)
              (dockerRepository backendImage)
              (parseQuery (Wai.rawQueryString request)),
        proxyTargetDockerRewrite = Nothing
      }

dockerTokenProxyTargetNoScope :: ByteString -> Wai.Request -> Maybe ProxyTarget
dockerTokenProxyTargetNoScope backendRegistry request = do
  (tokenDomain, tokenPath) <- dockerTokenEndpoint backendRegistry
  pure
    ProxyTarget
      { proxyTargetDomain = tokenDomain,
        proxyTargetPath = tokenPath,
        proxyTargetQueryString = Wai.rawQueryString request,
        proxyTargetDockerRewrite = Nothing
      }

rewriteScopeQuery :: ByteString -> ByteString -> Query -> Query
rewriteScopeQuery publicRepository backendRepository =
  fmap rewrite
  where
    rewrite ("scope", Just scope) =
      ("scope", Just (rewriteRepositoryScope publicRepository backendRepository scope))
    rewrite queryParam =
      queryParam

rewriteRepositoryScope :: ByteString -> ByteString -> ByteString -> ByteString
rewriteRepositoryScope publicRepository backendRepository scope =
  let publicPrefix = "repository:" <> publicRepository <> ":"
      backendPrefix = "repository:" <> backendRepository <> ":"
   in case ByteString.stripPrefix publicPrefix scope of
        Just suffix -> backendPrefix <> suffix
        Nothing -> scope

dockerRepository :: [Text] -> ByteString
dockerRepository =
  ByteString.intercalate "/" . fmap Text.encodeUtf8

dockerTokenEndpoint :: ByteString -> Maybe (ByteString, ByteString)
dockerTokenEndpoint registry
  | registry == "docker.io" || registry == "registry-1.docker.io" =
      Just ("auth.docker.io", "/token")
  | registry == "ghcr.io" =
      Just ("ghcr.io", "/token")
  | registry == "quay.io" =
      Just ("quay.io", "/v2/auth")
  | registry == "cgr.dev" =
      Just ("cgr.dev", "/token")
  | registry == "gcr.io" || ".gcr.io" `ByteString.isSuffixOf` registry =
      Just (registry, "/v2/token")
  | ".pkg.dev" `ByteString.isSuffixOf` registry =
      Just (registry, "/v2/token")
  | otherwise =
      Nothing

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
