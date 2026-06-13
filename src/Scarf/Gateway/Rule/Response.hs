module Scarf.Gateway.Rule.Response
  ( DockerProxyRewrite (..),
    ProxyTarget (..),
    ResponseBuilder (..),
  )
where

import Data.Aeson qualified
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.Wai qualified as Wai
import Scarf.Gateway.Rule.Capture (RuleCapture)

data DockerProxyRewrite = DockerProxyRewrite
  { dockerProxyPublicRepository :: !(Maybe ByteString),
    dockerProxyBackendRepository :: !(Maybe ByteString)
  }

data ProxyTarget = ProxyTarget
  { proxyTargetDomain :: !ByteString,
    proxyTargetPath :: !ByteString,
    proxyTargetQueryString :: !ByteString,
    proxyTargetDockerRewrite :: !(Maybe DockerProxyRewrite)
  }

-- | Allows building an abstract response from the rule matchers. Keeping the
-- response abstract helps keeps us honest and eases testing.
data ResponseBuilder response = ResponseBuilder
  { telemetryResponse ::
      Maybe Text ->
      -- \^ Package id
      [Data.Aeson.Object] ->
      response,
    -- | Returns a 404 not found
    notFound ::
      RuleCapture ->
      response,
    unauthorized ::
      response,
    -- | Absolute url, e.g. https://registry.docker.com/v2/library/hello-world/..
    redirectTo ::
      [RuleCapture] ->
      ByteString ->
      response,
    -- | Proxy the request to the given domain. Allowing to capture details from
    -- the upstream response.
    proxyTo ::
      (Wai.Response -> RuleCapture) ->
      ProxyTarget ->
      response
  }
