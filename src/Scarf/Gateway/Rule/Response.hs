module Scarf.Gateway.Rule.Response
  ( ResponseBuilder (..),
  )
where

import Data.ByteString (ByteString)
import Network.Wai qualified as Wai
import Scarf.Gateway.Rule.Capture (RuleCapture)

-- | Allows building an abstract response from the rule matchers. Keeping the
-- response abstract helps keeps us honest and eases testing.
data ResponseBuilder response = ResponseBuilder
  { -- | Returns a 404 not found
    notFound ::
      RuleCapture ->
      response,
    -- | Absolute url, e.g. https://registry.docker.com/v2/library/hello-world/..
    redirectTo ::
      RuleCapture ->
      ByteString ->
      response,
    -- | Proxy the request to the given domain. Allowing to capture details from
    -- the upstream response.
    proxyTo ::
      (Wai.Response -> RuleCapture) ->
      ByteString ->
      response
  }
