module Scarf.Gateway.Rule.Request
  ( Request (..),
    newRequest,
  )
where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.Wai qualified as Wai

-- | Wraps around a 'Wai.Request' to provide and cache additional info
-- potentially used by by many rules.
data Request = Request
  { -- | The underlying Wai.Request.
    requestWai :: Wai.Request,
    -- | It's annoying to recalculate this between flat file rules. Instead
    -- we cache it here when creating a Request.
    requestPath :: String,
    -- | This is to handle the case where query string are use in the request
    requestQueryString :: String
  }

newRequest :: Wai.Request -> Request
newRequest request =
  Request
    { requestWai = request,
      requestPath =
        -- Safe way to go from ByteString to String for UTF-8
        -- but stupidly indirect.
        Text.unpack $ Text.decodeUtf8 (Wai.rawPathInfo request),
      requestQueryString =
        Text.unpack $ Text.decodeUtf8 (Wai.rawQueryString request)
    }
