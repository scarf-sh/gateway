{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Scarf.Gateway
  ( -- * Rules
    Rule,
    newDockerRuleV1,
    newDockerRuleV2,
    newFlatfileRule,
    newPixelRule,
    newPythonRule,
    newScarfJsRule,

    -- * Captured info about a request
    RuleCapture (..),

    -- * Running the Gateway
    GatewayConfig (..),
    proxyTo,
    gateway,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad.Fix (fix)
import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Builder.Extra (byteStringInsert, lazyByteStringInsert)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
  ( Status,
    found302,
    hLocation,
    methodNotAllowed405,
    notFound404,
    notModified304,
    ok200,
    unprocessableEntity422,
  )
import Network.Wai
  ( Application,
    Request (..),
    requestHeaderHost,
    requestHeaders,
    responseBuilder,
    responseLBS,
    responseStatus,
  )
import Network.Wai qualified as Wai
import OpenTracing.Tracer (traced_)
import Scarf.Gateway.Rule
  ( ResponseHeaders (..),
    Rule,
    RuleCapture (..),
    newDockerRuleV1,
    newDockerRuleV2,
    newFlatfileRule,
    newPixelRule,
    newPythonRule,
    newScarfJsRule,
  )
import Scarf.Gateway.Rule qualified as Rule
import Scarf.Gateway.Rule.Response qualified
import Scarf.Lib.Tracing
  ( ActiveSpan,
    Tag,
    TagVal (BoolT, StringT),
    Tracer,
    addTag,
    childOf,
    spanOpts,
    traceWaiApplication,
  )

-- | Basic configuration necessary to run a Gateway.
data GatewayConfig = GatewayConfig
  { -- | A modifier that's applied to a domain right before the
    -- Gateway starts proxying through it. Scarfs usecase is to
    -- replace certain domains with domains for our internal caches.
    -- Also returns whether to use a secure connection for proxying.
    gatewayModifyProxyDomain :: ByteString -> (ByteString, Bool),
    -- | Lookup function to get the available rules for a domain.
    -- This takes a ByteString to avoid unecessary conversions from
    -- Request values to Text.
    gatewayDomainRules :: ActiveSpan -> ByteString -> IO [Rule],
    -- | How to report a request.
    gatewayReportRequest :: ActiveSpan -> Request -> Status -> Maybe RuleCapture -> IO (),
    -- | An 'Application' that proxies requests to the given host.
    gatewayProxyTo ::
      ActiveSpan ->
      -- http/https?
      Bool ->
      -- Host proxy to e.g. registry-1.docker.io
      ByteString ->
      Application
  }

pattern Match :: Tag
pattern Match = ("rule.match", BoolT True)

pattern NoMatch :: Tag
pattern NoMatch = ("rule.match", BoolT False)

-- | Main entry-point to the Scarf Gateway 'Application'.
gateway ::
  Tracer ->
  GatewayConfig ->
  Application
gateway tracer GatewayConfig {..} = do
  traceWaiApplication tracer $ \span -> \request respond -> do
    -- Here comes the meat: We got a request, now figure out whether this is a
    -- Docker or Flatfile request and do the appropriate redirect (or not).
    let host :: ByteString
        host =
          fromMaybe "" $
            lookup "X-Forwarded-Host" (requestHeaders request)
              <|> requestHeaderHost request

    addTag span ("host", StringT (Text.decodeUtf8 host))

    rules <- gatewayDomainRules span host
    match <- traced_ tracer (spanOpts "match-rules" (childOf span)) $ \span -> do
      result <-
        Rule.runMatch $
          Rule.match
            rules
            request
            ( Scarf.Gateway.Rule.Response.ResponseBuilder
                { redirectTo = \capture absoluteUrl -> \_tracer span request respond ->
                    redirectTo tracer span absoluteUrl request respond
                      `finally` gatewayReportRequest span request found302 (Just capture),
                  proxyTo = \newCapture domain -> \_tracer span request respond ->
                    let (targetDomain, shouldUseTLS) = gatewayModifyProxyDomain domain
                     in gatewayProxyTo
                          span
                          shouldUseTLS
                          targetDomain
                          request
                          $ \response -> do
                            let !status = responseStatus response
                                !capture = newCapture response
                            respond response
                              `finally` gatewayReportRequest span request status (Just capture),
                  bytes = \capture headers bytes -> \tracer span request respond ->
                    respondBytes tracer span headers bytes request respond
                      `finally` gatewayReportRequest span request ok200 (Just capture),
                  notFound = \capture -> \_tracer span request respond ->
                    notFound request respond
                      `finally` gatewayReportRequest span request notFound404 (Just capture),
                  notModified = \capture etag -> \tracer span request respond ->
                    notModified tracer span etag request respond
                      `finally` gatewayReportRequest span request notModified304 (Just capture),
                  methodNotAllowed = \tracer span request respond ->
                    methodNotAllowed tracer span request respond
                      `finally` gatewayReportRequest span request methodNotAllowed405 Nothing,
                  invalidRequest = \tracer span request respond ->
                    invalidRequest tracer span request respond
                      `finally` gatewayReportRequest span request unprocessableEntity422 Nothing
                }
            )

      addTag span $ case result of
        Nothing -> NoMatch
        Just {} -> Match
      pure result

    case match of
      Just (_rule, respondApp) ->
        respondApp tracer span request respond
      Nothing ->
        notFound request respond
          `finally` gatewayReportRequest span request notFound404 Nothing

-- Sensible default for cases in which we don't know about a request.
-- TODO: Nice to have, separate error responses for unknown host and
-- unknown image.
notFound :: Application
notFound = \request respond ->
  case pathInfo request of
    "v2" : "_catalog" : _ ->
      respond (responseLBS notFound404 [] mempty)
    -- Looks like a Docker request, return an error message that
    -- Docker-Clients can parse.
    "v2" : _ ->
      respond $
        responseLBS notFound404 [("Content-Type", "application/json")] $
          encode $
            object
              [ "errors"
                  .= [ object
                         [ "code" .= ("NAME_UNKNOWN" :: Text),
                           "message" .= ("repository name not known to registry" :: Text),
                           "detail" .= object []
                         ]
                     ]
              ]
    _ ->
      respond (responseLBS notFound404 [] mempty)

notModified ::
  Tracer ->
  ActiveSpan ->
  -- | Etag
  ByteString ->
  Application
notModified tracer span etag = \_request respond ->
  traced_ tracer (spanOpts "not-modified" (childOf span)) $ \span -> do
    addTag span ("etag", StringT (Text.decodeUtf8 etag))
    respond (responseBuilder notModified304 [("etag", "\"" <> etag <> "\"")] mempty)

methodNotAllowed ::
  Tracer ->
  ActiveSpan ->
  Application
methodNotAllowed tracer span = \_request respond ->
  traced_ tracer (spanOpts "method-not-allowed" (childOf span)) $ \_span -> do
    respond (responseBuilder methodNotAllowed405 [] mempty)

invalidRequest ::
  Tracer ->
  ActiveSpan ->
  Application
invalidRequest tracer span = \_request respond ->
  traced_ tracer (spanOpts "invalid-request" (childOf span)) $ \_span -> do
    respond (responseBuilder unprocessableEntity422 [] mempty)

-- Best case: we can send a redirect back to the client.
-- TODO better types, redirectTo expects an absolute URL
redirectTo ::
  Tracer ->
  ActiveSpan ->
  ByteString ->
  Application
redirectTo tracer span absoluteUrl = \_ respond ->
  traced_ tracer (spanOpts "redirect-to" (childOf span)) $ \span -> do
    addTag span ("redirect-to", StringT (Text.decodeUtf8 absoluteUrl))
    respond (responseLBS found302 [(hLocation, absoluteUrl)] mempty)

respondBytes ::
  Tracer ->
  ActiveSpan ->
  -- | Headers
  ResponseHeaders ->
  LBS.ByteString ->
  Application
respondBytes tracer span ResponseHeaders {..} bytes = \_ respond ->
  traced_ tracer (spanOpts "respond-bytes" (childOf span)) $ \_ -> do
    let headers =
          [("Etag", "\"" <> e <> "\"") | Just e <- [etag]]
            <> [("Cache-control", c) | Just c <- [cacheControl]]
            <> [("Content-Length", BS.pack (show c)) | Just c <- [contentLength]]
            <> [("Content-Type", contentType)]
    respond $ responseBuilder ok200 headers (lazyByteStringInsert bytes)

-- Not so great case: We have to proxy through the request. We are deferring
-- the nitty-gritties to the http-reverse-proxy library. We probably put nginx
-- in front of the gateway and use X-Accel-Redirect header.
-- TODO better types, proxtTO expects a domain name
proxyTo ::
  Tracer ->
  Manager ->
  ActiveSpan ->
  -- | Secure?
  Bool ->
  ByteString ->
  Application
proxyTo tracer manager span shouldUseTLS domain = \request respond ->
  traced_ tracer (spanOpts "proxy-to" (childOf span)) $ \span -> do
    addTag span ("proxy-method", StringT (Text.decodeUtf8 (requestMethod request)))
    addTag span ("proxy-path", StringT (Text.decodeUtf8 (rawPathInfo request)))
    addTag span ("proxy-query", StringT (Text.decodeUtf8 (rawQueryString request)))
    addTag span ("proxy-domain", StringT (Text.decodeUtf8 domain))
    addTag span ("proxy-secure", BoolT shouldUseTLS)
    let (host, port)
          | [host_, port_] <- BS.split ':' domain,
            Just (port, "") <- BS.readInt port_ =
              (host_, port)
          | otherwise =
              (domain, if shouldUseTLS then 443 else 80)

        proxyRequest =
          HC.defaultRequest
            { HC.checkResponse = \_ _ -> pure (),
              HC.responseTimeout = HC.responseTimeoutMicro 10000000, -- 10 seconds
              HC.method = Wai.requestMethod request,
              HC.secure = shouldUseTLS,
              HC.host = host,
              HC.port = port,
              HC.path = Wai.rawPathInfo request,
              HC.queryString = Wai.rawQueryString request,
              HC.requestHeaders = fixupRequestHeaders request,
              HC.requestBody =
                -- We don't proxy request bodies. Read-only access throuhgout.
                mempty,
              HC.redirectCount = 0
            }

    HC.withResponse proxyRequest manager $ \response -> do
      respond $
        Wai.responseStream
          (HC.responseStatus response)
          (fixupResponseHeaders (HC.responseHeaders response))
          ( \emit flush -> do
              let reader = HC.responseBody response
              fix $ \loop -> do
                bytes <- HC.brRead reader
                if BS.null bytes
                  then pure ()
                  else do
                    emit (byteStringInsert bytes)
                    flush
                    loop
          )
  where
    fixupResponseHeaders headers =
      [ (name, sanitizedValue)
        | (name, value) <- headers,
          case name of
            "Transfer-Encoding" -> False
            "Accept-Encoding" -> False
            "Content-Encoding" -> False
            _ -> True,
          let sanitizedValue =
                case name of
                  -- In case upstream returns a redirect where the Location header contains
                  -- a relative url, we have to make it an absolute one, otherwise the client
                  -- will try to follow that redirect at the Scarf Gateway where it really had
                  -- to follow it upstream.
                  "Location"
                    | "/" `BS.isPrefixOf` (BS.strip value) ->
                        (if shouldUseTLS then "https://" else "http://")
                          <> domain
                          <> (BS.strip value)
                  _ ->
                    value
      ]

    fixupRequestHeaders request =
      ( case lookup "X-Forwarded-For" (Wai.requestHeaders request) of
          Just forwardedFor
            | host : _ <- map BS.strip (BS.split ',' forwardedFor) ->
                [("X-Forwarded-For", host)]
          _ -> []
      )
        ++ [ header
             | header@(name, value) <- Wai.requestHeaders request,
               case name of
                 -- http-client will add the Host header automatically.
                 -- Need to remove this one otherwise we have Host included
                 -- twice.
                 "Host" -> False
                 "Connection" -> value == "close"
                 -- I am not sure about the following headers. I copied
                 -- most of them from wai-reverse-proxy
                 "Transfer-Encoding" -> False
                 "Accept-Encoding" -> False
                 "Content-Encoding" -> False
                 "Content-Length" -> False
                 _ -> True
           ]
