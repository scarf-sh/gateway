{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scarf.Gateway.Rule.Capture
  ( RequestId (..),
    newRequestIdGen,
    RuleCapture (..),
    CapturedRequest,
    captureRequest,
    encodeCapturedRequestToJSON,
  )
where

import Data.Aeson (Value, toEncoding)
import Data.Aeson.Encoding
  ( Encoding,
    dict,
    fromEncoding,
    int,
    list,
    null_,
    pair,
    pairs,
    text,
    unsafeToEncoding,
    utcTime,
  )
import Data.Atomics.Counter (incrCounter, newCounter)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
  ( Builder,
    char7,
    word64Dec,
    word64HexFixed,
  )
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IP (fromHostAddress, fromHostAddress6)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX
  ( utcTimeToPOSIXSeconds,
  )
import Data.Word (Word64)
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Header (hReferer)
import Network.Socket (SockAddr (..))
import Network.Wai qualified as Wai
import OpenTracing (SpanContext)
import OpenTracing.Jaeger.CollectorReporter (jaegerPropagation)
import OpenTracing.Propagation (TextMap, inject)
import System.Random (newStdGen, randoms)

-- | Interesting information about a rule match.
data RuleCapture
  = -- | Flatfiles allow capturing arbitrary variables from
    -- the public url.
    FlatfileCapture
      { -- | Absolute URL we are redirecting to
        fileAbsoluteUrl :: ByteString,
        -- | Extracted variables and their values
        fileVariables :: HashMap Text Text,
        -- | The package the rule belongs to
        filePackage :: Text
      }
  | -- | Docker rules capture the image and reference.
    DockerCapture
      { dockerCaptureImage :: ![Text],
        dockerCaptureReference :: !Text,
        dockerCaptureBackendRegistry :: !Text,
        -- | Package that this image belongs to, if not an
        -- auto-creation-rule.
        dockerCapturePackage :: !(Maybe Text),
        -- | Rule that was matched to cause auto-creation, if any.
        dockerCaptureAutoCreate :: !(Maybe Text)
      }
  | -- | Pixel-id
    PixelCapture
      !Text
  | -- | Python rules capture the package and version
    PythonCapture
      { -- | Scarf package id
        pythonCapturePackage :: !(Maybe Text),
        -- | Package Version - Nothing in case it is not a package download.
        pythonCaptureVersion :: !(Maybe Text),
        pythonCaptureFileName :: !(Maybe Text),
        pythonCaptureFileBackendURL :: !(Maybe Text)
      }
  | ScarfJsCapture
      { scarfJsRequestBody :: !Value,
        scarfJsPackage :: !(Maybe Text)
      }
  deriving (Eq, Show)

-- | Uniquely identifies a Request. We are trying to be as unique as
-- it can get without global counters. It doesn't matter too much in the
-- scheme of things if there are ids twice occurring twice.
data RequestId = RequestId !Word64 !Word64 !Word64

newRequestIdGen :: IO (IO RequestId)
newRequestIdGen = do
  rng <- newStdGen
  let (a, b) = case randoms rng of
        a : b : _ -> (a, b)
        _ -> error "impossible"
  counter <- newCounter 0
  return $ do
    x <- incrCounter 1 counter
    pure $! RequestId a b (fromIntegral x)

-- | All the relevant information that we emit to our queues for
-- further processing. Careful when introducing breaking changes,
-- downstream consumers will need adjustments as well.
--
-- This type is backward-compatible with the Types.DockerLog in
-- scarf-server.
--
-- TODO Switch to e.g. thrift encoding using Schema
data CapturedRequest = CapturedRequest
  { capturedRequestId :: RequestId,
    -- | POSIX timstamp of when the request happened.
    capturedRequestTime :: UTCTime,
    -- | Useragent of the request.
    capturedRequestUserAgent :: Text,
    -- | Referer of the request.
    capturedRequestReferer :: Text,
    -- | Concatenation of the raw header lines.
    capturedRequestRawHeaders :: Text,
    -- | Request specific info
    capturedRequest :: Request,
    -- | Response status
    capturedResponseStatus :: !Int,
    -- | Interesting info about a match
    capturedRequestRuleCapture :: Maybe RuleCapture
  }

-- | HTTP Request specific info that got captured.
data Request = Request
  { requestContentType :: Text,
    requestHost :: Text,
    requestMethod :: Text,
    requestUri :: Text,
    requestQueryString :: Text,
    requestRemoteAddress :: Text,
    requestRemotePort :: Text
  }

-- | Captures all the relevant information for further processing.
--
-- This function is total so that every request, whatever it is,
-- can be captured.
captureRequest ::
  -- | Time the request arrived.
  UTCTime ->
  -- | A process-unique identifier for the request. Ideally it is
  -- also globally unique but no care is taken to ensure that.
  RequestId ->
  -- | Request we are capturing info from.
  Wai.Request ->
  -- | Response status
  Status ->
  -- | Relevant information about a match (if any). Nothing means
  -- that no rule matched. We still allow this case to be able to
  -- gather info from ALL the requests vs. only those that matched
  -- to gain further info into the request behavior.
  Maybe RuleCapture ->
  CapturedRequest
captureRequest time requestId request responseStatus mcapture =
  CapturedRequest
    { capturedRequestId =
        requestId,
      capturedRequestTime =
        time,
      capturedRequestReferer =
        maybe "" decodeUtf8 (lookup hReferer (Wai.requestHeaders request)),
      capturedRequestUserAgent =
        maybe "" decodeUtf8 (Wai.requestHeaderUserAgent request),
      capturedRequestRawHeaders =
        decodeUtf8 $
          mconcat $
            intersperse
              "\r\n"
              [ original header <> ": " <> value
                | (header, value) <- Wai.requestHeaders request
              ],
      capturedRequest =
        Request
          { requestContentType =
              "",
            requestHost =
              maybe "" decodeUtf8 (Wai.requestHeaderHost request),
            requestMethod =
              decodeUtf8 (Wai.requestMethod request),
            requestUri =
              decodeUtf8 (Wai.rawPathInfo request),
            requestQueryString =
              decodeUtf8 (Wai.rawQueryString request),
            requestRemoteAddress = case Wai.remoteHost request of
              SockAddrInet _ address -> Text.pack $ show (fromHostAddress address)
              SockAddrInet6 _ _ address _ -> Text.pack $ show (fromHostAddress6 address)
              SockAddrUnix {} -> "",
            requestRemotePort = case Wai.remoteHost request of
              SockAddrInet port _ -> Text.pack (show port)
              SockAddrInet6 port _ _ _ -> Text.pack (show port)
              SockAddrUnix {} -> ""
          },
      capturedResponseStatus = fromEnum responseStatus,
      capturedRequestRuleCapture = mcapture
    }

-- | Encode a `CapturedRequest` into a form that is backward compatible with
-- the current state of affairs.
encodeCapturedRequestToJSON ::
  -- | Information about the trace that captured the request
  Maybe SpanContext ->
  -- | An indicator of the region this request was captured. e.g. us-west-2
  Text ->
  CapturedRequest ->
  Builder
encodeCapturedRequestToJSON spanContext origin CapturedRequest {capturedRequest = Request {..}, ..} =
  let RequestId a b c = capturedRequestId
   in fromEncoding $
        pairs
          ( pair "!v" (text "1")
              <> pair "origin" (text origin)
              <> pair "time" (utcTime capturedRequestTime)
              <> pair "unixTime" (int (round (utcTimeToPOSIXSeconds capturedRequestTime)))
              <> pair
                "request_id"
                ( unsafeToEncoding
                    ( char7 '"'
                        <> word64HexFixed a
                        <> word64HexFixed b
                        <> char7 '-'
                        <> word64Dec c
                        <> char7 '"'
                    )
                )
              <> pair "request_time" (int 0) --  At this point we don't have
              -- the overall request time, stubbing out
              -- for now.
              <> pair
                "request"
                ( pairs
                    ( pair "method" (text requestMethod)
                        <> pair "host" (text requestHost)
                        <> pair "uri" (text requestUri)
                        <> pair "contentType" (text requestContentType)
                        <> pair "queryString" (text requestQueryString)
                        <> pair "remoteAddress" (text requestRemoteAddress)
                        <> pair "remotePort" (text requestRemotePort)
                    )
                )
              <> pair "responseStatus" (int capturedResponseStatus)
              <> pair "referer" (text capturedRequestReferer)
              <> pair "userAgent" (text capturedRequestUserAgent)
              <> pair "rawHeaders" (text capturedRequestRawHeaders)
              <> pair "extract" (maybe null_ encodeRuleCaptureToJSON capturedRequestRuleCapture)
              <> pair "trace" (maybe null_ encodeSpanContextToJSON spanContext)
              <> pair "partition-key" (text partitionKey)
          )
  where
    -- In order to distribute the logs across all Kafka partitions evenly
    -- we not only rely on the package type and name in the partition key
    -- but also include the event date.
    partitionKey = case capturedRequestRuleCapture of
      Just DockerCapture {dockerCaptureImage} ->
        "docker-" <> date <> "-" <> Text.intercalate "/" dockerCaptureImage
      Just FlatfileCapture {filePackage} ->
        "file-" <> date <> "-" <> filePackage
      Just (PixelCapture pixelId) ->
        "pixel-" <> date <> "-" <> pixelId
      Just PythonCapture {pythonCapturePackage} ->
        "python-" <> date <> "-" <> fromMaybe "" pythonCapturePackage
      Just ScarfJsCapture {scarfJsPackage} ->
        "scarfjs-" <> date <> "-" <> fromMaybe "" scarfJsPackage
      Nothing ->
        "-" <> date

    date =
      Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d-%H" capturedRequestTime

encodeSpanContextToJSON :: SpanContext -> Encoding
encodeSpanContextToJSON context =
  dict
    text
    text
    HashMap.foldrWithKey
    (inject jaegerPropagation context :: TextMap)

encodeRuleCaptureToJSON :: RuleCapture -> Encoding
encodeRuleCaptureToJSON ruleCapture = case ruleCapture of
  FlatfileCapture {filePackage, fileVariables} ->
    pairs
      ( pair "type" (text "flatfile")
          <> pair "variables" (dict text text HashMap.foldrWithKey fileVariables)
          <> pair "package" (text filePackage)
      )
  DockerCapture {..} ->
    pairs
      ( pair "type" (text "docker")
          <> pair "image" (list text dockerCaptureImage)
          <> pair "reference" (text dockerCaptureReference)
          <> pair "backend-registry" (text dockerCaptureBackendRegistry)
          <> pair "package" (maybe null_ text dockerCapturePackage)
          <> pair "auto-create" (maybe null_ text dockerCaptureAutoCreate)
      )
  PixelCapture pixelId ->
    pairs
      ( pair "type" (text "pixel")
          <> pair "pixel-id" (text pixelId)
      )
  PythonCapture {..} ->
    pairs
      ( pair "type" (text "python")
          <> pair "version" (maybe null_ text pythonCaptureVersion)
          <> pair "package" (maybe null_ text pythonCapturePackage)
          <> pair "file" (maybe null_ text pythonCaptureFileName)
          <> pair "backend-url" (maybe null_ text pythonCaptureFileBackendURL)
      )
  ScarfJsCapture {..} ->
    pairs
      ( pair "type" (text "scarfjs")
          <> pair "request-body" (toEncoding scarfJsRequestBody)
          <> pair "package" (maybe null_ text scarfJsPackage)
      )
