{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Scarf.Gateway.Rule.Capture
  ( RequestId (..),
    newRequestId,
    RuleCapture (..),
    CapturedRequest,
    captureRequest,
  )
where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Types (Status)
import Network.Wai qualified as Wai

-- | Interesting information about a rule match.
data RuleCapture
  = -- | Flatfiles allow capturing arbitrary variables from
    -- the public url.
    FlatfileCapture
      { -- | Absolute URL we are redirecting to
        fileAbsoluteUrl :: Maybe ByteString,
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
newtype RequestId = RequestId UUID

newRequestId :: IO RequestId
newRequestId = RequestId <$> UUID.nextRandom

-- | All the relevant information that we emit to our queues for
-- further processing. Careful when introducing breaking changes,
-- downstream consumers will need adjustments as well.
--
-- This type is backward-compatible with the Types.DockerLog in
-- scarf-server.
--
-- TODO Switch to e.g. thrift encoding using Schema
newtype CapturedRequest = CapturedRequest (Maybe RuleCapture)
  deriving newtype (Show)

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
captureRequest _time _requestId _request _responseStatus mcapture =
  CapturedRequest $ mcapture
