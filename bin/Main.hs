{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Main where

import Network.HTTP.Client.OpenSSL (withOpenSSL, defaultMakeContext,
  defaultOpenSSLSettings, opensslManagerSettings)
import System.IO (hSetBinaryMode, stdout, hSetBuffering,
  BufferMode (..), hFlush)
import Scarf.Lib.Tracing (withRootTracer)
import Network.HTTP.Client (Manager, ManagerSettings (..), newManager)
import OpenSSL.Session (contextSetDefaultVerifyPaths)
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (mask, bracket)
import Scarf.Gateway.Rule.Capture (newRequestIdGen)
import Scarf.Lib.Tracing (runTracer)
import Scarf.Lib.Tracing (traced_)
import Scarf.Lib.Tracing (spanOpts)
import Scarf.Gateway (GatewayConfig(..))
import Scarf.Lib.Tracing (Tracer)
import Scarf.Gateway.Rule.Capture (RequestId)
import Scarf.Lib.Tracing (ActiveSpan)
import Network.Wai (Request (..), responseLBS)
import Network.HTTP.Types (Status, ok200, notFound404)
import Scarf.Gateway (RuleCapture)
import Scarf.Gateway (RuleCapture(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Scarf.Lib.Tracing (childOf)
import Control.Monad.IO.Class (liftIO)
import Scarf.Lib.Tracing (TagVal(..))
import Scarf.Lib.Tracing (addTag)
import Data.Text.Encoding qualified as Text
import Scarf.Gateway (proxyTo)
import Control.Concurrent.Async (withAsync)
import Data.Foldable (for_)
import Data.Time (getCurrentTime)
import Scarf.Gateway.Rule.Capture (captureRequest)
import Scarf.Lib.Tracing (getSpanContext)
import Data.ByteString.Builder (hPutBuilder)
import Scarf.Gateway.Rule.Capture (encodeCapturedRequestToJSON)
import Network.Wai.Handler.Warp (run)
import Scarf.Gateway (gateway)

-- | Creates a new 'Manager' that uses OpenSSL to establish secure connections.
newOpenSSLManager ::
  -- | max. open connections per domain
  Int ->
  IO Manager
newOpenSSLManager maxOpenConn = do
  let makeContext = do
        context <- defaultMakeContext defaultOpenSSLSettings
        -- Respect SSL_CERT_FILE and SSL_CERT_DIR
        contextSetDefaultVerifyPaths context
        pure context

      managerSettings =
        (opensslManagerSettings makeContext)
          { managerConnCount = maxOpenConn
          }
          
  newManager managerSettings

-- | We want to avoid garbled output when writing outputs to STDOUT. hPutBuilder
-- does that already but we want to issue an additional flush.
type Lock = MVar ()

newLock :: IO Lock
newLock =
  newMVar ()

withLock :: Lock -> IO a -> IO a
withLock lock action =
  mask $ \restore ->
    bracket (takeMVar lock) (putMVar lock) (\_ -> restore action)

-- | Logs the request and the extracted info (if present) for further
-- processing due to our processing pipeline.
logRequest ::
  Tracer ->
  -- | Generates a unique'ish id of the request
  IO RequestId ->
  -- | Origin of this request. e.g. us-west-2
  Text ->
  -- | Lock to avoid intermingled output
  Lock ->
  ActiveSpan ->
  Request ->
  -- | Response status
  Status ->
  Maybe RuleCapture ->
  IO ()
logRequest tracer idGen origin lock span request responseStatus capture =
  runTracer tracer $
    traced_ (spanOpts "emit-output" (childOf span)) $ \span -> liftIO $ do
      addTag span ("response-status", IntT (fromIntegral (fromEnum responseStatus)))
      case capture of
        Just FlatfileCapture {fileAbsoluteUrl, filePackage} -> do
          addTag span ("file-absolute-url", StringT (Text.decodeUtf8 fileAbsoluteUrl))
          addTag span ("file-package", StringT filePackage)
        Just DockerCapture {dockerCaptureImage, dockerCaptureReference, dockerCaptureAutoCreate} -> do
          addTag span ("docker-image", StringT (Text.intercalate "/" dockerCaptureImage))
          addTag span ("docker-reference", StringT dockerCaptureReference)
          for_ dockerCaptureAutoCreate $ \rule ->
            addTag span ("docker-auto-create-rule", StringT rule)
        Just (PixelCapture pixelId) ->
          addTag span ("pixel-id", StringT pixelId)
        Just PythonCapture {pythonCapturePackage} -> do
          for_ pythonCapturePackage $ \package ->
            addTag span ("python-package", StringT package)
        Just ScarfJsCapture {} ->
          pure ()
        Nothing ->
          pure ()

      requestId <- idGen
      now <- getCurrentTime
      let capturedRequest =
            captureRequest now requestId request responseStatus capture

      spanCtx <- getSpanContext span

      withLock lock $ do
        hPutBuilder stdout (encodeCapturedRequestToJSON (Just spanCtx) origin capturedRequest <> "\n")
        -- Flush to avoid "stuck" outputs
        hFlush stdout

-- | We use a separate server for healtcheck.
healthcheck :: IO ()
healthcheck = run 8082 $ \request respond -> do
  case pathInfo request of
    ["healthz"] -> do
      respond $ responseLBS ok200 [] mempty
    _ ->
      respond $ responseLBS notFound404 [] mempty

main :: IO ()
main = withOpenSSL $ do

  -- new OpenSSLManager with a fixed number of proxy connections
  manager <- newOpenSSLManager 5

  -- It's recommended to use hSetBinaryMode and BlockBuffering with
  -- hPutBuilder.
  hSetBinaryMode stdout True
  hSetBuffering stdout (BlockBuffering Nothing)

  withRootTracer manager Nothing $ \tracer -> do
    lock <- newLock
    idGen <- newRequestIdGen

    runTracer tracer $ do
      -- A dummy trace to ensure we are outputting something
      traced_ (spanOpts "init" mempty) $ \_span -> do
        pure ()

      traced_ (spanOpts "gateway" mempty) $ \_span -> do
        -- Setting up a background sync to avoid doing IO on the
        -- critical path.

        let gatewayConfig =
              GatewayConfig
                { gatewayModifyProxyDomain =
                    \domain -> (domain, True),
                  -- Currently this is really just for testing purposes. We don't hit
                  -- Redis or anything but instead always returning a fixed set of rules.
                  gatewayDomainRules = \_span _domain -> do
                    pure [],
                  -- Reporting is happening the same way the old Gateway reported things
                  -- is thus a drop-in replacement.
                  gatewayReportRequest =
                    logRequest tracer idGen "my-gateway-application" lock,
                  gatewayProxyTo =
                    proxyTo tracer manager
                }

        -- Go!
        liftIO $
          withAsync healthcheck $ \_ ->
            run 8081 (gateway tracer gatewayConfig)


