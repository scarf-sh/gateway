{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.Async (withAsync)
import Control.Exception (bracket, mask)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (readFile)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime)
import Network.HTTP.Client (Manager, ManagerSettings (..), newManager)
import Network.HTTP.Client.OpenSSL
  ( defaultMakeContext,
    defaultOpenSSLSettings,
    opensslManagerSettings,
    withOpenSSL,
  )
import Network.HTTP.Types (Status, notFound404, ok200)
import Network.Wai (Request (..), responseLBS)
import Network.Wai.Handler.Warp (run)
import OpenSSL.Session (contextSetDefaultVerifyPaths)
import Options.Applicative qualified as Options
import Scarf.Gateway
  ( GatewayConfig (..),
    Rule,
    RuleCapture (..),
    gateway,
    proxyTo,
  )
import Scarf.Gateway.Rule.Capture
  ( RequestId,
    captureRequest,
    newRequestId,
  )
import Scarf.Lib.Tracing
  ( ActiveSpan,
    TagVal (..),
    Tracer,
    addTag,
    childOf,
    getSpanContext,
    runTracer,
    spanOpts,
    traced_,
    withRootTracer,
  )
import Scarf.Manifest (decodeManifest, manifestToRules)
import System.IO
  ( BufferMode (..),
    hFlush,
    hPutStrLn,
    hSetBinaryMode,
    hSetBuffering,
    stdout,
  )

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
logRequest tracer idGen _origin lock span request responseStatus capture =
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

      _spanCtx <- getSpanContext span

      withLock lock $ do
        hPutStrLn stdout (show capturedRequest)
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

newtype Options = Options {manifestPath :: String}

optionsParser :: Options.Parser Options
optionsParser =
  Options
    <$> Options.strOption
      ( Options.long "manifest"
          <> Options.help "Manifest file path"
      )

main :: IO ()
main = withOpenSSL $ do
  Options {manifestPath} <-
    liftIO $
      Options.execParser $
        Options.info (optionsParser Options.<**> Options.helper) mempty

  rules <- readManifest manifestPath

  -- new OpenSSLManager with a fixed number of proxy connections
  manager <- newOpenSSLManager 5

  -- It's recommended to use hSetBinaryMode and BlockBuffering with
  -- hPutBuilder.
  hSetBinaryMode stdout True
  hSetBuffering stdout (BlockBuffering Nothing)

  withRootTracer manager Nothing $ \tracer -> do
    lock <- newLock

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
                    pure rules,
                  -- Reporting is happening the same way the old Gateway reported things
                  -- is thus a drop-in replacement.
                  gatewayReportRequest =
                    logRequest tracer newRequestId "my-gateway-application" lock,
                  gatewayProxyTo =
                    proxyTo tracer manager
                }

        -- Go!
        liftIO $
          withAsync healthcheck $ \_ ->
            run 8081 (gateway tracer gatewayConfig)

readManifest :: FilePath -> IO [Rule]
readManifest manifestFile = do
  content <- Data.ByteString.Lazy.readFile manifestFile
  let manifest = fromMaybe (error "manifest error") (decodeManifest content)
  pure $ concatMap snd $ manifestToRules manifest
