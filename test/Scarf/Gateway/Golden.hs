{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Scarf.Gateway.Golden (test_gateway_golden) where

import Control.Monad (when)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (Null),
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
  )
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Yaml (decodeFileThrow, encode)
import Network.Wai
  ( Request
      ( rawQueryString,
        requestBody,
        requestHeaderHost,
        requestHeaderReferer,
        requestHeaderUserAgent,
        requestHeaders,
        requestMethod
      ),
    defaultRequest,
    rawPathInfo,
    responseLBS,
  )
import Network.Wai.Test
  ( SResponse
      ( SResponse,
        simpleBody,
        simpleHeaders,
        simpleStatus
      ),
    request,
    runSession,
    setPath,
  )
import Scarf.Gateway
  ( GatewayConfig (..),
    gateway,
  )
import Scarf.Gateway.Rule
  ( newPixelRule,
    newScarfJsRule,
    optimizeRules,
    sortRules,
  )
import Scarf.Gateway.Rule.Capture
  ( CapturedRequest,
    captureRequest,
    newRequestId,
  )
import Scarf.Lib.Tracing (nullTracer)
import Scarf.Manifest (Manifest, manifestToRules)
import System.FilePath (replaceExtension, takeExtensions)
import System.IO.Unsafe (unsafeInterleaveIO)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Text.Show.Pretty qualified

-- | Input values for Golden tests.
data Input = Input
  { -- | Relative path of the url. Must start with /
    inputPath :: Text,
    -- | HTTP method to use for the request. Defaults to "GET" if omitted.
    inputMethod :: Text,
    -- | Headers to add to the test request
    inputHeaders :: [(Text, Text)],
    -- | Manifest to use for the Gateway
    inputManifest :: Manifest
  }

-- | Test output value.
data Output = Output
  { -- | Query string captured from the request
    outputQueryString :: Text,
    -- | Response status code
    outputStatus :: Int,
    -- | Response HTTP headers
    outputHeaders :: [(Text, Text)],
    -- | Response Capture(s), this is what we feed into the DockerLogProcessor
    outputCaptures :: [CapturedRequest]
  }

-- | Construct a new 'GatewayConfig' from a 'Manifest'.
newGatewayConfig ::
  Manifest ->
  GatewayConfig
newGatewayConfig manifest =
  let rules =
        fmap (first encodeUtf8) $
          ("scarf.sh", [newScarfJsRule])
            : ("static.scarf.sh", [newPixelRule "image/png" "<this could be your png>"])
            : manifestToRules manifest
      mapping = fmap (optimizeRules . sortRules) (HashMap.fromList rules)
   in GatewayConfig
        { gatewayModifyProxyDomain = \domain -> (domain, True),
          gatewayDomainRules = \_ domain ->
            case HashMap.lookup domain mapping of
              Nothing -> pure []
              Just xs -> pure xs,
          gatewayReportRequest = \_ _ _ _ -> pure (),
          gatewayProxyTo = \_span isSecure host -> \request respond ->
            let target =
                  (if isSecure then "https://" else "http://")
                    <> host
                    <> rawPathInfo request
             in respond $
                  responseLBS
                    (toEnum 307)
                    [ ("Location", target),
                      ("X-This-Request-Was-Proxied", "1")
                    ]
                    ""
        }

test_gateway_golden :: IO [TestTree]
test_gateway_golden = goldenTests "test/golden"

goldenTests :: FilePath -> IO [TestTree]
goldenTests testsDirectory = do
  inputFiles' <- findByExtension [".yaml"] testsDirectory

  -- The glob above also includes the .output.yaml files. Ensure
  -- we only take the input files.
  let inputFiles =
        filter
          (\file -> takeExtensions file == ".yaml")
          inputFiles'

  when (null inputFiles) $
    error "No golden tests!"
  flip foldMap inputFiles $ \inputFile -> do
    Input {..} <- decodeFileThrow inputFile
    -- Only read body file if it the Gateway wants to
    body <-
      unsafeInterleaveIO $
        ByteString.readFile (replaceExtension inputFile "input.body")
    bodyRef <- newIORef body
    -- unsafeInterleaveIO defers running the test until testResult is demanded.
    -- Unfortunately tasty-golden doesn't support having multiple files as output.
    -- With this trick we run the underlying test only once but are able to leverage
    -- all of tasty-goldens goodies and nice integration with the tasty ecosystem.
    testResult <- unsafeInterleaveIO $ do
      -- Run the input in a Wai session and produce a Response and a CaptureRequest
      chan <- newIORef []
      requestId <- newRequestId
      let config =
            (newGatewayConfig inputManifest)
              { gatewayReportRequest = \_span request responseStatus capture -> do
                  captures <- readIORef chan
                  writeIORef chan $
                    ( request,
                      captureRequest
                        (read "2022-01-11 08:34:00.914835 UTC")
                        requestId
                        request
                        responseStatus
                        capture
                    )
                      : captures
              }
          headers =
            fmap (bimap (CaseInsensitive.mk . encodeUtf8) encodeUtf8) inputHeaders
      SResponse
        { simpleStatus,
          simpleHeaders,
          simpleBody
        } <- flip runSession (gateway nullTracer config) $ do
        request $
          (setPath defaultRequest (encodeUtf8 inputPath))
            { requestMethod =
                encodeUtf8 inputMethod,
              requestHeaderHost =
                lookup "Host" headers,
              requestHeaderUserAgent =
                lookup "User-Agent" headers,
              requestHeaderReferer =
                lookup "Referer" headers,
              requestHeaders =
                headers,
              requestBody = do
                result <- readIORef bodyRef
                writeIORef bodyRef ByteString.empty
                pure result
            }

      captures <- readIORef chan

      let request =
            case captures of
              (request, _capture) : _ -> request
              _ -> error "no captures emitted during test"

          output =
            Output
              { outputQueryString =
                  decodeUtf8 (rawQueryString request),
                outputStatus =
                  fromEnum simpleStatus,
                outputHeaders =
                  fmap (bimap (decodeUtf8 . CaseInsensitive.original) decodeUtf8) simpleHeaders,
                outputCaptures =
                  fmap snd captures
              }

      pure (Data.ByteString.Lazy.fromStrict (Data.Yaml.encode output), simpleBody)

    let compareFiles = \ref new -> ["diff", "-u", ref, new]

    pure
      [ goldenVsStringDiff
          inputFile
          compareFiles
          (replaceExtension inputFile "output.yaml")
          (pure (fst testResult)),
        goldenVsStringDiff
          (inputFile <> " body")
          compareFiles
          (replaceExtension inputFile "output.body")
          (pure (snd testResult))
      ]

instance ToJSON Input where
  toJSON Input {..} =
    object $
      filter
        (not . isNull . snd)
        [ "path" .= inputPath,
          "headers" .= case inputHeaders of
            [] -> Null
            _ -> toJSON (Map.fromList inputHeaders),
          "manifest" .= inputManifest
        ]
    where
      isNull Null = True
      isNull _ = False

instance FromJSON Input where
  parseJSON = withObject "Input" $ \o -> do
    Input
      <$> o .: "path"
      <*> o .:? "method" .!= "GET"
      <*> (fmap Map.toList <$> (o .:? "headers")) .!= []
      <*> o .: "manifest"

instance ToJSON Output where
  toJSON Output {..} =
    object $
      [ "status" .= outputStatus,
        "headers" .= Map.fromList outputHeaders,
        "captures" .= Text.Show.Pretty.ppShow outputCaptures
      ]
        <> [ "query" .= outputQueryString
             | outputQueryString /= ""
           ]
