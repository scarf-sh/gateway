{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Scarf.Gateway.Golden (test_gateway_golden) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.ByteString.Builder (stringUtf8)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (Null),
    decode,
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Encode.Pretty (confCompare, defConfig, encodePretty')
import Data.Aeson.Encoding (dict, int, pair, pairs, text, unsafeToEncoding)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Yaml (decodeFileThrow)
import Network.Wai
  ( Request
      ( requestBody,
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
import Scarf.Gateway.Manifest (Manifest, manifestToRules)
import Scarf.Gateway.Rule
  ( newPixelRule,
    newScarfJsRule,
    optimizeRules,
  )
import Scarf.Gateway.Rule.Capture
  ( CapturedRequest,
    RequestId (RequestId),
    captureRequest,
  )
import Scarf.Lib.Tracing (nullTracer)
import System.FilePath (replaceExtension)
import System.IO.Unsafe (unsafeInterleaveIO)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)

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
  { -- | Response status code
    outputStatus :: Int,
    -- | Response HTTP headers
    outputHeaders :: [(Text, Text)],
    -- | Response Capture, this is what we feed into the DockerLogProcessor
    outputCapture :: CapturedRequest
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
      mapping = fmap (optimizeRules . sort) (HashMap.fromList rules)
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
  inputFiles <- findByExtension [".yaml"] testsDirectory
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
      chan <- newEmptyMVar
      let config =
            (newGatewayConfig inputManifest)
              { gatewayReportRequest = \_span request responseStatus capture -> do
                  putMVar chan $
                    captureRequest
                      (read "2022-01-11 08:34:00.914835 UTC")
                      (RequestId 0 0 0)
                      request
                      responseStatus
                      capture
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
      capture <- takeMVar chan

      -- We want to have clear and deterministic diffs. For that we have to run the produced
      -- JSON through aeson-pretty.
      let json :: Value
          json =
            fromMaybe (error "impossible") $
              decode $
                encode $
                  Output
                    { outputStatus =
                        fromEnum simpleStatus,
                      outputHeaders =
                        fmap (bimap (decodeUtf8 . CaseInsensitive.original) decodeUtf8) simpleHeaders,
                      outputCapture =
                        capture
                    }
      pure (encodePretty' (defConfig {confCompare = compare}) json, simpleBody)

    let compareFiles = \ref new -> ["diff", "-u", ref, new]

    pure
      [ goldenVsStringDiff
          inputFile
          compareFiles
          (replaceExtension inputFile "output.json")
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
  toJSON =
    error "Unimplemented due to CapturedRequest not having a ToJSON instance for performance reasons"

  toEncoding Output {..} =
    pairs $
      pair "status" (int outputStatus)
        <> pair
          "headers"
          (dict text text Map.foldrWithKey (Map.fromList outputHeaders))
        <> pair
          "capture"
          ( unsafeToEncoding
              ( stringUtf8 $ show outputCapture
              )
          )
