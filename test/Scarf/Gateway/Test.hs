{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Scarf.Gateway.Test (testTree) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack (HasCallStack)
import Network.Wai
  ( Request
      ( requestHeaderHost,
        requestHeaderUserAgent,
        requestHeaders,
        requestMethod
      ),
    rawPathInfo,
    responseLBS,
  )
import Network.Wai.Test
  ( SResponse (..),
    Session,
    assertBody,
    assertHeader,
    assertStatus,
    defaultRequest,
    request,
    runSession,
    setPath,
  )
import Scarf.Gateway
  ( GatewayConfig (..),
    Rule,
    RuleCapture (..),
    gateway,
    newDockerRuleV1,
    newDockerRuleV2,
    newFlatfileRule,
    newPixelRule,
  )
import Scarf.Lib.Tracing (nullTracer)
import Scarf.Manifest (decodeManifest, encodeManifest, manifestToRules)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

gatewayConfigFromManifest :: FilePath -> IO GatewayConfig
gatewayConfigFromManifest manifestFile = do
  content <- Data.ByteString.Lazy.readFile manifestFile
  let manifest = fromMaybe (error "manifest error") (decodeManifest content)
  pure $
    newGatewayConfig $
      fmap (first encodeUtf8) (manifestToRules manifest)

newGatewayConfig ::
  [(ByteString, [Rule])] ->
  GatewayConfig
newGatewayConfig rules =
  let mapping = HashMap.fromList rules
   in GatewayConfig
        { gatewayModifyProxyDomain = \domain -> (domain, True),
          gatewayDomainRules = \_ domain ->
            case HashMap.lookup domain mapping of
              Nothing -> pure []
              Just xs -> pure xs,
          gatewayReportRequest = \_ _ _ _ -> pure (),
          gatewayProxyTo = \_span isSecure host -> \request respond ->
            -- Our dummy proxy always returns 307 for us to inspect
            -- the target location as well as a X-Scarf-Proxy-To
            -- header to be absolutely sure it was going through the
            -- proxy code path.
            let target =
                  (if isSecure then "https://" else "http://")
                    <> host
                    <> rawPathInfo request
             in respond $
                  responseLBS
                    (toEnum 307)
                    [ ("Location", target),
                      ("X-Scarf-Proxy-To", "")
                    ]
                    ""
        }

initialRequest :: ByteString -> Request
initialRequest =
  setPath defaultRequest

gatewayRequest ::
  -- | Host header value
  ByteString ->
  -- | Path
  ByteString ->
  Session SResponse
gatewayRequest host path =
  request $
    (initialRequest path)
      { requestMethod =
          "GET",
        requestHeaderHost =
          Nothing,
        requestHeaderUserAgent =
          -- We don't want to proxy upstream in unit tests
          Just "Docker-Client",
        requestHeaders =
          [ -- Making sure we test the Proxy-Protocol adherence
            ("X-Forwarded-Host", host)
          ]
      }

gatewayProxyRequest ::
  -- | Host header value
  ByteString ->
  -- | Path
  ByteString ->
  Session SResponse
gatewayProxyRequest host path =
  request $
    (initialRequest path)
      { requestMethod =
          "GET",
        requestHeaderHost =
          Nothing,
        requestHeaderUserAgent =
          -- We want to proxy upstream in unit tests
          Just "crio",
        requestHeaders =
          [ -- Making sure we test the Proxy-Protocol adherence
            ("X-Forwarded-Host", host)
          ]
      }

type RunRequest =
  ByteString -> ByteString -> Session (SResponse, Maybe RuleCapture)

-- | Abstracting over how to obtain a 'GatewayConfig' allows us to also
-- read them from Manifests.
class IsGatewayConfig a where
  getConfig :: a -> IO GatewayConfig

instance IsGatewayConfig GatewayConfig where
  getConfig = pure

instance IsGatewayConfig (IO GatewayConfig) where
  getConfig = id

gatewayTestCase ::
  (IsGatewayConfig config) =>
  TestName ->
  config ->
  (RunRequest -> Session a) ->
  TestTree
gatewayTestCase name toConfig session = do
  testCase name $ do
    -- A one-element channel
    chan <- liftIO newEmptyMVar
    config <- liftIO $ getConfig toConfig
    let config' =
          config
            { gatewayReportRequest =
                gatewayReportRequest config
                  <> ( \_span _request _responseStatus capture -> do
                         putMVar chan capture
                     )
            }

        runRequest = \host path -> do
          response <- gatewayRequest host path
          capture <- liftIO (takeMVar chan)
          pure (response, capture)

    _ <- runSession (session runRequest) (gateway nullTracer config')
    pure ()

gatewayProxyTestCase ::
  (IsGatewayConfig config) =>
  TestName ->
  config ->
  (RunRequest -> Session a) ->
  TestTree
gatewayProxyTestCase name toConfig session = do
  testCase name $ do
    -- A one-element channel
    chan <- liftIO newEmptyMVar
    config <- liftIO $ getConfig toConfig
    let config' =
          config
            { gatewayReportRequest =
                gatewayReportRequest config
                  <> ( \_span _request _responseStatus capture -> do
                         putMVar chan capture
                     )
            }

        runRequest = \host path -> do
          response <- gatewayProxyRequest host path
          capture <- liftIO (takeMVar chan)
          pure (response, capture)

    _ <- runSession (session runRequest) (gateway nullTracer config')
    pure ()

assertRedirect :: (HasCallStack) => ByteString -> SResponse -> Session ()
assertRedirect location response = do
  assertStatus 302 response
  assertHeader "Location" location response

-- | We add a X-Scarf-Proxy-To header to witness that a request got proxied.
_assertProxiedRedirect :: (HasCallStack) => ByteString -> SResponse -> Session ()
_assertProxiedRedirect location response = do
  assertStatus 307 response
  assertHeader "X-Scarf-Proxy-To" "" response
  assertHeader "Location" location response

test_gateway_no_rules :: TestTree
test_gateway_no_rules =
  gatewayTestCase "Gateway, no rules" (newGatewayConfig []) $ \request -> do
    (response, _) <- request "test.scarf.sh" "/some-path"
    assertStatus 404 response

test_gateway_docker_rule_v1 :: TestTree
test_gateway_docker_rule_v1 =
  gatewayTestCase
    "Gateway, Docker Pull"
    ( newGatewayConfig
        [ ( "test.scarf.sh",
            [ newDockerRuleV1 "package-1" ["library", "hello-world"] "docker.io"
            ]
          )
        ]
    )
    $ \request -> do
      (response1, capture) <- request "test.scarf.sh" "/v2/_catalog"
      assertStatus 404 response1

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = [],
                  dockerCaptureReference = "",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response2, capture) <- request "test.scarf.sh" "/v2/"
      assertRedirect "https://docker.io/v2/" response2

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = [],
                  dockerCaptureReference = "",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response3, capture) <- request "test.scarf.sh" "/v2/library/hello-world/manifests/latest"
      assertRedirect "https://docker.io/v2/library/hello-world/manifests/latest" response3

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = ["library", "hello-world"],
                  dockerCaptureReference = "latest",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Just "package-1",
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response4, capture) <- request "test.scarf.sh" "/v2/library/nginx/manifests/latest"
      assertStatus 404 response4

      liftIO $ capture @?= Nothing

test_gateway_docker_rule_v1_proxied :: TestTree
test_gateway_docker_rule_v1_proxied =
  gatewayProxyTestCase
    "Gateway, Docker Pull with proxied requests"
    ( newGatewayConfig
        [ ( "test.scarf.sh",
            [ newDockerRuleV1 "package-1" ["library", "hello-world"] "docker.io"
            ]
          )
        ]
    )
    $ \request -> do
      (response1, capture) <- request "test.scarf.sh" "/v2/_catalog"
      assertStatus 404 response1

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = [],
                  dockerCaptureReference = "",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response2, capture) <- request "test.scarf.sh" "/v2/"
      _assertProxiedRedirect "https://docker.io/v2/" response2

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = [],
                  dockerCaptureReference = "",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response3, capture) <- request "test.scarf.sh" "/v2/library/hello-world/manifests/latest"
      _assertProxiedRedirect "https://docker.io/v2/library/hello-world/manifests/latest" response3

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = ["library", "hello-world"],
                  dockerCaptureReference = "latest",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Just "package-1",
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response4, capture) <- request "test.scarf.sh" "/v2/library/nginx/manifests/latest"
      assertStatus 404 response4

      liftIO $ capture @?= Nothing

test_gateway_docker_rule_v2 :: TestTree
test_gateway_docker_rule_v2 =
  gatewayTestCase
    "Gateway, Docker Pull (with auto package creation)"
    ( newGatewayConfig
        [ ( "test.scarf.sh",
            [ newDockerRuleV2 "test-rule-1" "library/*" "docker.io"
            ]
          )
        ]
    )
    $ \request -> do
      (response1, capture) <- request "test.scarf.sh" "/v2/_catalog"
      assertStatus 404 response1

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = [],
                  dockerCaptureReference = "",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response2, _) <- request "test.scarf.sh" "/v2/"
      assertRedirect "https://docker.io/v2/" response2

      (response3, _) <- request "test.scarf.sh" "/v2/something/hello-world/manifests/latest"
      assertStatus 404 response3

      (response4, capture) <- request "test.scarf.sh" "/v2/library/nginx/manifests/latest"
      assertRedirect "https://docker.io/v2/library/nginx/manifests/latest" response4

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = ["library", "nginx"],
                  dockerCaptureReference = "latest",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Just "test-rule-1"
                }
            )

      (response5, capture) <- request "test.scarf.sh" "/v2/library/redis/manifests/1.0"
      assertRedirect "https://docker.io/v2/library/redis/manifests/1.0" response5

      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = ["library", "redis"],
                  dockerCaptureReference = "1.0",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Just "test-rule-1"
                }
            )

test_gateway_manifest_roundtrip :: TestTree
test_gateway_manifest_roundtrip =
  testCase "Manifest roundtrip" $ do
    let test :: FilePath -> IO ()
        test file = do
          content <- Data.ByteString.Lazy.readFile file
          let manifest = fromMaybe (error "manifest error") (decodeManifest content)
          decodeManifest (encodeManifest manifest) @?= Just manifest

    test "test/test-manifest-1.json"
    test "test/test-manifest-2.json"

test_gateway_manifest_2 :: TestTree
test_gateway_manifest_2 =
  gatewayTestCase
    "Gateway, with rules coming from a manifest (2)"
    (gatewayConfigFromManifest "test/test-manifest-2.json")
    $ \request -> do
      (response, _capture) <- request "fabioluz.gateway.scarf53.sh" "/simple/"
      assertStatus 200 response
      assertHeader "Etag" "\"V5vrPPiIHdnkkoBivA0iBAHsMgm_e-IFA0AhY9HoOEw=\"" response

      (response, _capture) <- request "fabioluz.gateway.scarf53.sh" "/simple/numpy/"
      assertStatus 200 response
      assertHeader "Etag" "\"C_m1QvctH-XxmPfkfMx-IxrY6sVUBmaTq2KOmVSEdPc=\"" response

      (response, _capture) <- request "fabioluz.gateway.scarf53.sh" "/simple/numpy/numpy-1.22.1.zip"
      assertRedirect "https://files.pythonhosted.org/packages/0a/c8/a62767a6b374a0dfb02d2a0456e5f56a372cdd1689dbc6ffb6bf1ddedbc0/numpy-1.22.1.zip#sha256=e348ccf5bc5235fc405ab19d53bec215bb373300e5523c7b476cc0da8a5e9973" response

test_gateway_manifest :: TestTree
test_gateway_manifest =
  gatewayTestCase
    "Gateway, with rules coming from a manifest"
    (gatewayConfigFromManifest "test/test-manifest-1.json")
    $ \request -> do
      (response1, capture) <- request "alexbiehl2.docker.scarf53.sh" "/v2/library/proxy/manifests/latest"
      assertRedirect "https://docker.io/v2/library/proxy/manifests/latest" response1
      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = ["library", "proxy"],
                  dockerCaptureReference = "latest",
                  dockerCaptureBackendRegistry = "docker.io",
                  dockerCapturePackage = Nothing,
                  dockerCaptureAutoCreate = Just "4"
                }
            )

      (response2, capture) <- request "dysinger.docker.scarf53.sh" "/v2/library/controller/manifests/latest"
      assertRedirect "https://ghcr.io/v2/library/controller/manifests/latest" response2
      liftIO $
        capture
          @?= Just
            ( DockerCapture
                { dockerCaptureImage = ["library", "controller"],
                  dockerCaptureReference = "latest",
                  dockerCaptureBackendRegistry = "ghcr.io",
                  dockerCapturePackage = Just "aaf2ec15-5244-484b-845a-ffd559e5f802",
                  dockerCaptureAutoCreate = Nothing
                }
            )

      (response3, capture) <- request "mochajen.gateway.scarf53.sh" "/avisstoragebox/darwin/1.0"
      assertRedirect "https://github.com/aviaviavi/curl-runnings/releases/download/1.0/curl-runnings-1.0-darwin.tar.gz" response3
      liftIO $
        capture
          @?= Just
            ( FlatfileCapture
                { fileAbsoluteUrl = "https://github.com/aviaviavi/curl-runnings/releases/download/1.0/curl-runnings-1.0-darwin.tar.gz",
                  fileVariables =
                    HashMap.fromList
                      [ ("platform", "darwin"),
                        ("version", "1.0")
                      ],
                  filePackage = "9495e1d9-2832-4a3d-8b98-b334173afb17"
                }
            )

      (response4, capture) <- request "mochajen.gateway.scarf53.sh" "/something-here/windows/ver1.0.tar.gz"
      assertRedirect "https://bucket.s3.aws.amazon.com/something-here/windows/ver1.0.tar.gz" response4
      liftIO $
        capture
          @?= Just
            ( FlatfileCapture
                { fileAbsoluteUrl = "https://bucket.s3.aws.amazon.com/something-here/windows/ver1.0.tar.gz",
                  fileVariables = HashMap.empty,
                  filePackage = "44d540c0-41f2-469f-98de-50e1a705bd65"
                }
            )

      (response5, capture) <- request "ctl.test.org" "/install"
      assertRedirect "https://raw.githubusercontent.com/testorg/testctl/master/install.sh" response5
      liftIO $
        capture
          @?= Just
            ( FlatfileCapture
                { fileAbsoluteUrl = "https://raw.githubusercontent.com/testorg/testctl/master/install.sh",
                  fileVariables = HashMap.empty,
                  filePackage = "aaf2ec15-5244-484b-845a-ffd559e5f802"
                }
            )

test_gateway_file_rule :: TestTree
test_gateway_file_rule =
  gatewayTestCase
    "Gateway, one file rule"
    ( newGatewayConfig
        [ ( "test.scarf.sh",
            [ newFlatfileRule
                "package-123"
                "/{package}-{os}-{arch}"
                "https://some-backend-1/downloads/{package}/{os}/{arch}"
            ]
          )
        ]
    )
    $ \request -> do
      (response1, capture) <- request "test.scarf.sh" "/minikube-linux-amd64"
      assertRedirect "https://some-backend-1/downloads/minikube/linux/amd64" response1

      liftIO $
        capture
          @?= Just
            ( FlatfileCapture
                { fileAbsoluteUrl = "https://some-backend-1/downloads/minikube/linux/amd64",
                  fileVariables =
                    HashMap.fromList
                      [ ("package", "minikube"),
                        ("os", "linux"),
                        ("arch", "amd64")
                      ],
                  filePackage = "package-123"
                }
            )

      (response2, capture) <- request "test.scarf.sh1" "/kubernetes-1.0.tar.gz"
      assertStatus 404 response2
      liftIO $ capture @?= Nothing

      (response3, capture) <- request "test.scarf.sh" "/minikube"
      assertStatus 404 response3
      liftIO $ capture @?= Nothing

      (response4, capture) <- request "test.scarf.sh" "/minikube-linux"
      assertStatus 404 response4
      liftIO $ capture @?= Nothing

      (response5, capture) <- request "test.scarf.sh" "/minikube-linux-amd64-hallo"
      assertRedirect "https://some-backend-1/downloads/minikube/linux/amd64-hallo" response5

      liftIO $
        capture
          @?= Just
            ( FlatfileCapture
                { fileAbsoluteUrl = "https://some-backend-1/downloads/minikube/linux/amd64-hallo",
                  fileVariables =
                    HashMap.fromList
                      [ ("package", "minikube"),
                        ("os", "linux"),
                        ("arch", "amd64-hallo")
                      ],
                  filePackage = "package-123"
                }
            )

test_gateway_file_rule_2 :: TestTree
test_gateway_file_rule_2 =
  gatewayTestCase
    "Gateway, one file rule (contd.)"
    ( newGatewayConfig
        [ ( "test.scarf.sh",
            [ newFlatfileRule
                "package-123"
                "/{package}-{version}.{ext}"
                "https://some-backend-1/downloads/{package}/{version}/{ext}"
            ]
          )
        ]
    )
    $ \request -> do
      (response1, capture) <- request "test.scarf.sh" "/minikube-1.0.tar.gz"
      assertRedirect "https://some-backend-1/downloads/minikube/1/0.tar.gz" response1

      liftIO $
        capture
          @?= Just
            ( FlatfileCapture
                { fileAbsoluteUrl = "https://some-backend-1/downloads/minikube/1/0.tar.gz",
                  fileVariables =
                    HashMap.fromList
                      [ ("ext", "0.tar.gz"),
                        ("package", "minikube"),
                        ("version", "1")
                      ],
                  filePackage = "package-123"
                }
            )

      (response2, capture) <- request "test.scarf.sh" "/minikube-1.0.iso"
      assertRedirect "https://some-backend-1/downloads/minikube/1/0.iso" response2

      liftIO $
        capture
          @?= Just
            ( FlatfileCapture
                { fileAbsoluteUrl = "https://some-backend-1/downloads/minikube/1/0.iso",
                  fileVariables =
                    HashMap.fromList
                      [ ("ext", "0.iso"),
                        ("package", "minikube"),
                        ("version", "1")
                      ],
                  filePackage = "package-123"
                }
            )

test_gateway_pixel_rule :: TestTree
test_gateway_pixel_rule =
  gatewayTestCase
    "Gateway, pixel rule"
    ( newGatewayConfig
        [ ( "static.scarf.sh",
            [ newPixelRule
                -- To avoid confusion: It just doesn't matter whether we return
                -- an image or anything else, from the Gateways perspective it's
                -- all bytes. Just make sure they match up.
                "application/json"
                "{}"
            ]
          )
        ]
    )
    $ \request -> do
      (response1, capture) <- request "static.scarf.sh" "/a.png?x-pxid=12345"
      assertStatus 200 response1
      assertHeader "Content-Type" "application/json" response1
      assertBody "{}" response1

      liftIO $ capture @?= Just (PixelCapture "12345")

      -- We expect the request to not fail in case of missing pixel id
      (response2, capture) <- request "static.scarf.sh" "/a.png?"
      assertStatus 200 response2
      assertHeader "Content-Type" "application/json" response2
      assertBody "{}" response2

      liftIO $ capture @?= Just (PixelCapture "")

      (response3, capture) <- request "static.scarf.sh" "/b.png"
      assertStatus 404 response3

      liftIO $ capture @?= Nothing

testTree :: IO [TestTree]
testTree =
  pure
    [ test_gateway_no_rules,
      test_gateway_docker_rule_v1,
      test_gateway_docker_rule_v2,
      test_gateway_docker_rule_v1_proxied,
      test_gateway_file_rule,
      test_gateway_file_rule_2,
      test_gateway_manifest,
      test_gateway_manifest_2,
      test_gateway_manifest_roundtrip,
      test_gateway_pixel_rule
    ]
