{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Scarf.Gateway.Rule
  ( -- * Rules
    Rule,
    newDockerRuleV1,
    newDockerRuleV2,
    newFlatfileRule,
    newFileRuleV2,
    newPixelRule,
    newScarfJsRule,
    newPythonRule,
    optimizeRules,
    RedirectOrProxy (..),
    ResponseHeaders (..),

    -- * Matching rules
    RuleCapture (..),
    MonadMatch,
    runMatch,
    match,
  )
where

import Crypto.Hash.SHA256 (hashlazy)
import Data.Aeson.Parser qualified as Aeson
import Data.Attoparsec.ByteString qualified as Attoparsec
import Data.ByteString
  ( ByteString,
    isInfixOf,
    isPrefixOf,
  )
import Data.ByteString.Base64.URL (encode)
import Data.ByteString.Builder (Builder, char7, string7, toLazyByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types.URI (Query, parseQuery, parseQueryText)
import Network.URI (URI (..), parseRelativeReference)
import Network.Wai
  ( getRequestBodyChunk,
    pathInfo,
    rawPathInfo,
    rawQueryString,
    requestHeaderUserAgent,
    requestMethod,
  )
import Network.Wai qualified as Wai
import Network.Wai.Internal (Response (..))
import Scarf.Gateway.ImagePattern qualified as ImagePattern
import Scarf.Gateway.Regex qualified as Regex
import Scarf.Gateway.Rule.Capture (RuleCapture (..))
import Scarf.Gateway.URLTemplate (URLTemplate)
import Scarf.Gateway.URLTemplate qualified as URLTemplate

-- | The monad the rule matchers operate in.
-- TODO for now it's IO but it will need to become a bit
-- more elaborate.
type MonadMatch m = (m ~ IO)

-- | Run a MonadMatch into IO.
runMatch :: MonadMatch m => m a -> IO a
runMatch = id

-- | Rule that matches a particular image and redirects (or proxies)
-- to a backend registry.
data DockerRuleV1 = DockerRuleV1
  { -- | Images matching this rule. e.g. ["library", "hello-world"] and their
    -- respective package ids.
    ruleImages :: !(HashMap [Text] Text),
    -- | Domain of the registry to redirect (or proxy) to.
    ruleBackendRegistry :: !ByteString
  }
  deriving (Eq, Show)

data DockerRuleV2 = DockerRuleV2
  { -- | Images matching this pattern will be considered a rule
    -- match. Images matching a rule will be flagged for auto-creation.
    ruleImagePattern :: !ImagePattern.Pattern,
    -- | Id of the rule in the backend.
    ruleRuleId :: !Text,
    -- | Domain of the registry to redirect (or proxy) to.
    ruleBackendRegistry :: !ByteString
  }
  deriving (Eq, Show)

data FlatfileRule = FlatfileRule
  { -- | Package this rule belongs to.
    rulePackage :: Text,
    -- | The set of variables that need to be present to accept a match.
    ruleExpectedVariables :: !(HashSet Text),
    -- | URL template for the public url that users use to download
    -- the artifacts. Beware: the public template is only applicable
    -- to the path part of the request, including leading "/"
    -- e.g. /minikube-{platform}-{version}.tar.gz
    rulePublicTemplate :: !URLTemplate,
    -- | URL template for the url we redirect to from the public url.
    -- notice the use of the template variables {platform} + {version}
    -- e.g. https://github.com/kubernetes/minikube/releases/downloads/minikube-{platform}-{version}.tar.gzf
    ruleBackendTemplate :: !URLTemplate
  }
  deriving (Eq, Show)

instance Ord FlatfileRule where
  a `compare` b = rulePublicTemplate a `compare` rulePublicTemplate b

data FileRuleV2 = FileRuleV2
  { -- | Package this rule belongs to.
    rulePackage :: Text,
    -- | The set of variables that need to be present to accept a match.
    ruleExpectedVariables :: !(HashSet Text),
    -- | Regular expression matching the incoming request path.
    -- Beware: the public template is only applicable
    -- to the path part of the request, including leading "/"
    -- e.g. /minikube-{platform}-{version}.tar.gz
    ruleIncomingPathRegex :: !Regex.Regex,
    -- | URL template for the url we redirect to from the public url.
    -- notice the use of the template variables {platform} + {version}
    -- e.g. https://github.com/kubernetes/minikube/releases/downloads/minikube-{platform}-{version}.tar.gzf
    ruleBackendTemplate :: !URLTemplate
  }
  deriving (Eq, Show)

instance Ord FileRuleV2 where
  a `compare` b = ruleIncomingPathRegex a `compare` ruleIncomingPathRegex b

-- | Matching Scarf's documentation pixels. We don't validate the pixel-id
-- at request time. Just log the access and carry on.
data PixelRule = PixelRule
  { ruleTransparentImageContentType :: !ByteString,
    ruleTransparentImage :: !LBS.ByteString
  }
  deriving (Eq, Show)

-- | A file in a Python package. e.g. numpy-1.3.0.tar.gz, numpy-1.3.0.tar.gz.sig
data PythonFile = PythonFile
  { -- | e.g. 1.0
    fileVersion :: !Text,
    -- | e.g. (md5, 68b329da9893e34099c7d8ad5cb9c940)
    fileHashValue :: !(Maybe (Text, Text)),
    -- | A value of either true or false to indicate whether or not there is a GPG signature.
    fileGPGSig :: !(Maybe Bool),
    -- | The Python version(s) that the distribution is guaranteed to be compatible with.
    -- The value follows the PEP 345 spec.
    fileRequiresPython :: !(Maybe Text),
    -- | The backend URL to download the package.
    fileBackendURL :: !Text
  }
  deriving stock (Show, Eq, Ord)

-- | Python package. e.g. numpy, scipy, fabric, etc.
data PythonPackage = PythonPackage
  { -- | Scarf package identifier
    packageId :: !Text,
    -- | Pre-rendered HTML for the simple index package page.
    -- This is calculated lazily.
    packageProjectPageHTML :: LBS.ByteString,
    -- | Files belonging to the Python package
    packageFiles :: !(HashMap Text PythonFile),
    -- | Pre-calculated etag for this package.
    -- This is calculated lazily.
    packageEtag :: ByteString
  }
  deriving stock (Show, Eq, Ord)

-- | A 'PythonRuleV1' is a redirecting version of a Python package repository
-- which presents itself as a Python Simple Index according to
-- https://www.python.org/dev/peps/pep-0503/.
--
-- 'optimizeRules' is able to freely merge 'PythonRuleV1' on the same domain.
-- In doing so it takes great care to not pre-calculate the HTML and ETags
-- more than necessary.
data PythonRuleV1 = PythonRuleV1
  { -- Python packages in this Python repository.
    rulePackages :: !(HashMap Text PythonPackage),
    -- | Pre-rendered HTML for the simple index root page.
    -- This is calculated lazily.
    ruleRootRepoHTML :: LBS.ByteString,
    -- | Pre-calculated etag for the root page.
    -- This is calculated lazily.
    ruleEtag :: ByteString
  }
  deriving stock (Show, Eq, Ord)

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
        Text.unpack $ Text.decodeUtf8 (rawPathInfo request),
      requestQueryString =
        Text.unpack $ Text.decodeUtf8 (rawQueryString request)
    }

data Rule
  = RuleDockerV1 DockerRuleV1
  | RuleDockerV2 DockerRuleV2
  | RuleFlatfile FlatfileRule
  | RuleFileV2 FileRuleV2
  | RulePixel PixelRule
  | RulePythonV1 PythonRuleV1
  | RuleScarfJs
  deriving (Show, Eq)

instance Ord Rule where
  a `compare` b = case (a, b) of
    (RuleDockerV1 _, RuleDockerV1 _) -> EQ
    (RuleDockerV1 _, _) -> LT
    (RuleDockerV2 _, RuleDockerV1 _) -> GT
    (RuleDockerV2 _, _) -> LT
    (RulePixel _, RuleDockerV1 _) -> GT
    (RulePixel _, RuleDockerV2 _) -> GT
    (RulePixel _, RulePixel _) -> EQ
    (RulePixel _, _) -> LT
    (RuleFlatfile _, RuleDockerV1 _) -> GT
    (RuleFlatfile _, RuleDockerV2 _) -> GT
    (RuleFlatfile _, RulePixel _) -> GT
    (RuleFlatfile f1, RuleFlatfile f2) -> f1 `compare` f2
    (RuleFlatfile _, RuleFileV2 _) -> LT
    (RuleFlatfile _, RulePythonV1 _) -> LT
    (RuleFileV2 _, RuleDockerV1 _) -> GT
    (RuleFileV2 _, RuleDockerV2 _) -> GT
    (RuleFileV2 _, RulePixel _) -> GT
    (RuleFileV2 _, RuleFlatfile _) -> GT
    (RuleFileV2 f1, RuleFileV2 f2) -> f1 `compare` f2
    (RuleFileV2 _, RulePythonV1 _) -> LT
    (RulePythonV1 _, RuleDockerV1 _) -> GT
    (RulePythonV1 _, RuleDockerV2 _) -> GT
    (RulePythonV1 _, RulePixel _) -> GT
    (RulePythonV1 _, RuleFlatfile _) -> GT
    (RulePythonV1 _, RuleFileV2 _) -> GT
    (RulePythonV1 p1, RulePythonV1 p2) -> p1 `compare` p2
    (RuleScarfJs, RuleScarfJs) -> EQ
    (RuleScarfJs, _) -> LT
    (_, RuleScarfJs) -> GT

data ResponseHeaders = ResponseHeaders
  { contentType :: ByteString,
    contentLength :: Maybe Int,
    etag :: Maybe ByteString,
    cacheControl :: Maybe ByteString
  }

-- | Indicates whether a request needs to be redirect to or proxied.
data RedirectOrProxy
  = -- | Absolute url, e.g. https://registry.docker.com/v2/library/hello-world/..
    RedirectTo
      !RuleCapture
      !ByteString
  | -- | Host to proxy the request to, e.g. registry.docker.com
    ProxyTo
      (Response -> RuleCapture)
      !ByteString
  | -- | Respond with bytes directly.
    RespondBytes
      !RuleCapture
      !ResponseHeaders
      -- ^ Headers we might want sent back
      !LBS.ByteString
      -- ^ Response body
  | -- | We want to return a 404 for GET /v2/_catalog but if we return
    -- Nothing in the matcher it would attempt to match other rules.
    -- This constructor is used to explicitly return a 404.
    RespondNotFound
      !RuleCapture
  | -- | Etag for caching
    RespondNotModified
      !RuleCapture
      !ByteString
  | RespondMethodNotAllowed
  | RespondInvalidRequest

-- | registry.hub.docker.com no longer works. Until
-- we fixed up all the data in the database we patch it
-- here.
sanitizeDockerhubDomain :: Text -> Text
sanitizeDockerhubDomain domain = case domain of
  "registry.hub.docker.com" -> "registry-1.docker.io"
  domain -> domain

newDockerRuleV1 :: Text -> [Text] -> Text -> Rule
newDockerRuleV1 package image backendDomain =
  RuleDockerV1
    DockerRuleV1
      { ruleImages = HashMap.singleton image package,
        ruleBackendRegistry = Text.encodeUtf8 (sanitizeDockerhubDomain backendDomain)
      }

newDockerRuleV2 :: Text -> Text -> Text -> Rule
newDockerRuleV2 ruleId textPattern backendDomain =
  RuleDockerV2
    DockerRuleV2
      { ruleImagePattern =
          case ImagePattern.fromText textPattern of
            Nothing -> error "Invalid Pattern"
            Just p -> p,
        ruleRuleId = ruleId,
        ruleBackendRegistry = Text.encodeUtf8 (sanitizeDockerhubDomain backendDomain)
      }

-- | Unsafely parses the templates and returns a flatfile rule.
newFlatfileRule ::
  -- | Package identifier
  Text ->
  -- | Public URL template
  Text ->
  -- | Backend URL template
  Text ->
  Rule
newFlatfileRule package public backend =
  let unsafeParseTemplate x = case URLTemplate.fromText x of
        Nothing -> error "Invalid URLTemplate"
        Just t -> t

      backendTemplate = unsafeParseTemplate backend
   in RuleFlatfile
        FlatfileRule
          { rulePackage = package,
            ruleExpectedVariables = HashSet.fromList (URLTemplate.variables backendTemplate),
            rulePublicTemplate = unsafeParseTemplate public,
            ruleBackendTemplate = backendTemplate
          }

newFileRuleV2 ::
  -- | Package identifier
  Text ->
  -- | Incoming path regex
  Regex.Regex ->
  -- | Backend URL template
  URLTemplate ->
  Rule
newFileRuleV2 package incoming backend =
  RuleFileV2
    FileRuleV2
      { rulePackage = package,
        ruleExpectedVariables = HashSet.fromList (URLTemplate.variables backend),
        ruleIncomingPathRegex = incoming,
        ruleBackendTemplate = backend
      }

newPixelRule ::
  -- | Content-type of pixel
  ByteString ->
  -- | Bytes of pixel
  ByteString ->
  Rule
newPixelRule contentType image =
  RulePixel
    PixelRule
      { ruleTransparentImageContentType = contentType,
        ruleTransparentImage = LBS.fromStrict image
      }

newScarfJsRule :: Rule
newScarfJsRule = RuleScarfJs

newPythonRule ::
  -- | Package name, without version e.g aca.
  Text ->
  -- | File name e.g. aca-0.1.2.tar.gz, aca-0.1.2.tar.gz.sig, aca_x86-64-linux-0.1.2.whl
  Text ->
  -- | Version
  Text ->
  -- | Hash method and hash. e.g. ("md5", "401b30e3b8b5d629635a5c613cdb7919")
  Maybe (Text, Text) ->
  -- | GPG Signature
  Maybe Bool ->
  -- | Requires Python version
  Maybe Text ->
  -- | Backend URL
  Text ->
  -- | Scarf Package Id
  Text ->
  Rule
newPythonRule packageName fileName version hashValue gpgSig reqPython backendURL packageId =
  let file =
        PythonFile
          { fileVersion = version,
            fileHashValue = hashValue,
            fileGPGSig = gpgSig,
            fileRequiresPython = reqPython,
            fileBackendURL = backendURL
          }
      projectPageHTML =
        pythonProjectPageHTML packageName package
      package =
        PythonPackage
          { packageId = packageId,
            packageProjectPageHTML = projectPageHTML,
            packageFiles = HashMap.singleton fileName file,
            packageEtag = pythonPackageEtag package
          }
      rootRepoHTML =
        pythonRootRepoHTML rule
      rule =
        PythonRuleV1
          { rulePackages = HashMap.singleton packageName package,
            ruleRootRepoHTML = rootRepoHTML,
            ruleEtag = pythonRuleEtag rule
          }
   in RulePythonV1 rule

pythonPackageEtag :: PythonPackage -> ByteString
pythonPackageEtag PythonPackage {..} =
  -- We want the Etag to change in case the backend urls change too.
  let input =
        "scarf-python-package"
          `LBS.append` LBS.fromChunks
            ( map
                (Text.encodeUtf8 . fileBackendURL)
                (HashMap.elems packageFiles)
            )
          `LBS.append` packageProjectPageHTML
   in encode (hashlazy input)

pythonRuleEtag :: PythonRuleV1 -> ByteString
pythonRuleEtag PythonRuleV1 {..} =
  let input = "scarf-python-rule" `LBS.append` ruleRootRepoHTML
   in encode (hashlazy input)

pythonRootRepoHTML :: PythonRuleV1 -> LBS.ByteString
pythonRootRepoHTML PythonRuleV1 {..} =
  toLazyByteString $
    string7 "<!DOCTYPE html><html><body>\n"
      <> HashMap.foldrWithKey (\k _ acc -> buildProjectLink k <> acc) mempty rulePackages
      <> string7 "</body></html>"
  where
    buildProjectLink :: Text -> Builder
    buildProjectLink packageName =
      string7 "<a href=\"/simple/"
        <> Text.encodeUtf8Builder packageName
        <> string7 "/\">"
        <> Text.encodeUtf8Builder packageName
        <> string7 "</a><br>\n"

pythonProjectPageHTML :: Text -> PythonPackage -> LBS.ByteString
pythonProjectPageHTML packageName PythonPackage {packageFiles = files} =
  toLazyByteString $
    string7 "<!DOCTYPE html><html><body><h1>"
      <> Text.encodeUtf8Builder packageName
      <> string7 "</h1>\n"
      <> HashMap.foldrWithKey (\k v acc -> buildFileLink packageName k v <> acc) mempty files
      <> string7 "</body></html>"
  where
    buildFileLink :: Text -> Text -> PythonFile -> Builder
    buildFileLink packageName fileName PythonFile {..} =
      string7 "<a href=\"/simple/"
        <> Text.encodeUtf8Builder packageName
        <> char7 '/'
        <> Text.encodeUtf8Builder fileName
        <> maybe
          mempty
          (\(hashType, hash) -> char7 '#' <> Text.encodeUtf8Builder hashType <> char7 '=' <> Text.encodeUtf8Builder hash)
          fileHashValue
        <> char7 '\"'
        <> maybe mempty (\x -> string7 " data-gpg-sig=\"" <> string7 (if x then "true" else "false") <> char7 '\"') fileGPGSig
        <> maybe mempty (\x -> string7 " data-requires-python=\"" <> Text.encodeUtf8Builder x <> char7 '\"') fileRequiresPython
        <> char7 '>'
        <> Text.encodeUtf8Builder fileName
        <> string7 "</a><br>\n"

-- | Common up rules if possible to reduce the number of rules. It's always safe
-- to apply this function.
optimizeRules :: [Rule] -> [Rule]
optimizeRules [] = []
optimizeRules (rule : rules) =
  let go pivot [] =
        [pivot]
      go pivot (x : ys) = case (pivot, x) of
        (RuleDockerV1 docker1@DockerRuleV1 {ruleBackendRegistry = ruleBackendRegistry1}, RuleDockerV1 docker2@DockerRuleV1 {ruleBackendRegistry = ruleBackendRegistry2})
          | docker1 == docker2 ->
              go pivot ys
          | ruleBackendRegistry1 == ruleBackendRegistry2 ->
              go
                ( RuleDockerV1
                    docker1
                      { ruleImages =
                          HashMap.union
                            (ruleImages docker2)
                            (ruleImages docker1)
                      }
                )
                ys
        (RulePythonV1 r1, RulePythonV1 r2) ->
          let mergeFiles :: Text -> PythonPackage -> PythonPackage -> PythonPackage
              mergeFiles packageName p1 p2 =
                let projectPageHTML =
                      pythonProjectPageHTML packageName package
                    package =
                      PythonPackage
                        { packageId = packageId p1,
                          packageFiles = HashMap.union (packageFiles p1) (packageFiles p2),
                          packageProjectPageHTML = projectPageHTML,
                          packageEtag = pythonPackageEtag package
                        }
                 in package

              mergePackages :: PythonRuleV1 -> PythonRuleV1 -> PythonRuleV1
              mergePackages p1 p2 =
                let rootRepoHTML =
                      pythonRootRepoHTML rule
                    rule =
                      PythonRuleV1
                        { rulePackages = HashMap.unionWithKey mergeFiles (rulePackages p1) (rulePackages p2),
                          ruleRootRepoHTML = rootRepoHTML,
                          ruleEtag = pythonRuleEtag rule
                        }
                 in rule
              !mergedPackages =
                mergePackages r1 r2
           in go (RulePythonV1 mergedPackages) ys
        _ ->
          pivot : go x ys
   in go rule rules

-- | Traverse the set of rules and matches the best applicable.
-- TODO We should probably return all the rules that matched to
-- report ambigouites.
match ::
  MonadMatch m =>
  [Rule] ->
  Wai.Request ->
  m (Maybe (Rule, RedirectOrProxy))
match rules waiRequest =
  let request :: Request
      request =
        newRequest waiRequest

      go :: [Rule] -> IO (Maybe (Rule, RedirectOrProxy))
      go [] =
        pure Nothing
      go (rule : rules) = do
        result <- matchRule rule request
        case result of
          Nothing -> go rules
          Just x -> pure $ Just (rule, x)
   in go rules

matchRule :: MonadMatch m => Rule -> Request -> m (Maybe RedirectOrProxy)
matchRule rule request = case rule of
  RuleDockerV1 dockerRule@DockerRuleV1 {ruleBackendRegistry} ->
    matchDocker (dockerImageMatcherV1 dockerRule) ruleBackendRegistry request
  RuleDockerV2 dockerRule@DockerRuleV2 {ruleBackendRegistry} ->
    matchDocker (dockerImageMatcherV2 dockerRule) ruleBackendRegistry request
  RuleFlatfile flatfileRule ->
    matchFlatfile flatfileRule request
  RuleFileV2 fileRuleV2 ->
    matchFileRuleV2 fileRuleV2 request
  RulePixel pixelRule ->
    matchPixel pixelRule request
  RulePythonV1 pythonRule ->
    matchPython pythonRule request
  RuleScarfJs ->
    matchScarfJsPackageEvent request

-- | Given an image name, determine whether the image is a match.
-- Returns Either PackageId AutoCreationRuleId.
type DockerImageMatcher = [Text] -> Maybe (Either Text Text)

-- | Matches an image specified in a 'DockerRuleV1'. Returns the package id
-- of the matched image.
dockerImageMatcherV1 :: DockerRuleV1 -> DockerImageMatcher
dockerImageMatcherV1 DockerRuleV1 {ruleImages} = \image ->
  case HashMap.lookup image ruleImages of
    Just packageId -> Just (Left packageId)
    Nothing -> Nothing

-- | Matches an image based on a 'DockerRuleV1'. Returns the package-auto-creation-rule
-- id.
dockerImageMatcherV2 :: DockerRuleV2 -> DockerImageMatcher
dockerImageMatcherV2 DockerRuleV2 {ruleImagePattern, ruleRuleId} = \image ->
  if ImagePattern.match ruleImagePattern image
    then Just (Right ruleRuleId)
    else Nothing

-- | Checks whether a 'Request' is a Docker request and determines
-- if the client supports redirects and fallbacks to proxying if not.
--
-- This is the Gateways core functionality, be careful when changing
-- this function.
--
-- See https://docs.docker.com/registry/spec/api/ for reference.
matchDocker ::
  MonadMatch m =>
  DockerImageMatcher ->
  -- | Backend registry
  ByteString ->
  Request ->
  m (Maybe RedirectOrProxy)
matchDocker matchImage backendRegistry Request {requestWai = request}
  | "/v2/_catalog" <- rawPathInfo request =
      pure $ Just (RespondNotFound emptyCapture)
  | "/v2/" <- rawPathInfo request =
      pure $ Just $ redirectOrProxy request backendRegistry emptyCapture
  | ("v2" : image, _manifestsOrBlobsOrTags : reference : _) <-
      break
        (\x -> x == "manifests" || x == "blobs" || x == "tags")
        (pathInfo request),
    Just result <- matchImage image =
      let !capture =
            DockerCapture
              { dockerCaptureImage = image,
                dockerCaptureBackendRegistry = Text.decodeUtf8 backendRegistry,
                dockerCaptureReference = reference,
                dockerCapturePackage = either Just (const Nothing) result,
                dockerCaptureAutoCreate = either (const Nothing) Just result
              }
       in pure $ Just $ redirectOrProxy request backendRegistry capture
  | otherwise =
      pure Nothing
  where
    emptyCapture =
      DockerCapture
        { dockerCaptureImage = [],
          dockerCaptureReference = "",
          dockerCaptureBackendRegistry = Text.decodeUtf8 backendRegistry,
          -- No package identified
          dockerCapturePackage = Nothing,
          -- Nothing to auto-create anyway
          dockerCaptureAutoCreate = Nothing
        }

-- | Quick and dirty heuristic whether the client supports redirects
-- or requires proxying.
redirectOrProxy :: Wai.Request -> ByteString -> RuleCapture -> RedirectOrProxy
redirectOrProxy request domain !capture
  | shouldRedirectDockerRequest request =
      -- As with the Host header we have to respect the proxy protocol
      -- when redirecting.
      let !absoluteUrl = "https://" <> domain <> rawPathInfo request
       in RedirectTo capture absoluteUrl
  | otherwise =
      ProxyTo (const capture) domain

shouldRedirectDockerRequest :: Wai.Request -> Bool
shouldRedirectDockerRequest request
  | Just userAgent <- requestHeaderUserAgent request =
      "docker/"
        `isPrefixOf` userAgent
        || "Docker-Client"
        `isInfixOf` userAgent
        || "containerd/1.4.9"
        `isInfixOf` userAgent
        || "containerd/1.5.5"
        `isInfixOf` userAgent
        || "containerd/1.6"
        `isInfixOf` userAgent
        || "Watchtower (Docker)"
        == userAgent
  | otherwise =
      False

matchFlatfile ::
  MonadMatch m =>
  FlatfileRule ->
  Request ->
  m (Maybe RedirectOrProxy)
matchFlatfile FlatfileRule {..} request@Request {requestPath, requestQueryString} =
  case URLTemplate.match rulePublicTemplate (requestPath <> requestQueryString) of
    xs@(_ : _)
      | let extracted = HashMap.fromList xs,
        HashSet.isSubsetOf ruleExpectedVariables (HashMap.keysSet extracted),
        Just !expanded <- URLTemplate.expand (`HashMap.lookup` extracted) ruleBackendTemplate -> do
          let !absoluteUrl = Text.encodeUtf8 expanded
          pure
            ( Just
                ( RedirectTo
                    ( FlatfileCapture
                        { filePackage = rulePackage,
                          fileAbsoluteUrl = absoluteUrl,
                          fileVariables = extracted
                        }
                    )
                    (Text.encodeUtf8 expanded)
                )
            )
    []
      -- This is the case where the backend doesn't have any variables, it's
      -- it's basically a plain old boring redirect and resolves around a
      -- string comparison of the url path.
      -- TODO would be good to be abe to distinguish no-match from match with
      -- novariables
      | HashSet.null ruleExpectedVariables,
        Just !ruleBackendExpanded <- URLTemplate.expand (\_ -> Nothing) ruleBackendTemplate,
        Just !rulePublicExpanded <- URLTemplate.expand (\_ -> Nothing) rulePublicTemplate,
        -- check that the publicPathRule matches our request including queryStrings if they are present
        pathAndQueryValidation rulePublicExpanded request -> do
          let !absoluteUrl = Text.encodeUtf8 ruleBackendExpanded
          pure
            ( Just
                ( RedirectTo
                    ( FlatfileCapture
                        { filePackage = rulePackage,
                          fileAbsoluteUrl = absoluteUrl,
                          fileVariables = HashMap.empty
                        }
                    )
                    absoluteUrl
                )
            )
    _ ->
      pure Nothing

matchFileRuleV2 :: MonadMatch m => FileRuleV2 -> Request -> m (Maybe RedirectOrProxy)
matchFileRuleV2 FileRuleV2 {..} Request {requestPath} =
  case Regex.match ruleIncomingPathRegex requestPath of
    Just xs
      | let extracted = HashMap.fromList xs,
        HashSet.isSubsetOf ruleExpectedVariables (HashMap.keysSet extracted),
        Just !expanded <- URLTemplate.expand (`HashMap.lookup` extracted) ruleBackendTemplate -> do
          let !absoluteUrl = Text.encodeUtf8 expanded
          pure
            ( Just
                ( RedirectTo
                    ( FlatfileCapture
                        { filePackage = rulePackage,
                          fileAbsoluteUrl = absoluteUrl,
                          fileVariables = extracted
                        }
                    )
                    (Text.encodeUtf8 expanded)
                )
            )

      -- This is the case where the backend doesn't have any variables, it's
      -- it's basically a plain old boring redirect and resolves around a
      -- string comparison of the url path.
      | HashSet.null ruleExpectedVariables,
        Just !ruleBackendExpanded <- URLTemplate.expand (\_ -> Nothing) ruleBackendTemplate -> do
          let !absoluteUrl = Text.encodeUtf8 ruleBackendExpanded
          pure
            ( Just
                ( RedirectTo
                    ( FlatfileCapture
                        { filePackage = rulePackage,
                          fileAbsoluteUrl = absoluteUrl,
                          fileVariables = HashMap.empty
                        }
                    )
                    absoluteUrl
                )
            )
      | otherwise ->
          pure Nothing
    Nothing ->
      pure Nothing

pathAndQueryValidation :: Text -> Request -> Bool
pathAndQueryValidation rulePublic Request {requestPath, requestQueryString} = do
  -- parse rulePublic to get access to it's queries and path
  case parseRelativeReference $ Text.unpack rulePublic of
    Nothing -> False
    Just publicUrl -> do
      let publicPath = uriPath publicUrl
          publicQuery = uriQuery publicUrl
      -- check that publicPath is equal to request path
      if publicPath == requestPath
        then -- if both public and request don't have queries then public path and request path match so return true
        case null publicQuery && null requestQueryString of
          True -> True
          False -> do
            -- now check that both queries match
            let parsedPublic = parseQuery $ BS.pack publicQuery
                parsedRequest = parseQuery $ BS.pack requestQueryString
            doQueriesMatch parsedPublic parsedRequest
        else False

-- convert queries into hashmaps and check that they match
doQueriesMatch :: Query -> Query -> Bool
doQueriesMatch publicQuery requestQuery = do
  let publicQueryMap = HashMap.fromList publicQuery
      requestQueryMap = HashMap.fromList requestQuery
      intersectionQueries =
        HashMap.differenceWith
          (\publicVal requestVal -> if publicVal == requestVal then Nothing else Just publicVal)
          publicQueryMap
          requestQueryMap
  null intersectionQueries

-- | Match a pixel tracking request.
matchPixel ::
  MonadMatch m =>
  PixelRule ->
  Request ->
  m (Maybe RedirectOrProxy)
matchPixel PixelRule {..} Request {requestWai}
  | "GET" == requestMethod requestWai,
    "a.png" : _ <- pathInfo requestWai = do
      -- N.B. Strict to avoid returning thunks.
      let !capture =
            case lookup "x-pxid" (parseQueryText (rawQueryString requestWai)) of
              Just (Just pixelId) ->
                PixelCapture pixelId
              _ ->
                PixelCapture ""
      pure $
        Just
          ( RespondBytes
              capture
              ( ResponseHeaders
                  { contentType = ruleTransparentImageContentType,
                    contentLength = Just (fromIntegral (LBS.length ruleTransparentImage)),
                    etag = Nothing,
                    cacheControl = Just "no-cache, no-store, must-revalidate"
                  }
              )
              ruleTransparentImage
          )
  | otherwise =
      pure Nothing

-- | Etag matching for caching of Python Simple index. This allows clients (e.g. pip) to
-- avoid downloading the whole index again.
ifNoneMatch ::
  -- | We are looking for the If-None-Match header in the request
  Wai.Request ->
  -- | Etag for the requested resource
  ByteString ->
  -- | Value to return in case one of the Etags If-None-Match header
  -- matches
  a ->
  -- | Value to return if they don't match. Also returned in case the If-None-Match header
  -- is not present
  a ->
  a
ifNoneMatch request etag match noMatch
  | Just value <- lookup "If-None-Match" (Wai.requestHeaders request),
    let etags = map (unquoute . BS.strip) (BS.split ',' value),
    etag `elem` etags =
      match
  | otherwise =
      noMatch
  where
    unquoute xs
      | BS.length xs > 1,
        BS.head xs == '\"',
        BS.last xs == '\"' =
          BS.tail (BS.init xs)
      | otherwise =
          xs

-- | Match a python package request
matchPython ::
  MonadMatch m =>
  PythonRuleV1 ->
  Request ->
  m (Maybe RedirectOrProxy)
matchPython PythonRuleV1 {..} Request {requestWai = request}
  | not isHead && not isGet =
      pure Nothing
  | ["simple", ""] <- pathInfo request,
    let !capture =
          PythonCapture
            { pythonCapturePackage = Nothing,
              pythonCaptureVersion = Nothing,
              pythonCaptureFileName = Nothing,
              pythonCaptureFileBackendURL = Nothing
            } =
      pure $
        Just $!
          ifNoneMatch
            request
            ruleEtag
            ( RespondNotModified
                capture
                ruleEtag
            )
            ( RespondBytes
                capture
                ( ResponseHeaders
                    { contentType = "text/html",
                      contentLength = Just (fromIntegral (LBS.length ruleRootRepoHTML)),
                      etag = Just ruleEtag,
                      cacheControl = Nothing
                    }
                )
                (if isHead then mempty else ruleRootRepoHTML)
            )
  | ["simple", ruleName, ""] <- pathInfo request,
    Just PythonPackage {..} <- HashMap.lookup ruleName rulePackages,
    let !capture =
          PythonCapture
            { pythonCapturePackage = Just packageId,
              pythonCaptureVersion = Nothing,
              pythonCaptureFileName = Nothing,
              pythonCaptureFileBackendURL = Nothing
            } =
      pure $
        Just $!
          ifNoneMatch
            request
            packageEtag
            ( RespondNotModified
                capture
                packageEtag
            )
            ( RespondBytes
                capture
                ( ResponseHeaders
                    { contentType = "text/html",
                      contentLength = Just (fromIntegral (LBS.length packageProjectPageHTML)),
                      etag = Just packageEtag,
                      cacheControl = Nothing
                    }
                )
                (if isHead then mempty else packageProjectPageHTML)
            )
  | "GET" <- requestMethod request,
    ["simple", packageName, fileName] <- pathInfo request,
    Just PythonPackage {..} <- HashMap.lookup packageName rulePackages,
    Just PythonFile {..} <- HashMap.lookup fileName packageFiles,
    let !capture =
          PythonCapture
            { pythonCapturePackage = Just packageId,
              pythonCaptureVersion = Just fileVersion,
              pythonCaptureFileName = Just fileName,
              pythonCaptureFileBackendURL = Just fileBackendURL
            } =
      pure $ Just $ RedirectTo capture $ Text.encodeUtf8 fileBackendURL
  | otherwise =
      pure Nothing
  where
    isGet = requestMethod request == "GET"
    isHead = requestMethod request == "HEAD"

-- | Match a Scarfjs package install
matchScarfJsPackageEvent ::
  MonadMatch m =>
  Request ->
  m (Maybe RedirectOrProxy)
matchScarfJsPackageEvent Request {requestWai = request}
  | "package-event" : "install" : rest <- pathInfo request = do
      body <- parseBody
      case body of
        Nothing ->
          pure (Just RespondInvalidRequest)
        Just _value
          | "POST" /= requestMethod request ->
              pure (Just RespondMethodNotAllowed)
        Just value ->
          let !capture =
                ScarfJsCapture
                  { scarfJsRequestBody = value,
                    scarfJsPackage = case rest of
                      [] -> Nothing
                      packageId : _ -> Just packageId
                  }
           in pure
                ( Just
                    ( RespondBytes
                        capture
                        ( ResponseHeaders
                            { contentType = "application/json",
                              contentLength = Nothing,
                              etag = Nothing,
                              cacheControl = Nothing
                            }
                        )
                        mempty
                    )
                )
  | otherwise =
      pure Nothing
  where
    parseBody = do
      result <- Attoparsec.parseWith (getRequestBodyChunk request) Aeson.value' mempty
      pure (Attoparsec.maybeResult result)
