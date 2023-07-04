{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scarf.Manifest
  ( Domain,
    Manifest (..),
    ManifestRule (..),
    PythonFileHashV1 (..),
    emptyManifest,
    encodeManifest,
    decodeManifest,
    manifestToRules,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (Null),
    decode',
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (second)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Scarf.Gateway.ImagePattern qualified as ImagePattern
import Scarf.Gateway.Regex (Regex)
import Scarf.Gateway.Rule
  ( Rule,
    newDockerRuleV1,
    newDockerRuleV2,
    newFileRuleV2,
    newFlatfileRule,
    newPythonRule,
    optimizeRules,
  )
import Scarf.Gateway.URLTemplate (URLTemplate)
import Scarf.Gateway.URLTemplate qualified as URLTemplate

-- | This is an interface type. Changes must be backward compatible.
type Domain = Text

-- | This is an interface type. Changes must be backward compatible.
-- Note that strict determinism is a requirement for proper caching,
-- don't use HashMaps or similar data structures.
newtype Manifest = Manifest
  { manifestRules :: [ManifestRule]
  }
  deriving (Eq, Show)

-- | This is an interface type. Changes must be backward compatible.
-- Note that strict determinism is a requirement for proper caching,
-- don't use HashMaps or similar data structures.
data ManifestRule
  = ManifestDockerRuleV1
      { -- | Package name
        manifestRulePackageName :: !Text,
        -- | Owner of the package
        manifestRuleOwner :: !Text,
        -- | Package id for this Docker container.
        -- This will be the unique identifier for this docker container.
        manifestRulePackageId :: !Text,
        -- | e.g. @test.docker.scarf.sh@
        manifestRuleDomain :: !Domain,
        -- | e.g. @"library/hello-world"@
        manifestRuleRepositoryName :: !Text,
        -- | e.g. @docker.io@, @ghcr.io@
        manifestRuleBackendRegistry :: !Text
      }
  | ManifestDockerRuleV2
      { -- | Owner of the package
        manifestRuleOwner :: !Text,
        -- | e.g. @test.docker.scarf.sh@
        manifestRuleDomain :: !Domain,
        -- | Path segment patterns. e.g. @library/*@.
        manifestRulePattern :: !ImagePattern.Pattern,
        -- | Id of the corresponding rule in the database
        manifestRuleId :: !Text,
        -- | e.g. @docker.io@
        manifestRuleBackendRegistry :: !Text
      }
  | ManifestFileRuleV1
      { -- | Package name
        manifestRulePackageName :: !Text,
        -- | Owner of the package
        manifestRuleOwner :: !Text,
        -- | e.g. @test.docker.scarf.sh@
        manifestRuleDomain :: !Domain,
        -- | e.g. @/minikube-{platform}-{version}.tar.gz@
        manifestRuleIncomingPath :: !URLTemplate,
        -- | e.g. @https://github.com/kubernetes/minikube/releases/@
        -- @downloads/minikube-{platform}-{version}.tar.gz@
        manifestRuleOutgoingURL :: !(Maybe URLTemplate),
        -- | Package id this file belongs to
        -- This will be the unique identifier for this file package.
        manifestRulePackageId :: !Text
      }
  | ManifestFileRuleV2
      { -- | Package name
        manifestRulePackageName :: !Text,
        -- | Owner of the package
        manifestRuleOwner :: !Text,
        -- | e.g. cr.l5d.io
        manifestRuleDomain :: !Domain,
        -- | e.g. /minikube-{platform}-{version}.tar.gz
        manifestRuleIncomingPathRegex :: !Regex,
        -- | e.g. https://github.com/kubernetes/minikube/releases/
        -- downloads/minikube-{platform}-{version}.tar.gz
        manifestRuleOutgoingURL :: !(Maybe URLTemplate),
        -- | Package id this file belongs to
        manifestRulePackageId :: !Text
      }
  | ManifestPythonRuleV1
      { -- | Owner of the package
        manifestRuleOwner :: !Text,
        -- | e.g. @cr.l5d.io@
        manifestRuleDomain :: !Domain,
        -- | e.g. @aca@
        manifestRulePackageName :: !Text,
        -- | e.g. @aca-0.9.tar.gz@
        manifestRuleFileName :: !Text,
        -- | e.g. @1.0@
        manifestRuleVersion :: !Text,
        -- | e.g. @(md5, 123...)@
        manifestRuleHashValue :: !(Maybe PythonFileHashV1),
        -- | A value of either true or false to indicate whether or not there is a GPG signature.
        manifestRuleGPGSig :: !(Maybe Bool),
        -- | The Python version(s) that the distribution is guaranteed to be compatible with.
        -- The value follows the PEP 345 spec.
        manifestRuleRequiresPython :: !(Maybe Text),
        -- | The backend URL to download the package.
        manifestRuleBackendURL :: !Text,
        -- | Package Id.
        -- This will be the unique identifier for this python package.
        manifestRulePackageId :: !Text,
        -- | Manifest backend index. If not present, assume https://pypi.org/simple/
        manifestRuleBackendSimpleIndex :: !(Maybe Text)
      }
  deriving (Eq, Show)

data PythonFileHashV1 = PythonFileHashV1
  { -- | Any of the supported hash functions supported by Python's hashlib.
    -- (currently md5, sha1, sha224, sha256, sha384, sha512).
    fileHashType :: !Text,
    -- | Hash, lower case, encoded as hexadecimal. e.g. 68b329da9893e34099c7d8ad5cb9c940
    fileHash :: !Text
  }
  deriving (Eq, Show)

instance ToJSON PythonFileHashV1 where
  toJSON PythonFileHashV1 {..} =
    object
      [ "type" .= fileHashType,
        "hash" .= fileHash
      ]

instance FromJSON PythonFileHashV1 where
  parseJSON = withObject "PythonFileHashV1" $ \o ->
    PythonFileHashV1
      <$> o .: "type"
      <*> o .: "hash"

instance ToJSON Manifest where
  toJSON Manifest {..} =
    object
      [ "rules" .= manifestRules
      ]

instance FromJSON Manifest where
  parseJSON = withObject "Manifest" $ \o ->
    Manifest
      <$> o .: "rules"

instance FromJSON ManifestRule where
  parseJSON = withObject "ManifestRule" $ \o -> do
    _type <- o .: "type"
    case _type :: Text of
      "docker-v1" ->
        ManifestDockerRuleV1
          <$> o .:? "package-name" .!= ""
          <*> o .:? "owner" .!= ""
          <*> o .: "package-id"
          <*> o .: "domain"
          <*> o .: "repository-name"
          <*> o .: "registry"
      "docker-v2" ->
        ManifestDockerRuleV2
          <$> o .:? "owner" .!= ""
          <*> o .: "domain"
          <*> o .: "pattern"
          <*> o .: "rule-id"
          <*> o .: "registry"
      "file-v1" ->
        ManifestFileRuleV1
          <$> o .:? "package-name" .!= ""
          <*> o .:? "owner" .!= ""
          <*> o .: "domain"
          <*> o .: "incoming-path"
          <*> o .: "outgoing-url"
          <*> o .: "package-id"
      "file-v2" ->
        ManifestFileRuleV2
          <$> o .:? "package-name" .!= ""
          <*> o .:? "owner" .!= ""
          <*> o .: "domain"
          <*> o .: "incoming-path"
          <*> o .: "outgoing-url"
          <*> o .: "package-id"
      "python-v1" ->
        ManifestPythonRuleV1
          <$> o .:? "owner" .!= ""
          <*> o .: "domain"
          <*> o .: "package-name"
          <*> o .: "file-name"
          <*> o .: "version"
          <*> o .:? "hash-value"
          <*> o .:? "gpg-sig"
          <*> o .:? "requires-python"
          <*> o .: "backend-url"
          <*> o .: "package-id"
          <*> o .:? "backend-simple-index-url"
      _ ->
        fail "invalid manifest rule type"

instance ToJSON ManifestRule where
  toJSON ManifestDockerRuleV1 {..} =
    object $
      dropNull
        [ "type" .= ("docker-v1" :: Text),
          "package-name" .= manifestRulePackageName,
          "owner" .= manifestRuleOwner,
          "package-id" .= manifestRulePackageId,
          "domain" .= manifestRuleDomain,
          "repository-name" .= manifestRuleRepositoryName,
          "registry" .= manifestRuleBackendRegistry
        ]
  toJSON ManifestDockerRuleV2 {..} =
    object $
      dropNull
        [ "type" .= ("docker-v2" :: Text),
          "owner" .= manifestRuleOwner,
          "domain" .= manifestRuleDomain,
          "registry" .= manifestRuleBackendRegistry,
          "pattern" .= manifestRulePattern,
          "rule-id" .= manifestRuleId
        ]
  toJSON ManifestFileRuleV1 {..} =
    object $
      dropNull
        [ "type" .= ("file-v1" :: Text),
          "package-name" .= manifestRulePackageName,
          "owner" .= manifestRuleOwner,
          "domain" .= manifestRuleDomain,
          "incoming-path" .= manifestRuleIncomingPath,
          "outgoing-url" .= manifestRuleOutgoingURL,
          "package-id" .= manifestRulePackageId
        ]
  toJSON ManifestFileRuleV2 {..} =
    object $
      dropNull
        [ "type" .= ("file-v2" :: Text),
          "package-name" .= manifestRulePackageName,
          "owner" .= manifestRuleOwner,
          "domain" .= manifestRuleDomain,
          "incoming-path" .= manifestRuleIncomingPathRegex,
          "outgoing-url" .= manifestRuleOutgoingURL,
          "package-id" .= manifestRulePackageId
        ]
  toJSON ManifestPythonRuleV1 {..} =
    object $
      dropNull
        [ "type" .= ("python-v1" :: Text),
          "owner" .= manifestRuleOwner,
          "domain" .= manifestRuleDomain,
          "package-name" .= manifestRulePackageName,
          "file-name" .= manifestRuleFileName,
          "version" .= manifestRuleVersion,
          "hash-value" .= manifestRuleHashValue,
          "gpg-sig" .= manifestRuleGPGSig,
          "requires-python" .= manifestRuleRequiresPython,
          "backend-url" .= manifestRuleBackendURL,
          "package-id" .= manifestRulePackageId,
          "backend-simple-index-url" .= manifestRuleBackendSimpleIndex
        ]

dropNull :: [(a, Value)] -> [(a, Value)]
dropNull = filter $ \(_, x) -> case x of
  Null -> False
  _ -> True

emptyManifest :: Manifest
emptyManifest =
  Manifest
    { manifestRules = []
    }

decodeManifest :: ByteString -> Maybe Manifest
decodeManifest = decode'

encodeManifest :: Manifest -> ByteString
encodeManifest = encodePretty

-- | Convert the interface type 'Manifest' to the Gateway's internal 'Rule' type.
manifestToRules :: Manifest -> [(Domain, [Rule])]
manifestToRules Manifest {..} =
  map (second optimizeRules) $
    HashMap.toList $
      HashMap.fromListWith
        (++)
        [ (manifestRuleDomain rule, [manifestRuleToRule rule])
          | rule <- manifestRules
        ]

-- | Convert the interface type 'ManifestRule' to the Gateway's internal 'Rule' type.
manifestRuleToRule :: ManifestRule -> Rule
manifestRuleToRule manifestRule = case manifestRule of
  ManifestDockerRuleV1 {..} ->
    newDockerRuleV1
      manifestRulePackageId
      (Text.splitOn "/" manifestRuleRepositoryName)
      manifestRuleBackendRegistry
  ManifestDockerRuleV2 {..} ->
    newDockerRuleV2
      manifestRuleId
      (ImagePattern.toText manifestRulePattern)
      manifestRuleBackendRegistry
  ManifestFileRuleV1 {..} ->
    newFlatfileRule
      manifestRulePackageId
      (URLTemplate.toText manifestRuleIncomingPath)
      (URLTemplate.toText <$> manifestRuleOutgoingURL)
  ManifestFileRuleV2 {..} ->
    newFileRuleV2
      manifestRulePackageId
      manifestRuleIncomingPathRegex
      manifestRuleOutgoingURL
  ManifestPythonRuleV1 {..} ->
    newPythonRule
      manifestRulePackageName
      manifestRuleFileName
      manifestRuleVersion
      ( case manifestRuleHashValue of
          Just PythonFileHashV1 {..} ->
            Just (fileHashType, fileHash)
          Nothing ->
            Nothing
      )
      manifestRuleGPGSig
      manifestRuleRequiresPython
      manifestRuleBackendURL
      manifestRulePackageId
