{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scarf.Gateway.Sync
  ( Domain,
    SyncConfig (..),
    DomainLookup,
    GetIfChanged (..),
    Changed (..),
    withBackgroundSync,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad (forever)
import Control.Monad.Catch (catchAll)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
-- import Control.Monad.Trans.AWS
--   ( endpointHost,
--     endpointPort,
--     endpointSecure,
--     runAWST,
--     send,
--     serviceEndpoint,
--   )
import Control.Retry (recoverAll, retryPolicyDefault)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (split)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef,
  )
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
-- import Network.AWS.Auth (Credentials (..))
-- import Network.AWS.Data (toText)
-- import Network.AWS.Data.Body (toBody, _streamBody)
-- import Network.AWS.Env (Env, newEnvWith, override)
-- import Network.AWS.Prelude (serviceCheck, statusCode)
-- import Network.AWS.Response (sinkLBS)
-- import Network.AWS.S3.GetObject
--   ( getObject,
--     goIfNoneMatch,
--     gorsBody,
--     gorsETag,
--     gorsResponseStatus,
--   )
-- import Network.AWS.S3.PutObject (putObject)
-- import Network.AWS.S3.Types (BucketName (..), ObjectKey (..))
import Scarf.Gateway.Rule
  ( Rule,
    optimizeRules,
  )
import Scarf.Lib.Tracing
  ( ActiveSpan,
    TagVal (BoolT),
    Tracer,
    addTag,
    runTracer,
    spanOpts,
    traced_,
  )

-- | A simple datatype tracking whether the rule set changed.
data Changed a
  = Changed a
  | Unchanged
  deriving (Functor)

-- | Query-mode for accessing the manifest.
data GetIfChanged f where
  -- | Always fetch the manifest, regardless whether it has changed
  -- or not.
  AlwaysGet :: GetIfChanged Identity
  -- | Only fetch the manifest if it has changed relative to the last
  -- time we fetched it.
  IfChanged :: GetIfChanged Changed

-- | Convenience type synonym that represents a domain like
-- scarftest.docker.scarf.sh
type Domain = Text

-- | Configuration for the background sync.
data SyncConfig = SyncConfig
  { -- | Interval in which we re-fetch the mapping.
    syncConfigInterval :: Int,
    -- | Where to get the mapping from.
    syncConfigSource :: forall f. ActiveSpan -> GetIfChanged f -> IO (f [(Domain, [Rule])])
  }

-- | Looks up the domains the `Rule`s for a domain.
type DomainLookup =
  -- | Value from Host-header
  ByteString ->
  -- | Set of rules for this domain
  [Rule]

-- | This function goes out to Redis and fetches the info
-- necessary to resolve backend urls for Docker and Flatfile
-- packages.
fetchDomainMapping :: MonadIO m => ActiveSpan -> SyncConfig -> GetIfChanged f -> m (f DomainLookup)
fetchDomainMapping span SyncConfig {..} getIfChanged = do
  let buildDomainMapping domainRules =
        let mapping =
              HashMap.fromListWith
                (++)
                [ -- Domains are case insensitive. We normalize all domains to lower case.
                  (Text.toLower domain, optimizeRules $ List.sort rules)
                  | (domain, rules) <- domainRules
                ]
         in \domain ->
              case HashMap.lookup (Text.toLower (Text.decodeUtf8 (head (split ':' domain)))) mapping of
                Nothing -> []
                Just rules -> rules
  case getIfChanged of
    AlwaysGet -> do
      fmap buildDomainMapping <$> liftIO (syncConfigSource span AlwaysGet)
    IfChanged ->
      fmap buildDomainMapping <$> liftIO (syncConfigSource span IfChanged)

-- | Spin up a background worker that ensures there is up-to-date mapping info
-- available at all times.
withBackgroundSync ::
  MonadUnliftIO m =>
  ActiveSpan ->
  Tracer ->
  SyncConfig ->
  (IO Bool -> (forall m. MonadIO m => m DomainLookup) -> m r) ->
  m r
withBackgroundSync span tracer syncConfig@SyncConfig {..} withLookup = do
  -- Fetch an initial mapping synchronously. This ensures we can
  -- continue once a mapping is present.
  Identity initialDomainMapping <- fetchDomainMapping span syncConfig AlwaysGet
  mappingRef <- liftIO $ newIORef initialDomainMapping
  healthyRef <- liftIO $ newIORef True

  let -- Inifinite loop that fetches the mapping every syncConfigInterval
      -- seconds.
      worker = forever $ do
        threadDelay (syncConfigInterval * 1000000) -- in seconds
        catchAll
          ( do
              -- Retry up to 5 times with a 50ms delay in between. In case
              -- we can't succesfullt read the Rules we mark the service
              -- unhealthy (but do not end this thread, it might recover)
              recoverAll retryPolicyDefault $ \_ -> do
                runTracer tracer $
                  traced_ (spanOpts "fetching-domain-mapping" mempty) $ \span -> liftIO $ do
                    changed <- fetchDomainMapping span syncConfig IfChanged
                    writeIORef healthyRef True
                    case changed of
                      Changed domainMapping -> do
                        addTag span ("modified", BoolT True)
                        writeIORef mappingRef domainMapping
                      Unchanged -> do
                        addTag span ("modified", BoolT False)
          )
          ( \_ -> do
              writeIORef healthyRef False
          )

  withRunInIO $ \runInIO -> do
    withAsync worker $ \_workerHandle -> do
      runInIO $ withLookup (readIORef healthyRef) (liftIO (readIORef mappingRef))

-- -- | Used to setup our mock S3 endpoint during integration tests.
-- setupS3MockEnvIfRequired :: Maybe Text -> Env -> Env
-- setupS3MockEnvIfRequired mockEndpoint env = case mockEndpoint of
--   Just endpoint
--     | not (Text.null endpoint) ->
--         override
--           ( \service ->
--               service
--                 -- don't use ssl for testing
--                 & serviceEndpoint . endpointSecure .~ False
--                 -- testing uses s3Endpoint only
--                 & serviceEndpoint . endpointHost .~ Text.encodeUtf8 endpoint
--                 -- testing endpoint uses port 80
--                 & serviceEndpoint . endpointPort .~ 80
--           )
--           env
--   _ -> env

-- -- | Uploads a 'Manifest' to S3 into the given bucket at the given object key.
-- putManifestIntoS3 ::
--   (MonadTracer r m) =>
--   ActiveSpan ->
--   Manager ->
--   -- | Mock S3 endpoint (for testing)
--   Maybe Text ->
--   -- | Bucket name
--   Text ->
--   -- | Object key
--   Text ->
--   Manifest ->
--   m ()
-- putManifestIntoS3 span manager mockS3Endpoint bucketName objectKey manifest =
--   traced_ (spanOpts "put-manifest-into-s3" (childOf span)) $ \span -> do
--     addTag span ("bucket-name", StringT bucketName)
--     addTag span ("object-key", StringT objectKey)
--     addTag span ("mock-s3-endpoint", StringT (fromMaybe "" mockS3Endpoint))

--     -- TODO pass credentials in args
--     env' <- newEnvWith Discover Nothing manager
--     let env =
--           setupS3MockEnvIfRequired mockS3Endpoint env'
--         manifestBytes =
--           encodeManifest manifest
--     liftIO $
--       runResourceT $ do
--         _ <- runAWST env $ do
--           send $ putObject (BucketName bucketName) (ObjectKey objectKey) (toBody manifestBytes)
--         pure ()

-- -- | Reads a manifest from S3 given a bucket and object key.
-- getManifestFromS3 ::
--   (MonadTracer r m) =>
--   ActiveSpan ->
--   Manager ->
--   -- | Mock S3 endpoint (for testing)
--   Maybe Text ->
--   -- | Bucket name
--   Text ->
--   -- | Object key
--   Text ->
--   -- | ETag
--   Maybe Text ->
--   -- | Determines whether to un/-conditionally get
--   -- the manifest
--   GetIfChanged f ->
--   m (Maybe Text, f Manifest)
-- getManifestFromS3 span manager mockS3Endpoint bucketName objectKey etag ifChanged =
--   traced_ (spanOpts "get-manifest-from-s3" (childOf span)) $ \span -> do
--     addTag span ("bucket-name", StringT bucketName)
--     addTag span ("object-key", StringT objectKey)
--     addTag span ("etag", StringT (fromMaybe "" etag))
--     addTag span ("mock-s3-endpoint", StringT (fromMaybe "" mockS3Endpoint))

--     -- TODO pass credentials in args
--     env' <- newEnvWith Discover Nothing manager
--     let env =
--           -- Handle 404 as a success too to handle missing manifest gracefully.
--           -- This situation arises regularly when testing.
--           override
--             ( \service ->
--                 service
--                   & serviceCheck
--                     .~ ( \status ->
--                            statusCode status >= 200 && statusCode status < 400 || statusCode status == 404
--                        )
--             )
--             (setupS3MockEnvIfRequired mockS3Endpoint env')

--         etag' = case ifChanged of
--           AlwaysGet -> Nothing
--           IfChanged -> etag
--     liftIO $
--       runResourceT $ do
--         runAWST env $ do
--           response <-
--             send $
--               getObject (BucketName bucketName) (ObjectKey objectKey)
--                 & goIfNoneMatch .~ etag'
--           let newEtag = fmap toText (response ^. gorsETag)
--           case (ifChanged, response ^. gorsResponseStatus) of
--             (_, 404) -> do
--               addTag span ("empty-manifest", BoolT True)
--               addTag span ("modified", BoolT True)
--               pure
--                 ( Nothing,
--                   case ifChanged of
--                     AlwaysGet -> Identity emptyManifest
--                     IfChanged -> Changed emptyManifest
--                 )
--             (IfChanged, 304) -> do
--               addTag span ("modified", BoolT False)
--               pure (newEtag, Unchanged)
--             _ -> do
--               addTag span ("modified", BoolT True)
--               bytes <- sinkLBS $ _streamBody (response ^. gorsBody)
--               case decodeManifest bytes of
--                 Just manifest ->
--                   pureƒ
--                     ( newEtag,
--                       case ifChanged of
--                         AlwaysGet -> Identity manifest
--                         IfChanged -> Changed manifest
--                     )
--                 Nothing ->
--                   -- TODO remove explicit error
--                   error "parsing manifest failed"
