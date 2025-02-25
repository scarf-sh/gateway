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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Scarf.Gateway.Rule
  ( Rule,
    optimizeRules,
    sortRules,
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
fetchDomainMapping :: (MonadIO m) => ActiveSpan -> SyncConfig -> GetIfChanged f -> m (f DomainLookup)
fetchDomainMapping span SyncConfig {..} getIfChanged = do
  let buildDomainMapping domainRules =
        let toDomain domain =
              case split ':' domain of
                domain : _ -> domain
                [] -> error "impossible"

            mapping =
              HashMap.fromListWith
                (++)
                [ -- Domains are case insensitive. We normalize all domains to lower case.
                  (Text.toLower domain, optimizeRules $ sortRules rules)
                  | (domain, rules) <- domainRules
                ]
         in \domain ->
              case HashMap.lookup (Text.toLower (Text.decodeUtf8 (toDomain domain))) mapping of
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
  (MonadUnliftIO m) =>
  ActiveSpan ->
  Tracer ->
  SyncConfig ->
  (IO Bool -> (forall m. (MonadIO m) => m DomainLookup) -> m r) ->
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
