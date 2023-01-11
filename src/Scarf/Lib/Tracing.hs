{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scarf.Lib.Tracing
  ( withRootTracer,
    traceWaiApplication,
    MonadTracer,
    nullTracer,

    -- * Tracing functions
    tracedE_,
    getSpanContext,
    module Reexports,
    followsFromActive,
    tracedConduit_,
    tracedConduit,
    traced_',
  )
where

import Conduit
  ( ConduitT,
    ResourceT,
    bracketP,
  )
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (newIORef, atomicWriteIORef, readIORef)
import Control.Exception (BlockedIndefinitelyOnSTM, bracket, catch)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Data.Text (pack, splitOn, unpack)
import Text.Read (readMaybe)
import Data.Text qualified as Text
import Lens.Micro.Platform (view, (.~), (^.), (&))
import Network.HTTP.Client (Manager)
import Network.Wai (Application, pathInfo, requestMethod)
import Network.Wai.Middleware.OpenTracing (withOperationName)
import OpenTracing
  ( Addr (UDPAddr),
    Tracer (..),
    constSampler,
    probSampler,
    runSampler,
  )
import OpenTracing as Reexports
  ( ActiveSpan,
    FinishedSpan,
    HasSpanFields (..),
    HasTracer (..),
    LogField (..),
    Reference (..),
    Span,
    SpanContext (..),
    SpanOpts,
    SpanRefs,
    Tag,
    TagVal (..),
    Tags,
    Traced (..),
    Tracer,
    addLogRecord,
    addLogRecord',
    addTag,
    childOf,
    extract,
    followsFrom,
    readActiveSpan,
    refPropagated,
    runOpenTracing,
    runTracer,
    spanOpts,
    traced,
    traced_,
    pattern DbUser,
    pattern Error,
  )
import OpenTracing qualified (MonadTracer)
import OpenTracing.Jaeger.AgentReporter
  ( jaegerAgentOptions,
    jaegerAgentReporter,
    jaegerPropagation,
    jaoAddr,
    withJaegerAgent,
  )
import Data.Text.Encoding (decodeUtf8)
import OpenTracing.Reporting.Batch
  ( batchOptions,
    batchReporter,
    closeBatchEnv,
    newBatchEnv,
  )
import OpenTracing.Reporting.Pure (noReporter)
import OpenTracing.Reporting.Stdio (stderrReporter)
import OpenTracing.Standard (newStdEnv, stdTracer)
import OpenTracing.Tracer qualified as Tracer
  ( finishSpan,
    startSpan,
    traced_,
  )
import Data.Foldable (traverse_)
import OpenTracing.Zipkin.V2
  ( Endpoint (..),
    closeZipkin,
    newZipkin,
    zipkinHttpReporter,
    zipkinOptions,
    zoEndpoint,
  )
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
-- import Prelude hiding (lookupEnv)

type MonadTracer r m = (OpenTracing.MonadTracer r m, MonadIO m, MonadMask m)

-- | Start new `Span`s over incoming requests.
traceWaiApplication ::
  Tracer ->
  (ActiveSpan -> Application) ->
  Application
traceWaiApplication tracer tracedApp =
  withOperationName tracer jaegerPropagation operationName tracedApp
  where
    operationName request =
      decodeUtf8 (requestMethod request)
        <> " "
        <> Text.cons '/' (Text.intercalate "/" (pathInfo request))

-- | Create a new `Tracer` with the given sampling rate.
withRootTracer ::
  -- | 'Manager' for pushing traces via HTTP.
  -- | Sampling rate with which spans are emitted.
  --
  -- @
  --   Nothing <=> Every span is sampled
  --   Just d  <=> Trace d percent of emitted spans (where d is in [0.0 .. 1.0])
  -- @
  Manager ->
  Maybe Double ->
  -- | Action that should be traced.
  (Tracer -> IO a) ->
  IO a
withRootTracer manager msamplingRate action = do
  samplingRate <- maybe msamplingRate readMaybe <$> lookupEnv "TRACING_SAMPLING_RATE"
  serviceName <- maybe "unspecified service" pack <$> lookupEnv "TRACING_SERVICE_NAME"
  serviceAddress <- maybe (read "0.0.0.0") read <$> lookupEnv "TRACING_SERVICE_ADDR"

  -- STDIO is on by default
  enableStdioReporter <- (maybe True ("False" /=)) <$> lookupEnv "TRACING_STDIO"
  zipkinEndpoint <- lookupEnv "TRACING_ZIPKIN_ENDPOINT"
  jaegerEndpoint <- lookupEnv "TRACING_JAEGER_ENDPOINT"

  let sampler = case samplingRate of
        Just rate
          | rate == 1.0 -> constSampler True
          | rate == 0 -> constSampler False
          | otherwise -> probSampler rate
        _ -> constSampler True

      endpoint =
        Endpoint
          { serviceName,
            ipv4 = serviceAddress,
            ipv6 = Nothing,
            port = Nothing
          }

      withStdioReporter :: ((FinishedSpan -> IO ()) -> IO a) -> IO a
      withStdioReporter action
        | enableStdioReporter =
            action stderrReporter
        | otherwise =
            action (\_ -> pure ())

      withZipkinReporter :: ((FinishedSpan -> IO ()) -> IO a) -> IO a
      withZipkinReporter action
        | Just pushEndpoint <- zipkinEndpoint,
          not (null pushEndpoint) = do
            let options =
                  zipkinOptions manager endpoint
                    & zoEndpoint .~ pushEndpoint

                close zipkin =
                  closeZipkin zipkin `catch` \(_ :: BlockedIndefinitelyOnSTM) ->
                    -- FIXME: There seems to be a bug in the exception handling of
                    -- the opentracing library that crashes with a blocked
                    -- STM exception when shutting down. In the meantime we
                    -- silence the exception. It's on our way way out so it's
                    -- not super critical.
                    pure ()

            bracket (newZipkin options) close $ \zipkin ->
              action (zipkinHttpReporter zipkin)
        | otherwise =
            action (\_ -> pure ())

      withJaegerReporter :: ((FinishedSpan -> IO ()) -> IO a) -> IO a
      withJaegerReporter action
        | Just pushEndpoint <- jaegerEndpoint,
          not (null pushEndpoint),
          [hostname, port'] <- splitOn ":" (pack pushEndpoint),
          Just port <- readMaybe (unpack port') = do
            let options =
                  jaegerAgentOptions serviceName
                    & jaoAddr .~ UDPAddr (unpack hostname) port
            withJaegerAgent options $ \agent -> do
              let batchOpts =
                    batchOptions (traverse_ (jaegerAgentReporter agent))

                  close batch =
                    closeBatchEnv batch `catch` \(_ :: BlockedIndefinitelyOnSTM) ->
                      -- FIXME: There seems to be a bug in the exception handling of
                      -- the opentracing library that crashes with a blocked
                      -- STM exception when shutting down. In the meantime we
                      -- silence the exception. It's on our way way out so it's
                      -- not super critical.
                      pure ()

              bracket (newBatchEnv batchOpts) close $ \batchEnv ->
                action (batchReporter batchEnv)
        | otherwise =
            action (\_ -> pure ())

  withStdioReporter $ \stdioReporter ->
    withZipkinReporter $ \zipkinReporter -> do
      withJaegerReporter $ \jaegerReporter -> do
        -- We are rightfully paranoid. We let the library emit every Span and capture
        -- everything in plain old logs but do sample for our high-level tooling.
        env <- newStdEnv (constSampler True)
        let tracer =
              Tracer
                { tracerStart = stdTracer env,
                  tracerReport = \span -> liftIO $ do
                    -- Output to logs unconditionally
                    stdioReporter span

                    -- This is where we sample for our high-level tooling
                    let ctx = span ^. spanContext
                    sampled <- runSampler sampler (ctxTraceID ctx) (span ^. spanOperation)
                    when sampled $ do
                      zipkinReporter span
                      jaegerReporter span
                }
        action tracer

getSpanContext :: MonadIO m => ActiveSpan -> m SpanContext
getSpanContext span =
  view spanContext <$> readActiveSpan span

nullTracer :: Tracer
nullTracer =
  Tracer
    { tracerStart = stdTracer env,
      tracerReport = noReporter
    }
  where
    env =
      unsafePerformIO $
        newStdEnv (constSampler False)

-- | Variant of traced_ that logs errors.
tracedE_ ::
  ( MonadIO m,
    MonadMask m,
    Show e,
    MonadTracer r m
  ) =>
  SpanOpts ->
  (ActiveSpan -> m (Either e a)) ->
  m (Either e a)
tracedE_ opts action = traced_ opts $ \span -> do
  result <- action span
  case result of
    Left err -> do
      addTag span (Error True)
      addLogRecord span (LogField "error.object" err)
      return result
    _ ->
      return result

followsFromActive :: (MonadIO m) => ActiveSpan -> m SpanRefs
followsFromActive span = do
  span' <- readActiveSpan span
  pure $ (mempty & refPropagated .~ [FollowsFrom $ span' ^. spanContext])

tracedConduit ::
  ( HasTracer t,
    MonadIO m
  ) =>
  t ->
  SpanOpts ->
  (ActiveSpan -> ConduitT i o (ResourceT m) r') ->
  ConduitT i o (ResourceT m) (Traced r')
tracedConduit t opt go = do
  fin <- liftIO $ newIORef Nothing
  res <-
    bracketP
      (Tracer.startSpan t opt)
      (\span -> Tracer.finishSpan t span >>= atomicWriteIORef fin . Just)
      go
  liftIO (readIORef fin) >>= \case
    Just fin' -> pure $ Traced {tracedResult = res, tracedSpan = fin'}
    Nothing -> error "bracketP didn't call finalizer!"

tracedConduit_ ::
  ( HasTracer t,
    MonadIO m
  ) =>
  t ->
  SpanOpts ->
  (ActiveSpan -> ConduitT i o (ResourceT m) r') ->
  ConduitT i o (ResourceT m) r'
tracedConduit_ t opt go =
  tracedResult <$> tracedConduit t opt go

traced_' :: (MonadIO m, MonadMask m) => Tracer -> SpanOpts -> (ActiveSpan -> m a) -> m a
traced_' = Tracer.traced_
