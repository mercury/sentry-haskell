-- | OpenTelemetry plumbing for the checkout demo: an in-memory span exporter, a
-- 'SpanProcessor' that — on span end — reads the ambient Sentry scope and stamps
-- it onto the span as attributes, and 'withInMemoryTracer', which installs a
-- tracer around an action and hands back the harvested spans.
--
-- This is the same fixture used by the @sentry-optics@ checkout demo — it talks
-- only to the core "Sentry.Scope" API ('readAmbientScope', 'ScopeData'), so it
-- is shared verbatim between the optics and non-optics tours.
--
-- The Sentry scope lives in the OpenTelemetry ThreadLocal @Context@, so
-- 'Sentry.Scope.readAmbientScope' inside @spanProcessorOnEnd@ sees whatever
-- @withScope@ \/ the scope setters set on the current thread. Our processor
-- exports synchronously on that same thread, so the harvest always observes the
-- scope active where the span ended.
--
-- Writing attributes /at span end/ means mutating the span's @spanHot@
-- 'Data.IORef.IORef' directly (its accessors are re-exported from
-- "OpenTelemetry.Trace.Core"). The public attribute setters need a live,
-- unfinished @Span@, which can't be reconstructed from the @ImmutableSpan@ a
-- processor receives — so enriching strictly on end goes through @spanHot@, as a
-- real Sentry↔OTel processor does.
module Otel
  ( HarvestedSpan (..),
    withInMemoryTracer,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import OpenTelemetry.Attributes
  ( Attribute (..),
    Attributes,
    PrimitiveAttribute (..),
    addAttribute,
    defaultAttributeLimits,
    getAttributeMap,
  )
import OpenTelemetry.Common (optionalTimestampToMaybe)
import OpenTelemetry.Exporter.Span (ExportResult (..), SpanExporter (..))
import OpenTelemetry.Processor.Span (FlushResult (..), ShutdownResult (..), SpanProcessor (..))
import OpenTelemetry.Trace.Core
  ( ImmutableSpan (..),
    SpanContext (..),
    SpanHot (..),
    Tracer,
    createTracerProvider,
    emptyTracerProviderOptions,
    getSpanContext,
    getTracer,
    setGlobalTracerProvider,
    shutdownTracerProvider,
    timestampNanoseconds,
    tracerOptions,
  )
import Patrol.Type.User qualified as User
import Sentry.Scope (ScopeData (..), readAmbientScope)

-- | A harvested span: its name, wall-clock duration in nanoseconds, the
-- attributes it ended with, and its child spans (so a trace renders as a tree).
type HarvestedSpan :: Type
data HarvestedSpan = HarvestedSpan
  { name :: Text,
    durationNanos :: Integer,
    attributes :: [(Text, Text)],
    children :: [HarvestedSpan]
  }
  deriving stock (Show)

-- | Install a tracer backed by an in-memory exporter + the Sentry-scope
-- harvesting processor, run the action with that 'Tracer', then shut the
-- provider down (flushing) and return the harvested spans as a forest of
-- traces (each root is a top-level span).
withInMemoryTracer :: (Tracer -> IO a) -> IO (a, [HarvestedSpan])
withInMemoryTracer action = do
  (exporter, store) <- newInMemoryExporter
  provider <- createTracerProvider [sentryScopeProcessor exporter] emptyTracerProviderOptions
  setGlobalTracerProvider provider
  tracer <- getTracer provider "sentry-core-checkout" tracerOptions
  result <- action tracer
  _ <- shutdownTracerProvider provider Nothing
  spans <- readIORef store
  raw <- traverse collect spans
  pure (result, buildForest raw)
  where
    -- (own span id, parent span id, name, duration ns, attributes)
    collect immSpan = do
      hot <- readIORef immSpan.spanHot
      parent <- traverse getSpanContext immSpan.spanParent
      let started = timestampNanoseconds immSpan.spanStart
          ended = timestampNanoseconds <$> optionalTimestampToMaybe hot.hotEnd
          dur = maybe 0 (\e -> toInteger e - toInteger started) ended
      pure (immSpan.spanContext.spanId, spanId <$> parent, hot.hotName, dur, renderAttributes hot.hotAttributes)

    -- Roots are parentless spans; children match by parent span id. Sibling
    -- order follows the store (end order), which here matches start order.
    buildForest raw = [toNode r | r@(_, parent, _, _, _) <- raw, isNothing parent]
      where
        toNode (sid, _, nm, dur, attrs) =
          HarvestedSpan
            { name = nm,
              durationNanos = dur,
              attributes = attrs,
              children = [toNode c | c@(_, parent, _, _, _) <- raw, parent == Just sid]
            }

-- | A trivial 'SpanExporter' that accumulates received spans in an
-- 'Data.IORef.IORef', oldest (first-ended) first.
newInMemoryExporter :: IO (SpanExporter, IORef [ImmutableSpan])
newInMemoryExporter = do
  store <- newIORef []
  let exporter =
        SpanExporter
          { spanExporterExport = \batch -> do
              let spans = concatMap Vector.toList (HashMap.elems batch)
              modifyIORef' store (<> spans)
              pure Success,
            spanExporterShutdown = pure ShutdownSuccess,
            spanExporterForceFlush = pure FlushSuccess
          }
  pure (exporter, store)

-- | The demo processor: on span end, read the ambient Sentry scope, stamp it
-- onto the span, then forward the (now enriched) span to the exporter.
sentryScopeProcessor :: SpanExporter -> SpanProcessor
sentryScopeProcessor exporter =
  SpanProcessor
    { spanProcessorOnStart = \_ _ -> pure (),
      spanProcessorOnEnd = \immSpan -> do
        scope <- readAmbientScope
        enrichSpan immSpan scope
        _ <- exporter.spanExporterExport (HashMap.singleton "sentry-core-checkout" (Vector.singleton immSpan))
        pure (),
      spanProcessorShutdown = exporter.spanExporterShutdown,
      spanProcessorForceFlush = exporter.spanExporterForceFlush
    }

-- | Merge the scope-derived attributes into the span's live attribute set.
enrichSpan :: ImmutableSpan -> ScopeData -> IO ()
enrichSpan immSpan scope =
  modifyIORef' immSpan.spanHot \hot ->
    hot{hotAttributes = foldl' addPair hot.hotAttributes (scopeAttributes scope)}
  where
    addPair attrs (k, v) = addAttribute defaultAttributeLimits attrs k v

-- | Flatten the interesting parts of a 'ScopeData' into span attributes.
scopeAttributes :: ScopeData -> [(Text, Text)]
scopeAttributes scope =
  concat
    [ [("sentry.transaction", t) | Just t <- [scope.transaction]],
      [("sentry.level", Text.toLower (tshow l)) | Just l <- [scope.level]],
      [("sentry.user.email", e) | Just u <- [scope.user], let e = User.email u, not (Text.null e)],
      [("sentry.tag." <> k, v) | (k, v) <- Map.toList scope.tags]
    ]

renderAttributes :: Attributes -> [(Text, Text)]
renderAttributes attrs =
  [(k, renderAttribute v) | (k, v) <- HashMap.toList (getAttributeMap attrs)]

renderAttribute :: Attribute -> Text
renderAttribute attribute = case attribute of
  AttributeValue (TextAttribute t) -> t
  AttributeValue (BoolAttribute b) -> tshow b
  AttributeValue (DoubleAttribute d) -> tshow d
  AttributeValue (IntAttribute i) -> tshow i
  AttributeArray xs -> tshow xs

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
