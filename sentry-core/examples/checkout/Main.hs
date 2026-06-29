{-# LANGUAGE QualifiedDo #-}

-- | A runnable tour of the core Sentry API (@sentry-core@, no optics) wired up
-- to OpenTelemetry: the same scope that drives Sentry events is also harvested
-- onto OTel spans by a custom 'SpanProcessor' (see "Otel").
--
-- This is the non-optics counterpart to the @sentry-optics:checkout@ demo. It
-- performs the exact same workflow and prints the exact same output; the only
-- difference is /how the scope is authored/. Where the optics tour writes a
-- @editScope@ block of labelled optics, this one composes the 'ScopeUpdate'
-- builders from "Sentry.Scope.Update" — here via a @QualifiedDo@ @Scope.Update.do@
-- block, the imperative counterpart to @editScope@ — and builds Patrol records
-- with plain record syntax.
--
-- The checkout workflow is instrumented with nested spans:
--
--   * @checkout@ — outer span; the isolation scope carries transaction + feature
--     tag + user.
--   * @charge@ — inner span, inside a forked @withScope@ that adds a @step@ tag
--     and a level. Because it ends /inside/ that block, it harvests those too.
--   * @receipt@ — sibling span that ends /after/ the forked scope is popped, so
--     it harvests only the isolation-level metadata.
--
-- On the way out we print, per span, exactly which scope metadata it ended with
-- (so the location difference is visible), then dump the collected Sentry
-- events. Everything runs in-memory — no network:
--
-- > cabal run sentry-core:checkout
module Main (main) where

import Control.Exception (Exception, throwIO, try)
import Data.Default (def)
import Data.Kind (Type)
import Data.List (intercalate, sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import OpenTelemetry.Trace.Core (Tracer, defaultSpanArguments, inSpan')
import Otel (HarvestedSpan (..), withInMemoryTracer)
import Patrol.Type.Breadcrumb qualified as Breadcrumb
import Patrol.Type.BreadcrumbType qualified as BreadcrumbType
import Patrol.Type.Event qualified as Event
import Patrol.Type.Level qualified as Level
import Patrol.Type.User qualified as User
import Prettyprinter (Doc, annotate, fill, parens, pretty, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull, putDoc)
import Sentry (CapturedEvent (..), ClientOptions (..))
import Sentry qualified
import Sentry.Scope.Update qualified as Scope.Update
import Sentry.Test qualified as Test
import Text.Printf (printf)

-- | A trivial domain error for us to capture.
type CheckoutError :: Type
newtype CheckoutError = CardDeclined Text
  deriving stock (Show)
  deriving anyclass (Exception)

main :: IO ()
main = do
  let options =
        def
          { release = Just "checkout-demo@1.0.0",
            environment = Just "demo",
            -- A `beforeSend` hook is just a function over the captured event;
            -- here it stamps the server name and redacts the user's email with
            -- plain record updates over `ce.event`. (This rewrites the event
            -- only — the span harvest below sees the real scope.)
            beforeSend = Just \(CapturedEvent{event}) ->
              Just
                event
                  { Event.serverName = "demo-host",
                    Event.user = (\u -> u{User.email = "[redacted]"}) <$> event.user
                  }
          }

  -- Install a tracer backed by the in-memory, scope-harvesting processor, run
  -- the workflow against the in-memory Sentry transport, and collect both the
  -- harvested spans and the Sentry events.
  (events, spans) <- withInMemoryTracer \tracer -> do
    (_, transport) <- Test.withCustomClient options \_transport -> do
      checkout tracer "alice@example.com" 150.00 -- declined: amount over the limit
      checkout tracer "bob@example.com" 42.00 -- succeeds
    Test.fetchAndClearEvents transport

  putStrLn "\n=== harvested spans (by trace) ==="
  putDoc (renderForest spans)
  putStrLn ""

  putStrLn ("\n=== collected " <> show (length events) <> " event(s) ===")
  mapM_ print events

-- | One instrumented unit of work, wrapped in nested spans so the harvest
-- differs by where each span ends.
checkout :: (HasCallStack) => Tracer -> Text -> Double -> IO ()
checkout tracer email amount =
  Sentry.withIsolationScope \isolation ->
    inSpan' tracer "checkout" defaultSpanArguments \_ -> do
      -- Isolation-level metadata: visible to every span in this checkout.
      Scope.Update.apply isolation Scope.Update.do
        Scope.Update.setTransaction "checkout"
        Scope.Update.setTag "feature" "payments"
        Scope.Update.setUser (User.empty{User.email = email})

      Sentry.addBreadcrumb
        Breadcrumb.empty
          { Breadcrumb.type_ = Just BreadcrumbType.UI,
            Breadcrumb.category = "ui",
            Breadcrumb.message = "user clicked 'pay'"
          }

      -- A forked current scope: its metadata is only visible to spans that end
      -- within this block, so "charge" harvests it but "receipt" does not.
      Sentry.withScope \scope ->
        inSpan' tracer "charge" defaultSpanArguments \_ -> do
          Scope.Update.apply scope Scope.Update.do
            Scope.Update.setTag "step" "charge"
            Scope.Update.setLevel Level.Info
          Sentry.addBreadcrumb
            Breadcrumb.empty
              { Breadcrumb.type_ = Just BreadcrumbType.Info,
                Breadcrumb.category = "payments",
                Breadcrumb.message = "charging $" <> tshow amount
              }
          try (attemptCharge amount) >>= \case
            Right () -> pure ()
            Left err -> do
              Scope.Update.apply scope $ Scope.Update.setLevel Level.Error
              Sentry.captureException_ (err :: CheckoutError)

      -- Ends after the forked scope is popped → harvests only the
      -- isolation-level metadata (no step/level).
      inSpan' tracer "receipt" defaultSpanArguments \_ ->
        Sentry.captureMessage_ Level.Info ("receipt for " <> email)

-- | Trivial "work": decline anything over $100.
attemptCharge :: Double -> IO ()
attemptCharge amount
  | amount > 100 = throwIO (CardDeclined "amount exceeds per-transaction limit")
  | otherwise = pure ()

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- | Render the harvested span forest as a colored tree, one trace per root.
-- Every attribute the span ended with is shown; @sentry.*@ keys (the harvested
-- scope) are highlighted, OTel's own @code.*@ \/ @thread.*@ bookkeeping is dimmed.
renderForest :: [HarvestedSpan] -> Doc AnsiStyle
renderForest = vsep . intercalate [mempty] . map renderRoot

renderRoot :: HarvestedSpan -> [Doc AnsiStyle]
renderRoot s =
  (annotate (color Green <> bold) "●" <+> spanHeader s)
    : attrDocs "  " s
    ++ renderChildren "  " (children s)

renderChildren :: String -> [HarvestedSpan] -> [Doc AnsiStyle]
renderChildren prefix kids =
  concat [renderChild prefix (i == length kids - 1) k | (i, k) <- zip [0 :: Int ..] kids]

renderChild :: String -> Bool -> HarvestedSpan -> [Doc AnsiStyle]
renderChild prefix isLast s =
  (faint (pretty (prefix <> connector)) <> spanHeader s)
    : attrDocs (prefix <> bar <> "   ") s
    ++ renderChildren (prefix <> bar) (children s)
  where
    connector = if isLast then "└─ " else "├─ "
    bar = if isLast then "   " else "│  "

-- | A span's name (bold) and its duration (dim yellow).
spanHeader :: HarvestedSpan -> Doc AnsiStyle
spanHeader s =
  annotate bold (pretty (name s))
    <+> annotate (colorDull Yellow) (parens (pretty (formatDuration (durationNanos s))))

-- | Every attribute of a span, key-aligned and sorted; @sentry.*@ keys green,
-- everything else dimmed.
attrDocs :: String -> HarvestedSpan -> [Doc AnsiStyle]
attrDocs prefix s =
  [ faint (pretty prefix) <> fill width (annotate (keyStyle k) (pretty k)) <+> faint "=" <+> pretty v
  | (k, v) <- shown
  ]
  where
    shown = sortOn fst (attributes s)
    width = maximum (0 : [Text.length k | (k, _) <- shown])
    keyStyle k = if "sentry." `Text.isPrefixOf` k then color Green else colorDull White

faint :: Doc AnsiStyle -> Doc AnsiStyle
faint = annotate (colorDull White)

-- | Render a nanosecond duration as ms / µs / ns, whichever reads cleanest.
formatDuration :: Integer -> String
formatDuration ns
  | ns >= 1_000_000 = printf "%.2fms" (fromIntegral ns / 1e6 :: Double)
  | ns >= 1_000 = printf "%.2fµs" (fromIntegral ns / 1e3 :: Double)
  | otherwise = show ns <> "ns"
