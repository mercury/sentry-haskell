{-# LANGUAGE OverloadedLabels #-}

-- | A small, runnable tour of the optional optics API (@sentry-optics@).
--
-- It instruments a trivial "checkout" workflow end to end:
--
--   * a @beforeSend@ hook that rewrites every outgoing event with optics,
--   * an isolation scope stamped with a single @editScope@ block,
--   * a breadcrumb trail built from @empty*@ seeds, and
--   * a captured exception.
--
-- Rather than talk to a real Sentry, it uses the in-memory 'Sentry.Test'
-- transport to collect whatever the SDK would have sent, then pretty-prints the
-- captured events on the way out:
--
-- > cabal run sentry-optics:checkout
module Main (main) where

import Control.Exception (Exception, throwIO, try)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Sentry (CapturedEvent (..), ClientOptions (..))
import Sentry.Optics qualified as Sentry
import Sentry.Optics.Prelude
import Sentry.Test qualified as Test
import Text.Pretty.Simple (pPrint)

-- | A trivial domain error for us to capture.
newtype CheckoutError = CardDeclined Text
  deriving stock (Show)
  deriving anyclass (Exception)

main :: IO ()
main = do
  let options =
        def
          { release = Just "checkout-demo@1.0.0",
            environment = Just "demo",
            -- A `beforeSend` hook is just a function over the captured event.
            -- Here we reach the wire-format `Patrol.Event` via `ce.event` and
            -- rewrite it with optics: stamp the server name and redact the
            -- user's email through the optional `#user` with the `_Just` prism.
            beforeSend = Just \ce ->
              Just (ce.event & #serverName .~ "demo-host" & #user % _Just % #email .~ "[redacted]")
          }

  -- `withCustomClient` runs the full init lifecycle against an in-memory
  -- transport and hands it back so we can inspect what was collected.
  (_, transport) <- Test.withCustomClient options \_transport -> do
    checkout "alice@example.com" 150.00 -- declined: amount over the limit
    checkout "bob@example.com" 42.00 -- succeeds
  events <- Test.fetchAndClearEvents transport
  putStrLn ("\n=== collected " <> show (length events) <> " event(s) ===")
  pPrint events

-- | One instrumented unit of work. Everything captured inside the isolation
-- scope inherits the metadata stamped at the top.
checkout :: Text -> Double -> IO ()
checkout email amount =
  Sentry.withIsolationScope \scope -> do
    -- A single optics block stamps the whole scope. `?=` sets optional fields,
    -- `%` composes optics, and `at` indexes the tag map.
    Sentry.editScope scope do
      #level ?= #info
      #transaction ?= "checkout"
      #tags % at "feature" ?= "payments"
      #tags % at "currency" ?= "usd"
      #user ?= (Sentry.emptyUser & #email .~ email)

    -- Breadcrumbs built from `empty*` seeds with `&~`. `#ui` / `#info` are
    -- values here, type-directed by the `#type_` they sit under.
    Sentry.addBreadcrumb $
      Sentry.emptyBreadcrumb &~ do
        #type_ ?= #ui
        #category .= "ui"
        #message .= "user clicked 'pay'"
    Sentry.addBreadcrumb $
      Sentry.emptyBreadcrumb &~ do
        #type_ ?= #info
        #category .= "payments"
        #message .= ("charging $" <> tshow amount)

    try (attemptCharge amount) >>= \case
      Right () ->
        -- `#info` is a `Level` value here, fixed by `captureMessage_`'s type.
        Sentry.captureMessage_ #info ("checkout succeeded for " <> email)
      Left err -> do
        -- Escalate just this scope's level immediately before reporting.
        Sentry.editScope scope (#level ?= #error)
        Sentry.captureException_ (err :: CheckoutError)

-- | Trivial "work": decline anything over $100.
attemptCharge :: Double -> IO ()
attemptCharge amount
  | amount > 100 = throwIO (CardDeclined "amount exceeds per-transaction limit")
  | otherwise = pure ()

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
