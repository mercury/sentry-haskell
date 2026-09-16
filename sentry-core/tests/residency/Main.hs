-- | Measure retention while a scope and its unforced payload maps stay live.
module Main where

import Control.Monad (forM_, unless)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Sentry.Breadcrumb qualified as Breadcrumb
import Sentry.Context qualified as Context
import Sentry.Core qualified as Sentry
import Sentry.Scope qualified as Scope
import Sentry.User qualified as User
import System.Mem (performGC)

main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  unless enabled $ fail "RTS statistics are required (-T)"
  userResidency
  contextResidency
  contextTransformationResidency
  breadcrumbResidency

userResidency :: IO ()
userResidency = do
  scope <- Scope.create Scope.Current
  Sentry.updateScope scope (Scope.setUser (User.setId "resident"))
  performGC
  before <- gcdetails_live_bytes . gc <$> getRTSStats
  forM_ [1 .. 200_000 :: Int] \n ->
    Sentry.updateScope scope (Scope.modifyUser (User.setData "key" (Aeson.toJSON n)))
  performGC
  after <- gcdetails_live_bytes . gc <$> getRTSStats
  -- Read only after measurement: this keeps the scope live without forcing
  -- its map during the overwrite loop or the final collection.
  snapshot <- Scope.readScopeRef scope
  unless (((Map.lookup "key" . (.data_)) <$> snapshot.user) == Just (Just (Aeson.toJSON (200_000 :: Int)))) $
    fail "final user data was lost"
  let growth = toInteger after - toInteger before
  putStrLn ("User data: retained-memory growth after 200,000 overwrites: " <> show growth <> " bytes")
  unless (growth < 1_048_576) $ fail "keyed user updates retained at least 1 MiB"

-- Each case owns its scope. The baseline GC collects the preceding case,
-- and the payload is read only after the final measurement.
contextResidency :: IO ()
contextResidency = do
  scope <- Scope.create Scope.Current
  Sentry.updateScope scope (Scope.setContextValues "custom" [])
  performGC
  before <- gcdetails_live_bytes . gc <$> getRTSStats
  forM_ [1 .. 200_000 :: Int] \n ->
    Sentry.updateScope scope (Scope.setContextValue "custom" "key" (Aeson.toJSON n))
  performGC
  after <- gcdetails_live_bytes . gc <$> getRTSStats
  snapshot <- Scope.readScopeRef scope
  case Map.lookup "custom" snapshot.contexts of
    Just (Context.Other values) ->
      unless (Map.lookup "key" values == Just (Aeson.toJSON (200_000 :: Int))) $
        fail "final custom-context value was lost"
    _ -> fail "custom context was lost"
  let growth = toInteger after - toInteger before
  putStrLn ("Custom context: retained-memory growth after 200,000 overwrites: " <> show growth <> " bytes")
  unless (growth < 1_048_576) $ fail "keyed custom-context updates retained at least 1 MiB"

contextTransformationResidency :: IO ()
contextTransformationResidency = do
  scope <- Scope.create Scope.Current
  Sentry.updateScope scope (Scope.setContextValues "custom" [])
  performGC
  before <- gcdetails_live_bytes . gc <$> getRTSStats
  forM_ [1 .. 200_000 :: Int] \n ->
    Sentry.updateScope scope (Scope.alterContextValue "custom" "key" (const (Just (Aeson.toJSON n))))
  performGC
  after <- gcdetails_live_bytes . gc <$> getRTSStats
  snapshot <- Scope.readScopeRef scope
  case Map.lookup "custom" snapshot.contexts of
    Just (Context.Other values) ->
      unless (Map.lookup "key" values == Just (Aeson.toJSON (200_000 :: Int))) $
        fail "final custom-context value was lost"
    _ -> fail "custom context was lost"
  let growth = toInteger after - toInteger before
  putStrLn ("Custom context transformation: retained-memory growth after 200,000 overwrites: " <> show growth <> " bytes")
  unless (growth < 1_048_576) $ fail "keyed custom-context updates retained at least 1 MiB"

-- | The breadcrumb trail is the highest-churn scope path: unlike the keyed
-- overwrites above, each call appends rather than replacing a single key, so
-- retention depends on 'Scope.trimBreadcrumbs' keeping the trail bounded
-- rather than on overwrite forcing.
breadcrumbResidency :: IO ()
breadcrumbResidency = do
  scope <- Scope.create Scope.Current
  performGC
  before <- gcdetails_live_bytes . gc <$> getRTSStats
  forM_ [1 .. 200_000 :: Int] \n ->
    Sentry.updateScope
      scope
      (Scope.appendBreadcrumb (Breadcrumb.setMessage (tshow n)) <> Scope.trimBreadcrumbs breadcrumbCap)
  performGC
  after <- gcdetails_live_bytes . gc <$> getRTSStats
  snapshot <- Scope.readScopeRef scope
  unless (length snapshot.breadcrumbs == breadcrumbCap) $
    fail "breadcrumb trail was not trimmed to the cap"
  let growth = toInteger after - toInteger before
  putStrLn ("Breadcrumbs: retained-memory growth after 200,000 appends: " <> show growth <> " bytes")
  unless (growth < 1_048_576) $ fail "breadcrumb trimming retained at least 1 MiB"

breadcrumbCap :: Int
breadcrumbCap = 100

tshow :: Int -> Text
tshow = Text.pack . show
