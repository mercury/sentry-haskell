{-# LANGUAGE OverloadedStrings #-}

-- | Space benchmarks for the capture path.
--
-- Each case drives a fixed number of capture calls through a recording client
-- backed by an in-memory collecting transport ('Sentry.Test.TestTransport'), so
-- the full pipeline runs (scope resolution + merge, integrations, @beforeSend@,
-- sampling, envelope construction) without any real I/O. weigh runs each case
-- in its own subprocess, so the process-global scope mutations below do not
-- leak between cases.
--
-- The client is installed on the __global__ scope (the 'Sentry.init' path), and
-- the metadata cases model a realistic server request: a little data on the
-- global scope (set once at init), per-request data on the isolation scope
-- (tags, extras, a breadcrumb trail), and per-operation data on the current
-- scope. Keys are __disjoint across layers__ — as in real apps (@release@ on
-- global, @request_id@ on isolation, @span@ data on current) — so every capture
-- genuinely merges all three (@global \<> isolation \<> current@), which is what
-- the three-scope model does per capture (matching @sentry-python@'s
-- @_merge_scopes@).
--
-- The "no scope data" cases are the floor (pipeline only); "typical" is an
-- ordinary instrumented error; "heavy" is a heavily-instrumented upper bound
-- (more tags, a breadcrumb trail near the usual 100-crumb cap).
module Main where

import Control.Monad (replicateM_)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.Level qualified as Patrol.Level
import Sentry qualified
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Scope (Scope)
import Sentry.Scope qualified as Scope
import Sentry.Test qualified as Test
import Weigh qualified

-- | Number of capture calls per case.
iterations :: Int
iterations = 10000

-- | How much metadata each scope layer carries. Keys are disjoint across
-- layers, so a capture merges all three.
type Profile :: Type
data Profile = Profile
  { globalTags :: Int,
    requestTags :: Int,
    currentTags :: Int,
    requestExtras :: Int,
    requestBreadcrumbs :: Int
  }

-- | An ordinary instrumented error event: a handful of tags spread across the
-- layers, a few extras, a short breadcrumb trail.
typical :: Profile
typical =
  Profile
    { globalTags = 3,
      requestTags = 5,
      currentTags = 2,
      requestExtras = 5,
      requestBreadcrumbs = 20
    }

-- | A heavily-instrumented upper bound: more tags and a breadcrumb trail near
-- the customary 100-crumb cap.
heavy :: Profile
heavy =
  Profile
    { globalTags = 5,
      requestTags = 15,
      currentTags = 5,
      requestExtras = 20,
      requestBreadcrumbs = 100
    }

-- | Install a fresh recording client on the process-global scope. The
-- breadcrumb cap is raised so the trails below are not trimmed (we want a
-- deterministic count to merge).
installGlobalClient :: IO ()
installGlobalClient = do
  transport <- Test.new
  g <- Scope.getGlobal
  Scope.bindClient (Just (Test.mkCustomClient transport def{maxBreadcrumbs = 1000})) g

-- | Capture @iterations@ message events.
captureN :: IO ()
captureN = replicateM_ iterations (Sentry.captureMessage_ Patrol.Level.Info "benchmark message")

-- Metadata population ---------------------------------------------------------

setTags :: Scope -> Text -> Int -> IO ()
setTags scope prefix count =
  for_ [1 .. count] \i -> Scope.setTag scope (prefix <> tshow i) ("value-" <> tshow i)

setExtras :: Scope -> Int -> IO ()
setExtras scope count =
  for_ [1 .. count] \i ->
    Scope.setExtra scope ("extra-" <> tshow i) (Aeson.String ("payload-" <> tshow i))

-- | Add @count@ breadcrumbs to the active isolation scope (where
-- 'Sentry.addBreadcrumb' writes), as a request handler would accumulate them.
addBreadcrumbs :: Int -> IO ()
addBreadcrumbs count =
  for_ [1 .. count] \i ->
    Sentry.addBreadcrumb Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = "breadcrumb-" <> tshow i}

tshow :: Int -> Text
tshow = Text.pack . show

-- Cases -----------------------------------------------------------------------

baselineMessage :: () -> IO ()
baselineMessage () = installGlobalClient *> captureN

baselineException :: () -> IO ()
baselineException () = do
  installGlobalClient
  replicateM_ iterations (Sentry.captureException_ (userError "benchmark boom"))

-- | A realistic request lifecycle: global metadata at init, per-request data on
-- the isolation scope (tags, extras, level, breadcrumb trail), per-operation
-- data on the current scope, then capture under the merged scope.
runProfile :: Profile -> () -> IO ()
runProfile p () = do
  installGlobalClient
  g <- Scope.getGlobal
  setTags g "global.tag-" p.globalTags
  Sentry.withIsolationScope \iso -> do
    setTags iso "request.tag-" p.requestTags
    setExtras iso p.requestExtras
    Scope.setLevel iso Patrol.Level.Warning
    addBreadcrumbs p.requestBreadcrumbs
    Sentry.withScope \cur -> do
      setTags cur "span.tag-" p.currentTags
      Scope.setFingerprint cur (Vector.fromList ["benchmark", "sample"])
      captureN

main :: IO ()
main = Weigh.mainWith do
  Weigh.io "message, no scope data" baselineMessage ()
  Weigh.io "exception, no scope data" baselineException ()
  Weigh.io "message, typical request scope" (runProfile typical) ()
  Weigh.io "message, heavy request scope" (runProfile heavy) ()
