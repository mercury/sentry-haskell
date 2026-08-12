{-# LANGUAGE OverloadedStrings #-}

-- | Space benchmarks for the capture path.
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
import Sentry.Client (Client)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Core qualified as Sentry
import Sentry.Scope (Scope)
import Sentry.Scope qualified as Scope
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport qualified as Transport
import Weigh qualified
import Witch qualified

-- | Number of capture calls per case.
iterations :: Int
iterations = 10000

-- | How much metadata each scope layer carries.
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

-- | Install a fresh recording client on the process-global scope.
installGlobalClient :: IO ()
installGlobalClient = do
  transport <- Test.new
  g <- Scope.getGlobal
  Scope.bindClient (Just (Test.mkCustomClient transport def{maxBreadcrumbs = 1000})) g

-- | A transport that discards every envelope immediately, retaining nothing.
type DiscardTransport :: Type
data DiscardTransport = DiscardTransport

instance Transport DiscardTransport where
  send _ _ = pure Transport.SendProcessed

installDiscardingClient :: IO ()
installDiscardingClient = do
  globalScope <- Scope.getGlobal
  flip Scope.bindClient globalScope $
    Just $
      Witch.from @ClientOptions @Client
        def
          { dsn = Just Test.TEST_DSN,
            transport = Just $ Witch.from $ SomeTransport DiscardTransport,
            maxBreadcrumbs = 1000
          }

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

addBreadcrumbs :: Int -> IO ()
addBreadcrumbs count =
  for_ [1 .. count] \i ->
    Sentry.addBreadcrumb Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = "breadcrumb-" <> tshow i}

tshow :: Int -> Text
tshow = Text.pack . show

-- Cases -----------------------------------------------------------------------

baselineMessage :: () -> IO ()
baselineMessage () = installGlobalClient *> captureN

baselineMessageDiscarding :: () -> IO ()
baselineMessageDiscarding () = installDiscardingClient *> captureN

baselineException :: () -> IO ()
baselineException () = do
  installGlobalClient
  replicateM_ iterations (Sentry.captureException_ (userError "benchmark boom"))

-- | A realistic request lifecycle:
--
-- * global metadata at init
-- * per-request data on the isolation scope
-- * per-operation data on the current scope
-- * capture under the merged scope
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
  Weigh.setColumns [Weigh.Case, Weigh.Allocated, Weigh.GCs, Weigh.Live, Weigh.Max, Weigh.MaxOS]
  Weigh.io "message, no scope data" baselineMessage ()
  Weigh.io "message, no scope data (discarding transport)" baselineMessageDiscarding ()
  Weigh.io "exception, no scope data" baselineException ()
  Weigh.io "message, typical request scope" (runProfile typical) ()
  Weigh.io "message, heavy request scope" (runProfile heavy) ()
