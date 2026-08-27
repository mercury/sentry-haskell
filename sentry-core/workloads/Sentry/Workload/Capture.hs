module Sentry.Workload.Capture where

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

-- | Build a client and install it on the process-global scope.
installClientWith :: IO Client -> IO ()
installClientWith mkClient = do
  client <- mkClient
  g <- Scope.getGlobal
  Scope.bindClient (Just client) g

-- | A client backed by 'Sentry.Test.TestTransport', which retains every
-- captured envelope until the in-memory queue is drained.
--
--
-- Workloads using this client measure the combined cost of the SDK's capture
-- path plus this transport's own bookkeeping.
recordingClient :: IO Client
recordingClient = do
  transport <- Test.new
  pure (Test.mkCustomClient transport def{maxBreadcrumbs = 1000})

-- | A transport that discards every envelope immediately, retaining nothing.
type DiscardTransport :: Type
data DiscardTransport = DiscardTransport

instance Transport DiscardTransport where
  send _ _ = pure Transport.SendProcessed

-- | A client that discards every envelope immediately.
--
-- Workloads using this client measure the cost of the SDK's capture path in
-- isolation; as a consequence of this, benchmarks with this client should
-- indicate flat memory residency, otherwise we've introduced a space leak.
discardingClient :: IO Client
discardingClient =
  pure $
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

baselineMessage :: IO Client -> () -> IO ()
baselineMessage mkClient () = installClientWith mkClient *> captureN

baselineException :: IO Client -> () -> IO ()
baselineException mkClient () = do
  installClientWith mkClient
  replicateM_ iterations (Sentry.captureException_ (userError "benchmark boom"))

-- | A realistic request lifecycle:
--
-- * global metadata at init
-- * per-request data on the isolation scope
-- * per-operation data on the current scope
-- * capture under the merged scope
runProfile :: IO Client -> Profile -> () -> IO ()
runProfile mkClient p () = do
  installClientWith mkClient
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
