module Sentry.Workload.Capture where

import Control.Monad (replicateM_, when)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Client (Client)
import Sentry.Client qualified as Client
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Core qualified as Sentry
import Sentry.Scope.Operations (Scope)
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test qualified as Test
import Sentry.Transport (SomeTransport (..), Transport (..))
import Sentry.Transport qualified as Transport
import Sentry.User qualified
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
    requestBreadcrumbs :: Int,
    -- | Number of successive 'Scope.modifyUser' calls against the current
    -- scope's user.
    --
    -- 'Sentry.Scope.Update.modifyUser' forces each new user before storing it,
    -- so residency here should stay flat as this count grows. Before that
    -- force existed, every call left a nested closure in the scope's 'IORef' —
    -- the @Maybe@-wrapped @fmap@ was lazy, so @Just (f x)@ was already in
    -- weak head normal form and @f x@ never ran. Nothing collapsed the chain
    -- under 'discardingClient', whose transport never inspects the envelope
    -- it is handed. This axis is the regression guard for that.
    userEdits :: Int
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
      requestBreadcrumbs = 20,
      userEdits = 5
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
      requestBreadcrumbs = 100,
      userEdits = 50
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
  Test.mkCustomClient transport def{maxBreadcrumbs = 1000, defaultIntegrations = False}

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
  Client.new
    def
      { dsn = Dsn.Explicit Test.TEST_DSN,
        transport = Just $ Witch.from $ SomeTransport DiscardTransport,
        defaultIntegrations = False,
        maxBreadcrumbs = 1000
      }

-- | Capture @iterations@ message events.
captureN :: IO ()
captureN = replicateM_ iterations (Sentry.captureMessage_ Patrol.Level.Info "benchmark message")

-- Each workload includes one full client initialization outside its event loop.
-- Built-in integrations are disabled to isolate capture costs.

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

-- | Call 'Scope.modifyUser' @count@ times in a row, each assigning a distinct
-- name so GHC can't collapse the calls via common-subexpression elimination.
-- See 'userEdits' for why this is worth measuring.
--
-- 'Scope.modifyUser' no-ops without a user to update, so seed one first.
modifyUserRepeatedly :: Scope -> Int -> IO ()
modifyUserRepeatedly scope count = when (count > 0) do
  Scope.setUser scope (Sentry.User.setId "workload")
  for_ [1 .. count] \i -> Scope.modifyUser scope (Sentry.User.setName (tshow i))

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
      Scope.setFingerprint cur ["benchmark", "sample"]
      modifyUserRepeatedly cur p.userEdits
      captureN
