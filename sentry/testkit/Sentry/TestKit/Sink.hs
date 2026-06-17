-- | An in-process HTTP sink that accepts and discards envelope POSTs.
--
-- Unlike the kent mock (a connection-closing Flask dev server), this is a
-- @warp@ server that speaks HTTP\/1.1 keep-alive, so @http-client@ reuses
-- connections across sends. It keeps no event store — it exists purely to
-- exercise the transport's steady-state send path without per-request
-- connection churn, e.g. when profiling.
module Sentry.TestKit.Sink
  ( SinkHandle (..),
    withSink,
    dsnFor,
  )
where

import Control.Monad (void)
import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn

-- | A handle to a running sink server.
type SinkHandle :: Type
data SinkHandle = SinkHandle
  { port :: Int,
    manager :: HttpClient.Manager
  }

-- | Start a keep-alive HTTP sink on an OS-assigned free port, run the action,
-- then shut the server down.
--
-- 'Warp.testWithApplication' picks the port, runs the server on a background
-- thread, and tears it down when the action returns.
withSink :: (SinkHandle -> IO a) -> IO a
withSink action =
  Warp.testWithApplication (pure app) \port -> do
    manager <- HttpClient.newManager HttpClient.defaultManagerSettings
    action SinkHandle{port, manager}
  where
    app :: Wai.Application
    app request respond = do
      -- Drain the request body so the connection stays reusable, then 200.
      void $ Wai.consumeRequestBodyStrict request
      respond $ Wai.responseLBS Http.status200 [] mempty

-- | A DSN pointing at this sink for the given project ID.
dsnFor :: SinkHandle -> Text -> Patrol.Dsn
dsnFor handle pid =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "http",
      Patrol.Dsn.publicKey = "public",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = "127.0.0.1",
      Patrol.Dsn.port = Just (fromIntegral handle.port),
      Patrol.Dsn.path = "/",
      Patrol.Dsn.projectId = pid
    }
