{-# LANGUAGE TemplateHaskell #-}

-- | An in-process HTTP\/2 recording sink for testing and profiling the HTTP\/2
-- transport.
module Sentry.TestKit.Http2Sink
  ( -- * Recorded requests
    RecordedRequest (..),
    decodeBody,

    -- * Scripted responses
    SinkResponse (..),
    Responder,
    ok,
    setResponder,

    -- * Lifecycle
    Http2SinkHandle (..),
    withHttp2Sink,

    -- * DSN construction
    dsnFor,

    -- * Inspecting received requests
    received,
  )
where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent.Async qualified as Async
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import UnliftIO.Exception (bracket)

-- | A single HTTP request recorded by the sink.
type RecordedRequest :: Type
data RecordedRequest = RecordedRequest
  { -- | HTTP method; always @POST@ for Sentry envelope sends.
    method :: ByteString,
    -- | Raw request path, e.g. @\/api\/1\/envelope\/@.
    path :: ByteString,
    -- | Negotiated HTTP version.  Should always be 'Http.http20'.
    httpVersion :: Http.HttpVersion,
    -- | All request headers, including @x-sentry-auth@ and @content-encoding@.
    headers :: Http.RequestHeaders,
    -- | Raw request body as received (gzip-compressed when the transport uses
    -- the default 'Gzip' compression setting).
    body :: LBS.ByteString
  }

-- | Decompress the body if @content-encoding: gzip@ is present; otherwise
-- return the raw bytes unchanged.
decodeBody :: RecordedRequest -> LBS.ByteString
decodeBody req
  | Just "gzip" <- lookup "content-encoding" req.headers = GZip.decompress req.body
  | otherwise = req.body

-- | The response the sink sends for a particular request.
type SinkResponse :: Type
data SinkResponse = SinkResponse
  { status :: Http.Status,
    responseHeaders :: Http.ResponseHeaders
  }

-- | 200 OK with no extra headers — the default response.
ok :: SinkResponse
ok = SinkResponse{status = Http.status200, responseHeaders = []}

-- | @f i req@ maps request index @i@ (0-based) and the recorded request to the
-- response the sink should return.
--
-- Swap the responder mid-test with 'setResponder' to inject error codes or
-- rate-limit headers.
type Responder :: Type
type Responder = Int -> RecordedRequest -> IO SinkResponse

-- | A handle to a running HTTP\/2 sink server.
type Http2SinkHandle :: Type
data Http2SinkHandle = Http2SinkHandle
  { port :: Int,
    -- | @(request-count, requests-in-reverse-arrival-order)@
    recordRef :: IORef (Int, [RecordedRequest]),
    responderRef :: IORef Responder
  }

-- | Start an HTTP\/2 TLS sink on an OS-assigned free port, run the action,
-- then tear the server down.
withHttp2Sink :: (Http2SinkHandle -> IO a) -> IO a
withHttp2Sink action =
  bracket Warp.openFreePort (Socket.close . snd) \(port, sock) -> do
    recordRef <- newIORef (0, [])
    responderRef <- newIORef (\_ _ -> pure ok)
    let handle = Http2SinkHandle{port, recordRef, responderRef}
        runner = WarpTLS.runTLSSocket embeddedTlsSettings Warp.defaultSettings sock (mkApp recordRef responderRef)
    Async.withAsync runner \serverAsync -> do
      -- Surface any server-side crash immediately rather than letting the
      -- client hang waiting for a dead server.
      Async.link serverAsync
      action handle

-- | A DSN pointing at this sink for the given project ID.
--
-- Always uses the @https@ scheme (TLS with ALPN @h2@).
dsnFor :: Http2SinkHandle -> Text -> Patrol.Dsn
dsnFor handle pid =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "https",
      Patrol.Dsn.publicKey = "public",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = "127.0.0.1",
      Patrol.Dsn.port = Just (fromIntegral handle.port),
      Patrol.Dsn.path = "/",
      Patrol.Dsn.projectId = pid
    }

-- | All requests received so far, in arrival order.
received :: Http2SinkHandle -> IO [RecordedRequest]
received handle = do
  (_, rs) <- readIORef handle.recordRef
  pure (reverse rs)

-- | Replace the active 'Responder'.  Takes effect for the next request; a
-- request already being processed may still use the previous responder.
setResponder :: Http2SinkHandle -> Responder -> IO ()
setResponder handle = writeIORef handle.responderRef

-- | Read the request body strictly into a lazy 'LBS.ByteString'.
--
-- The eager read matters: draining all HTTP\/2 DATA frames before 'respond'
-- prevents a flow-control deadlock where the server blocks waiting for body
-- data while the client blocks waiting for the response.
strictBody :: Wai.Request -> IO LBS.ByteString
strictBody req = fmap (LBS.fromChunks . reverse) $ go []
  where
    go acc = do
      chunk <- Wai.getRequestBodyChunk req
      if BS.null chunk then pure acc else go (chunk : acc)

mkApp :: IORef (Int, [RecordedRequest]) -> IORef Responder -> Wai.Application
mkApp recordRef responderRef request respond = do
  body <- strictBody request
  let req =
        RecordedRequest
          { method = Wai.requestMethod request,
            path = Wai.rawPathInfo request,
            httpVersion = Wai.httpVersion request,
            headers = Wai.requestHeaders request,
            body
          }
  idx <- atomicModifyIORef' recordRef \(n, rs) -> ((n + 1, req : rs), n)
  responder <- readIORef responderRef
  SinkResponse{status, responseHeaders} <- responder idx req
  respond (Wai.responseLBS status responseHeaders mempty)

-- | P-256 self-signed certificate (10-year validity, SAN for 127.0.0.1 and
-- localhost), embedded at compile time.
embeddedCert :: ByteString
embeddedCert = $(makeRelativeToProject "testkit/certs/localhost.crt" >>= embedFile)

-- | Private key corresponding to 'embeddedCert'.
embeddedKey :: ByteString
embeddedKey = $(makeRelativeToProject "testkit/certs/localhost.key" >>= embedFile)

-- | TLS settings for the test server using the embedded self-signed cert.
embeddedTlsSettings :: WarpTLS.TLSSettings
embeddedTlsSettings = WarpTLS.tlsSettingsMemory embeddedCert embeddedKey
