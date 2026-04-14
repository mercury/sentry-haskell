-- | A pre-built 'HttpClient.Request' template scoped to a single 'Patrol.Dsn'.
--
-- Rather than parsing a URL from the DSN, inserting headers, etc. per-send,
-- we can preconstruct it for the lifetime of the HTTP transport:
-- 
--     * 'prepare' runs at transport construction to produce a'PreparedRequest'
--     * 'attach' runs before send and attaches the serialized envelope body
module Sentry.Transport.HTTP.Request
  ( PreparedRequest,
    prepare,
    attach,
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Encoding
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Constant qualified as Patrol.Constant
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope

-- | An 'HttpClient.Request' template constructed from a 'Patrol.Dsn'.
type PreparedRequest :: Type
newtype PreparedRequest = PreparedRequest HttpClient.Request

-- | Build the request template for the given DSN.
prepare :: Patrol.Dsn -> PreparedRequest
prepare dsn =
  PreparedRequest $
    HttpClient.defaultRequest
      { HttpClient.method = HttpTypes.methodPost,
        HttpClient.secure = dsn.protocol == "https",
        HttpClient.host = Encoding.encodeUtf8 dsn.host,
        HttpClient.port = fromMaybe defaultPort (fromIntegral <$> dsn.port),
        HttpClient.path =
          Encoding.encodeUtf8 $
            dsn.path <> "api/" <> dsn.projectId <> "/envelope/",
        HttpClient.requestHeaders =
          [ (HttpTypes.hContentType, Patrol.Constant.applicationXSentryEnvelope),
            (HttpTypes.hUserAgent, Encoding.encodeUtf8 Patrol.Constant.userAgent),
            (Patrol.Constant.xSentryAuth, Patrol.Dsn.intoAuthorization dsn)
          ]
      }
  where
    defaultPort
      | dsn.protocol == "https" = 443
      | otherwise = 80

-- | Attach the serialized envelope as the request body.
attach :: PreparedRequest -> Patrol.Envelope -> HttpClient.Request
attach (PreparedRequest template) envelope =
  template{HttpClient.requestBody = HttpClient.RequestBodyBS body}
  where
    body =
      LBS.toStrict
        . Builder.toLazyByteString
        . Patrol.Envelope.serialize
        $ envelope
