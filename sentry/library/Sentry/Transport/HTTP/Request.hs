-- | A pre-built 'HttpClient.Request' template scoped to a single 'Patrol.Dsn'.
--
-- Rather than parsing a URL from the DSN, inserting headers, etc. per-send,
-- we can preconstruct it for the lifetime of the HTTP transport:
--
--     * 'prepare' runs at transport construction to produce a 'PreparedRequest'
--     * 'attach' runs before send and attaches the encoded envelope body
module Sentry.Transport.HTTP.Request
  ( PreparedRequest,
    prepare,
    attach,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Constant qualified as Patrol.Constant
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Sentry.Sdk qualified
import Sentry.Transport.Encoding (Compression (..), EncodedBody)
import Sentry.Transport.Encoding qualified as Encoding

-- | An 'HttpClient.Request' template constructed from a 'Patrol.Dsn'.
type PreparedRequest :: Type
data PreparedRequest = PreparedRequest HttpClient.Request

-- | Build the request template for the given DSN.
--
-- The template carries no @Content-Encoding@ header; 'attach' adds one when
-- the body it is given is compressed.
prepare :: Patrol.Dsn -> PreparedRequest
prepare dsn =
  PreparedRequest $
    HttpClient.defaultRequest
      { HttpClient.method = HttpTypes.methodPost,
        HttpClient.secure = dsn.protocol == "https",
        HttpClient.host = Text.Encoding.encodeUtf8 dsn.host,
        HttpClient.port = fromMaybe defaultPort (fromIntegral <$> dsn.port),
        HttpClient.path =
          Text.Encoding.encodeUtf8 $
            dsn.path <> "api/" <> dsn.projectId <> "/envelope/",
        HttpClient.requestHeaders =
          [ (HttpTypes.hContentType, Patrol.Constant.applicationXSentryEnvelope),
            (HttpTypes.hUserAgent, Text.Encoding.encodeUtf8 Sentry.Sdk.userAgent),
            (Patrol.Constant.xSentryAuth, Patrol.Dsn.intoAuthorization dsn)
          ]
      }
  where
    defaultPort
      | dsn.protocol == "https" = 443
      | otherwise = 80

-- | Attach an encoded envelope body, taking @Content-Encoding@ from the
-- 'EncodedBody'.
attach :: PreparedRequest -> EncodedBody -> HttpClient.Request
attach (PreparedRequest template) body =
  template
    { HttpClient.requestBody = HttpClient.RequestBodyLBS (Encoding.bytes body),
      HttpClient.requestHeaders =
        HttpClient.requestHeaders template
          <> case Encoding.compression body of
            None -> []
            Gzip -> [(HttpTypes.hContentEncoding, "gzip")]
    }
