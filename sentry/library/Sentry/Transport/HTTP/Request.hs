-- | A pre-built 'HttpClient.Request' template scoped to a single 'Patrol.Dsn'.
--
-- Rather than parsing a URL from the DSN, inserting headers, etc. per-send,
-- we can preconstruct it for the lifetime of the HTTP transport:
--
--     * 'prepare' runs at transport construction to produce a 'PreparedRequest'
--     * 'attach' runs before send and attaches the serialized envelope body
module Sentry.Transport.HTTP.Request
  ( Compression (..),
    PreparedRequest,
    prepare,
    attach,
    serializeBody,
  )
where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default (def))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Encoding
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Constant qualified as Patrol.Constant
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope

-- | How to encode the request body on the wire.
--
-- Sentry's ingest endpoint accepts gzip-compressed envelope bodies via the
-- @Content-Encoding: gzip@ header, which is the default.
type Compression :: Type
data Compression
  = -- | Send the envelope body uncompressed.
    None
  | -- | Compress the envelope body with gzip (level 9) and set
    -- @Content-Encoding: gzip@.
    Gzip
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Defaults to 'Gzip'.
instance Default Compression where
  def = Gzip

-- | An 'HttpClient.Request' template constructed from a 'Patrol.Dsn'.
type PreparedRequest :: Type
data PreparedRequest = PreparedRequest Compression HttpClient.Request

-- | Build the request template for the given DSN.
prepare :: Compression -> Patrol.Dsn -> PreparedRequest
prepare compression dsn =
  PreparedRequest compression $
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
            <> case compression of
              None -> []
              Gzip -> [(HttpTypes.hContentEncoding, "gzip")]
      }
  where
    defaultPort
      | dsn.protocol == "https" = 443
      | otherwise = 80

-- | Serialise an envelope to a (possibly gzip-compressed) lazy 'LBS.ByteString'.
--
-- Shared by both the HTTP\/1.1 and HTTP\/2 transport backends so that body
-- construction is not duplicated.
serializeBody :: Compression -> Patrol.Envelope -> LBS.ByteString
serializeBody compression envelope =
  let raw = Builder.toLazyByteString . Patrol.Envelope.serialize $ envelope
   in case compression of
        None -> raw
        Gzip ->
          GZip.compressWith
            GZip.defaultCompressParams{GZip.compressLevel = GZip.bestCompression}
            raw

-- | Attach the serialized (and optionally compressed) envelope as the request body.
attach :: PreparedRequest -> Patrol.Envelope -> HttpClient.Request
attach (PreparedRequest compression template) envelope =
  template{HttpClient.requestBody = HttpClient.RequestBodyLBS (serializeBody compression envelope)}
