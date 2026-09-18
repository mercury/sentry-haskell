-- | Envelope serialization and compression, shared by the transport backends.
--
-- Designed to be imported qualified:
--
-- > import Sentry.Transport.Encoding qualified as Encoding
-- >
-- > body = Encoding.encode Encoding.Gzip envelope
module Sentry.Transport.Encoding
  ( -- * Encoding choice
    Compression (..),

    -- * Prepared bodies
    EncodedBody,
    encode,
    fromBytes,
    bytes,
    compression,
    size,
  )
where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default (def))
import Data.Int (Int64)
import Data.Kind (Type)
import Patrol qualified
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

-- | The bytes to send, the encoding they were prepared with, and their length.
type EncodedBody :: Type
data EncodedBody = EncodedBody
  { -- | The encoding the body was prepared with. Request builders derive
    -- @Content-Encoding@ from this, so it has to describe the bytes.
    compression :: Compression,
    -- | The payload to attach to the request body.
    bytes :: LBS.ByteString,
    -- | The length of 'bytes'.
    --
    -- This field is strict, so forcing an 'EncodedBody' to weak head normal
    -- form (e.g. with 'Control.Exception.evaluate') drains the whole lazy
    -- 'bytes' spine, finishing serialization and compression.
    size :: Int64
  }

-- | Smart constructor for 'EncodedBytes' given a pre-serialized envelope
-- and the compression level that was used to serialize it.
--
-- __NOTE__: 'Compression' must accurately reflect how the bytes passed to
-- this function were produced; it's used to determine the @Content-Encoding@
-- header on the outgoing request, and a mismatch would result in Sentry
-- rejecting the request.
fromBytes :: Compression -> LBS.ByteString -> EncodedBody
fromBytes compression bytes = EncodedBody compression bytes (LBS.length bytes)

-- | Serialize an envelope, compressing it when asked.
encode :: Compression -> Patrol.Envelope -> EncodedBody
encode compression envelope = fromBytes compression (serializeBody compression envelope)

serializeBody :: Compression -> Patrol.Envelope -> LBS.ByteString
serializeBody compression envelope =
  let raw = Builder.toLazyByteString . Patrol.Envelope.serialize $ envelope
   in case compression of
        None -> raw
        Gzip ->
          GZip.compressWith
            GZip.defaultCompressParams{GZip.compressLevel = GZip.bestCompression}
            raw
