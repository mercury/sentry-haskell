module EncodingTest where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Sentry.Transport.Encoding qualified as Encoding
import Test.Hspec

-- | 'Encoding.fromBytes' stores 'Encoding.size' in a strict field, so forcing
-- the resulting 'Encoding.EncodedBody' to weak head normal form has to sum
-- every chunk of the lazy body to get that length, not just look at the
-- first one. A chunk that throws only when it is itself forced, placed after
-- the first, pins that down: if forcing the record stopped at the first
-- chunk, this would never fire.
spec_forcing :: Spec
spec_forcing = describe "fromBytes strictness" do
  it "forces every chunk of the body, not just the first" do
    let chunks = ["kept", error "boom"] :: [BS.ByteString]
        body = Encoding.fromBytes Encoding.None (LBS.fromChunks chunks)
    evaluate body `shouldThrow` errorCall "boom"
