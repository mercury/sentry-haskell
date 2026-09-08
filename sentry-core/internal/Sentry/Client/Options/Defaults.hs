-- | Terminal configuration defaults shared by normalization and runtime projection.
module Sentry.Client.Options.Defaults where

import Data.Text (Text)

debug :: Bool
debug = False

sampleRate :: Float
sampleRate = 1

environment :: Text
environment = "production"
