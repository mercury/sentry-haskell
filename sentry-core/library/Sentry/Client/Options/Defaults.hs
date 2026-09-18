-- | Terminal configuration defaults shared by normalization and runtime projection.
--
-- /This module's API is unstable!/ Use "Sentry.Client.Options" to configure clients.
module Sentry.Client.Options.Defaults where

import Data.Text (Text)

debug :: Bool
debug = False

sampleRate :: Float
sampleRate = 1

environment :: Text
environment = "production"
