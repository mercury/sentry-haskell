module Sentry.Client.Options
  ( -- * ClientOptions
    Internal.ClientOptions (..),
    pattern Internal.DEFAULT_CLIENT_OPTIONS,

    -- * Transport provider
    Internal.TransportProvider (..),
  ) where

import Sentry.Internal qualified as Internal
