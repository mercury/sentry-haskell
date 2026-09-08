module Sentry.Client.Options
  ( -- * ClientOptions
    DsnSource (..),
    Internal.ClientOptions (..),
    pattern Internal.DEFAULT_CLIENT_OPTIONS,

    -- * Transport provider
    Internal.TransportProvider (..),
  ) where

import Sentry.Client.Options.Dsn (DsnSource (..))
import Sentry.Internal qualified as Internal
