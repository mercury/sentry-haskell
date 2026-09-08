module Sentry.Client.Options
  ( -- * ClientOptions
    DsnSource (..),
    Internal.ClientOptions (..),
    Internal.defaultClientOptions,

    -- * Transport provider
    Internal.TransportProvider (..),
  ) where

import Sentry.Client.Options.Dsn (DsnSource (..))
import Sentry.Internal qualified as Internal
