module Sentry.Client.Options
  ( -- * ClientOptions
    Internal.ClientOptions (..),
    pattern Internal.DEFAULT_CLIENT_OPTIONS,
    Internal.BeforeCallback,
  ) where

import Sentry.Internal qualified as Internal
