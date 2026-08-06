-- | The 'Patrol.Type.Mechanism.Mechanism' type along with smart constructors
-- for the general 'Patrol.Type.Mechanism.Mechanism's this SDK provides:
--
-- @
-- import Sentry.Mechanism qualified as Mechanism
--
-- Sentry.captureExceptionWith def{mechanismOverride = Just Mechanism.generic} e
-- Sentry.captureUnhandledException \"warp.onException\" e
-- @
--
-- Per Sentry's convention, 'Patrol.Type.Mechanism.type_' is a short,
-- lowercase, dot-separated identifier naming the source of the capture;
-- free-form context belongs in 'Patrol.Type.Mechanism.data_' instead.
module Sentry.Mechanism
  ( module Patrol.Type.Mechanism,
    generic,
    unhandled,
  )
where

import Data.Text (Text)
import Patrol.Type.Mechanism

-- | A generic mechanism that 'Sentry.Capture.captureException' attaches to
-- every event it constructs.
generic :: Mechanism
generic = empty{type_ = "generic", handled = Just True}

-- | A generic builder for unhandled exceptions, which accepts a 'Text'
-- argument from which the caller can set its 'Patrol.Type.Mechanism.type_'
-- field.
unhandled :: Text -> Mechanism
unhandled ty = empty{type_ = ty, handled = Just False}
