-- | Standalone TLS sink for out-of-process transport profiling.
--
-- Runs the testkit's discarding TLS sink (ALPN @h2@ + @http\/1.1@) on a fixed
-- port until killed.  Point @sentry-profile@ at it via
-- @SENTRY_PROFILE_SINK_PORT@ so the sink's CPU\/allocation never lands in the
-- client's profile.
--
-- Usage:
--
-- > sentry-sink <port>
module Main where

import Sentry.TestKit.Sink qualified as Sink
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main =
  getArgs >>= \case
    [arg] | Just port <- readMaybe arg -> do
      putStrLn $ "sentry-sink: TLS sink listening on 127.0.0.1:" <> show port
      Sink.runDiscardingSink port
    _ -> die "usage: sentry-sink <port>"
