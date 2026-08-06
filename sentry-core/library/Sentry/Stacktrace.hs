-- | Internal utilities for attaching and classifying stack-trace frames.
module Sentry.Stacktrace
  ( -- * Merge utilities
    mergeCallStackIntoException,
    mergeCallStackIntoThread,

    -- * Per-source extractors
    callStackFromAnnotations,
    callStackFromExceptionContext,

    -- * Frame deduplication
    dedupeFrames,

    -- * In-app classification
    classifyInApp,
    wellKnownNotInApp,
    packageName,
  )
where

import Control.Exception (SomeException)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Kind (Type)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (CallStack)
import Patrol qualified
import Patrol.Type.Event qualified as Patrol.Event
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions qualified as Patrol.Exceptions
import Patrol.Type.Frame qualified as Patrol.Frame
import Patrol.Type.Stacktrace qualified as Patrol.Stacktrace
import Patrol.Type.Thread qualified as Patrol.Thread
import Patrol.Type.Threads qualified as Patrol.Threads

import Control.Exception.Annotated (Annotation)
import Data.Annotation (tryAnnotations)

-- Merge utilities

-- | Merge the frames from a 'CallStack' into the /last/ exception value in
-- the event's @exception.values@ list, which /should/ be the exception that
-- is actually unhandled.
--
-- If the event has no @exception@, it is left untouched (use
-- 'mergeCallStackIntoThread' for message events).
--
-- Frames from @callStack@ are placed first (newer) and deduped against any
-- frames already present (older), so registering multiple sources
-- monotonically enriches without clobbering.
mergeCallStackIntoException :: CallStack -> Patrol.Event -> Patrol.Event
mergeCallStackIntoException callStack event =
  case event.exception of
    Nothing -> event
    Just exceptions ->
      let newStack = Patrol.Stacktrace.fromCallStack callStack
          mergedValues = case unsnoc (Patrol.Exceptions.values exceptions) of
            Nothing -> []
            Just (rest, exc) ->
              let merged = mergeStacktraces newStack (fromMaybe Patrol.Stacktrace.empty exc.stacktrace)
               in rest <> [exc{Patrol.Exception.stacktrace = Just merged}]
       in event
            { Patrol.Event.exception =
                Just exceptions{Patrol.Exceptions.values = mergedValues}
            }

-- | Merge the frames from a 'CallStack' into the @current@ thread entry in
-- the event's @threads@ list, creating one if it does not exist yet.
--
-- Used by message-capture integrations, which have no exception to attach to.
mergeCallStackIntoThread :: CallStack -> Patrol.Event -> Patrol.Event
mergeCallStackIntoThread callStack event =
  let newStack = Patrol.Stacktrace.fromCallStack callStack
      threads = foldMap Patrol.Threads.values event.threads
      -- Find an existing current-thread entry or build a fresh one.
      (updated, found) = foldl' step ([], False) threads
        where
          step (acc, alreadyFound) t
            | not alreadyFound,
              t.current == Just True =
                let merged = mergeStacktraces newStack (fromMaybe Patrol.Stacktrace.empty t.stacktrace)
                 in (t{Patrol.Thread.stacktrace = Just merged} : acc, True)
            | otherwise = (t : acc, alreadyFound)
      finalThreads
        | found = reverse updated
        | otherwise =
            reverse updated
              <> [ Patrol.Thread.empty
                     { Patrol.Thread.current = Just True,
                       Patrol.Thread.stacktrace = Just newStack
                     }
                 ]
   in event
        { Patrol.Event.threads =
            Just Patrol.Threads.Threads{Patrol.Threads.values = finalThreads}
        }

-- | Merge two 'Patrol.Stacktrace.Stacktrace' values, deduplicating frames.
--
-- @new@ frames come first (assumed newer / higher on the logical stack);
-- @old@ frames follow. 'dedupeFrames' retains the first occurrence of each
-- frame by key, so genuinely new frames shadow old ones at the same site.
--
-- @registers@ are combined with right-biased union (@old@ wins on collision,
-- matching the idea that the throw-site has more authoritative register state).
mergeStacktraces :: Patrol.Stacktrace.Stacktrace -> Patrol.Stacktrace.Stacktrace -> Patrol.Stacktrace.Stacktrace
mergeStacktraces new old =
  Patrol.Stacktrace.Stacktrace
    { Patrol.Stacktrace.frames =
        dedupeFrames (Patrol.Stacktrace.frames new <> Patrol.Stacktrace.frames old),
      Patrol.Stacktrace.registers =
        Patrol.Stacktrace.registers new <> Patrol.Stacktrace.registers old
    }

-- Per-source extractors

-- | Extract a 'CallStack' from a list of 'annotated-exception' 'Annotation's.
--
-- Returns the first 'CallStack' found (they are merged by
-- @annotated-exception@ during propagation, so there is usually at most one).
callStackFromAnnotations :: [Annotation] -> Maybe CallStack
callStackFromAnnotations anns =
  listToMaybe (fst (tryAnnotations @CallStack anns))

-- | Extract a 'CallStack' from the GHC 'ExceptionContext' attached to a
-- 'SomeException', via the 'Backtraces' annotation's @HasCallStack@ entry.
--
-- Returns 'Nothing' when the exception carries no 'Backtraces', when the
-- 'HasCallStackBacktrace' mechanism was disabled, or when the stack is empty.
--
-- __Implementation note__: GHC 9.10\/base-4.20 re-exports the 'Backtraces'
-- type from @Control.Exception.Backtrace@ but does /not/ export its record
-- field accessors (they remain in @ghc-internal@).
--
-- Until @base@ exposes a stable API for reading 'Backtraces' fields, this
-- function always returns 'Nothing'.
--
-- The 'Sentry.Integration.Stacktrace.AttachExceptionContextIntegration'
-- integration is therefore a no-op in the current GHC version but is kept as
-- a stub so it can be enabled without a breaking API change when the accessor
-- becomes available.
callStackFromExceptionContext :: SomeException -> Maybe CallStack
callStackFromExceptionContext _exc = Nothing

-- TODO: implement once base/GHC exposes Backtraces field accessors.
-- The intended implementation is:
--   let ctx = someExceptionContext exc
--       bts = getExceptionAnnotations ctx :: [Backtraces]
--   in listToMaybe bts >>= btrHasCallStack

-- Frame deduplication

-- | A frame's deduplication key.
type FrameKey :: Type
type FrameKey = (Text, Text, Text, Maybe Int, Maybe Int)

frameKey :: Patrol.Frame.Frame -> FrameKey
frameKey f = (f.package, f.module_, f.function, f.lineno, f.colno)

-- | Deduplicate a list of 'Patrol.Frame.Frame' values, preserving the order
-- of first occurrences.
--
-- Keyed on @(package, module_, function, lineno, colno)@. Two frames that
-- differ only in one field (e.g. one is missing a @function@ label) will
-- /not/ be merged and may appear as near-duplicates.
dedupeFrames :: [Patrol.Frame.Frame] -> [Patrol.Frame.Frame]
dedupeFrames = reverse . snd . foldl' step (Set.empty, [])
  where
    step :: (Set FrameKey, [Patrol.Frame.Frame]) -> Patrol.Frame.Frame -> (Set FrameKey, [Patrol.Frame.Frame])
    step (seen, acc) frame
      | Set.member k seen = (seen, acc)
      | otherwise = (Set.insert k seen, frame : acc)
      where
        k = frameKey frame

-- In-app classification

-- | Module\/package prefixes that are never considered in-app by default.
--
-- Mirrors @sentry-rust@'s @WELL_KNOWN_NOT_IN_APP@ list, adapted for Haskell
-- package and module naming conventions.
wellKnownNotInApp :: [Text]
wellKnownNotInApp =
  [ "base",
    "ghc-prim",
    "ghc-internal",
    "ghc-bignum",
    "sentry",
    "sentry-core"
  ]

-- | Extract the bare package name from a GHC-style package-id string, e.g.
-- @"sentry-core-0.0.0-inplace"@ or @"text-2.1.1"@, by dropping the trailing
-- version component.
--
-- Splits on @\'-\'@ and keeps segments up to the first digit.
packageName :: Text -> Text
packageName = Text.intercalate "-" . takeWhile (not . startsWithDigit) . Text.splitOn "-"
  where
    startsWithDigit t = maybe False (isDigit . fst) (Text.uncons t)

-- | Classify each frame in an event as in-app or not-in-app according to the
-- 'ClientOptions' include\/exclude sets and the built-in 'wellKnownNotInApp'
-- denylist.
--
-- Classification precedence per frame (skips frames where 'inApp' is already
-- set):
--
--   1. 'inAppInclude' — explicit include wins, marks @inApp = True@.
--   2. 'inAppExclude' — explicit exclude marks @inApp = False@.
--   3. 'wellKnownNotInApp' — built-in denylist marks @inApp = False@.
--
-- After all frames are classified, if /no/ frame ended up @in_app = True@,
-- every frame with @inApp = Nothing@ is marked @True@, which prevents events
-- with an entirely-excluded call stack from losing all in-app context.
classifyInApp :: HashSet Text -> HashSet Text -> Patrol.Event -> Patrol.Event
classifyInApp include exclude event =
  let event' = mapFrames classifyFrame event
      anyInApp = anyFrame (\f -> f.inApp == Just True) event'
   in if anyInApp
        then event'
        else mapFrames promoteNothing event'
  where
    classifyFrame :: Patrol.Frame.Frame -> Patrol.Frame.Frame
    classifyFrame frame
      -- Already explicitly set — leave it alone.
      | Just _ <- frame.inApp = frame
      | matchesPrefix frame include = frame{Patrol.Frame.inApp = Just True}
      | matchesPrefix frame exclude = frame{Patrol.Frame.inApp = Just False}
      | matchesBuiltinDenylist frame = frame{Patrol.Frame.inApp = Just False}
      | otherwise = frame

    -- any_in_app fallback: promote unclassified frames when none are in-app.
    promoteNothing :: Patrol.Frame.Frame -> Patrol.Frame.Frame
    promoteNothing frame
      | frame.inApp == Nothing = frame{Patrol.Frame.inApp = Just True}
      | otherwise = frame

    matchesPrefix :: Patrol.Frame.Frame -> HashSet Text -> Bool
    matchesPrefix frame prefixes =
      any (isPrefixOf frame) (HashSet.toList prefixes)

    matchesBuiltinDenylist :: Patrol.Frame.Frame -> Bool
    matchesBuiltinDenylist frame =
      any (isPrefixOf frame) wellKnownNotInApp

    -- A frame matches prefix @p@ if its @module_@ equals @p@ exactly or
    -- starts with @p <> "."@, or if its @package@'s bare name equals @p@
    -- exactly or starts with @p <> "."@.
    isPrefixOf :: Patrol.Frame.Frame -> Text -> Bool
    isPrefixOf frame p =
      fieldMatches frame.module_ || fieldMatches (packageName frame.package)
      where
        fieldMatches f =
          not (Text.null f)
            && (f == p || Text.isPrefixOf (p <> ".") f)

    -- apply a predicate to each frame in an event, short-circuiting on the
    -- first match.
    anyFrame :: (Patrol.Frame.Frame -> Bool) -> Patrol.Event -> Bool
    anyFrame p ev =
      any (anyStack p . Patrol.Exception.stacktrace) (foldMap Patrol.Exceptions.values ev.exception)
        || any (anyStack p . Patrol.Thread.stacktrace) (foldMap Patrol.Threads.values ev.threads)
      where
        anyStack q = any q . foldMap Patrol.Stacktrace.frames

-- | Map a function over every 'Patrol.Frame.Frame' in all stacktraces in an
-- event (both @exception.values[].stacktrace@ and @threads[].stacktrace@).
mapFrames :: (Patrol.Frame.Frame -> Patrol.Frame.Frame) -> Patrol.Event -> Patrol.Event
mapFrames f ev =
  ev
    { Patrol.Event.exception =
        ev.exception <&> \excs ->
          excs{Patrol.Exceptions.values = map mapExc (Patrol.Exceptions.values excs)},
      Patrol.Event.threads =
        ev.threads <&> \threads ->
          threads{Patrol.Threads.values = map mapThread (Patrol.Threads.values threads)}
    }
  where
    mapExc e = e{Patrol.Exception.stacktrace = fmap mapStackTrace e.stacktrace}
    mapThread t = t{Patrol.Thread.stacktrace = fmap mapStackTrace t.stacktrace}
    mapStackTrace st = st{Patrol.Stacktrace.frames = map f (Patrol.Stacktrace.frames st)}
