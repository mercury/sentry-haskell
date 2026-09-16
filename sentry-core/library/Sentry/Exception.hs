-- | Exceptions and their ordered chains. See "Sentry.Update" for composition.
module Sentry.Exception
  ( Exception (..),
    Exceptions (..),
    ExceptionUpdate,
    ExceptionsUpdate,
    empty,
    with,
    emptyChain,
    withChain,
    setType,
    setValue,
    setModule,
    setThreadId,
    setStacktrace,
    setOptionalStacktrace,
    unsetStacktrace,
    setMechanism,
    setOptionalMechanism,
    modifyMechanism,
    modifyExistingMechanism,
    unsetMechanism,
    singleton,
    setValues,
    clearValues,
    appendException,
    prependException,
    firstException,
    lastException,
    eachException,
    findException,
    filterExceptions,
  ) where

import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Text (Text)
import Patrol qualified
import Patrol.Type.Exception (Exception (..), empty)
import Patrol.Type.Exception qualified as Patrol.Exception
import Patrol.Type.Exceptions (Exceptions (..))
import Sentry.Collection.Internal (mapWHNF)
import Sentry.Mechanism qualified
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Witch qualified

-- | A pending exception edit.
type ExceptionUpdate :: Type
type ExceptionUpdate = Update Exception

-- | Inspect the exception after preceding edits.
with :: (Witch.From a ExceptionUpdate) => (Exception -> a) -> ExceptionUpdate
with = Sentry.Update.with

-- | Assign the exception type.
setType :: Text -> ExceptionUpdate
setType !assigned = Update \e -> e{Patrol.Exception.type_ = assigned}

-- | Assign the exception value.
setValue :: Text -> ExceptionUpdate
setValue !assigned = Update \e -> e{Patrol.Exception.value = assigned}

-- | Assign the exception module.
setModule :: Text -> ExceptionUpdate
setModule !assigned = Update \e -> e{Patrol.Exception.module_ = assigned}

-- | Assign the exception thread identifier.
setThreadId :: Text -> ExceptionUpdate
setThreadId !assigned = Update \e -> e{Patrol.Exception.threadId = assigned}

-- | Assign a stacktrace record.
setStacktrace :: Patrol.Stacktrace -> ExceptionUpdate
setStacktrace !assigned = Update \e -> e{Patrol.Exception.stacktrace = Just assigned}

-- | Remove the stacktrace.
unsetStacktrace :: ExceptionUpdate
unsetStacktrace = Update \e -> e{Patrol.Exception.stacktrace = Nothing}

-- | Replace the payload from an update, list, or record.
setMechanism :: (Witch.From a Sentry.Mechanism.MechanismUpdate) => a -> ExceptionUpdate
setMechanism upd = Update \e -> let !child = Sentry.Update.run upd Sentry.Mechanism.empty in e{Patrol.Exception.mechanism = Just child}

-- | Modify the payload, creating from empty when absent.
modifyMechanism :: (Witch.From a Sentry.Mechanism.MechanismUpdate) => a -> ExceptionUpdate
modifyMechanism upd = Update \e -> case e.mechanism of
  Nothing -> Sentry.Update.run (setMechanism upd) e
  Just _ -> Sentry.Update.run (modifyExistingMechanism upd) e

-- | Modify a present payload; absence skips the update.
modifyExistingMechanism :: (Witch.From a Sentry.Mechanism.MechanismUpdate) => a -> ExceptionUpdate
modifyExistingMechanism upd = Update \e -> case e.mechanism of
  Nothing -> e
  Just old -> let !child = Sentry.Update.run upd old in e{Patrol.Exception.mechanism = Just child}

-- | Remove the optional payload.
unsetMechanism :: ExceptionUpdate
unsetMechanism = Update \e -> e{Patrol.Exception.mechanism = Nothing}

-- | Updates to the collection; lists of child updates describe one child.
type ExceptionsUpdate :: Type
type ExceptionsUpdate = Update Exceptions

-- | An empty collection wrapper.
emptyChain :: Exceptions
emptyChain = Exceptions []

-- | Inspect the collection after preceding edits.
withChain :: (Witch.From a ExceptionsUpdate) => (Exceptions -> a) -> ExceptionsUpdate
withChain = Sentry.Update.with

-- | Construct one child from an update, update list, or record.
singleton :: (Witch.From a ExceptionUpdate) => a -> Exceptions
singleton upd = let !child = Sentry.Update.run upd empty in Exceptions [child]

-- | Replace the values.
setValues :: (Witch.From a ExceptionUpdate) => [a] -> ExceptionsUpdate
setValues upds = Update \_ ->
  let !result = mapWHNF (\upd -> Sentry.Update.run upd empty) upds
   in Exceptions result

-- | Empty the wrapper without removing it from its parent.
clearValues :: ExceptionsUpdate
clearValues = Update (const emptyChain)

-- | Append a child constructed from empty.
appendException :: (Witch.From a ExceptionUpdate) => a -> ExceptionsUpdate
appendException upd = Update \(Exceptions xs) ->
  let !child = Sentry.Update.run upd empty in Exceptions (xs <> [child])

-- | Prepend a child constructed from empty.
prependException :: (Witch.From a ExceptionUpdate) => a -> ExceptionsUpdate
prependException upd = Update \(Exceptions xs) ->
  let !child = Sentry.Update.run upd empty in Exceptions (child : xs)

-- | Edit the first entry; an empty selection skips the update.
firstException :: (Witch.From a ExceptionUpdate) => a -> ExceptionsUpdate
firstException upd = Update \collection@(Exceptions xs) -> case xs of
  [] -> collection
  x : rest -> let !child = Sentry.Update.run upd x in Exceptions (child : rest)

-- | Edit the last entry without forcing unselected records.
lastException :: (Witch.From a ExceptionUpdate) => a -> ExceptionsUpdate
lastException upd = Update \(Exceptions xs) -> let !result = go xs in Exceptions result
  where
    go [] = []
    go [x] = let !child = Sentry.Update.run upd x in [child]
    go (x : xs) = let !rest = go xs in x : rest

-- | Edit every entry independently.
eachException :: (Witch.From a ExceptionUpdate) => a -> ExceptionsUpdate
eachException upd = Update \(Exceptions xs) ->
  let !result = mapWHNF (Sentry.Update.run upd) xs
   in Exceptions result

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalStacktrace :: Maybe Patrol.Stacktrace -> ExceptionUpdate
setOptionalStacktrace = maybe unsetStacktrace setStacktrace

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalMechanism :: Maybe Sentry.Mechanism.Mechanism -> ExceptionUpdate
setOptionalMechanism = maybe unsetMechanism setMechanism

-- | Return the first matching entry.
findException :: (Exception -> Bool) -> Exceptions -> Maybe Exception
findException predicate collection = Foldable.find predicate collection.values

-- | Keep matching entries in order, preserving duplicates.
filterExceptions :: (Exception -> Bool) -> ExceptionsUpdate
filterExceptions predicate = Update \collection ->
  let !result = filter predicate collection.values in Exceptions result
