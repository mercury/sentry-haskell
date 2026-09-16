-- | Apply updates to 'Sentry.Scope.Internal.ScopeData' via the composable
-- 'ScopeUpdate' value.
--
-- 'ScopeUpdate's are built from the named smart constructors below and combined
-- with '<>'; applying one to a 'Scope' is a single atomic update. The left
-- operand of '<>' is applied first, so later updates win on conflicting scalar
-- fields:
--
-- @
-- import Sentry.Scope.Update qualified as Update
--
-- Update.apply scope $
--   Update.setLevel Warning
--     <> Update.setTag \"env\" \"prod\"
--     <> Update.setUser u
-- @
--
-- A list is accepted anywhere a single update is, so the same thing reads:
--
-- @
-- Update.apply scope
--   [ Update.setLevel Warning,
--     Update.setTag \"env\" \"prod\",
--     Update.setUser u
--   ]
-- @
--
-- Update bundles can be factored out and reused:
--
-- @
-- let stagingTags = Update.setTag \"env\" \"staging\" <> Update.setTag \"tier\" \"free\"
-- scope `Update.apply` (stagingTags <> Update.setUser u)
-- @
--
-- Optional setters accept concrete values: 'Just' replaces the local assignment
-- and 'Nothing' removes it. Removal may reveal inherited metadata; assigning
-- an empty record or collection retains a present local override.
--
-- This is the same 'Sentry.Update.Update' the builder modules use; see
-- "Sentry.Update" for the shared composition rules.
module Sentry.Scope.Update
  ( -- * Type
    ScopeUpdate,

    -- * Application
    apply,

    -- * Smart constructors

    -- ** Scalar fields
    setLevel,
    setOptionalLevel,
    unsetLevel,
    setUser,
    setOptionalUser,
    unsetUser,
    modifyUser,
    modifyExistingUser,
    defaultFingerprintComponent,
    prependFingerprintComponent,
    ensureDefaultFingerprint,
    removeDefaultFingerprint,
    modifyFingerprint,
    setFingerprint,
    setOptionalFingerprint,
    unsetFingerprint,
    appendFingerprintComponent,
    clearFingerprint,
    modifyExistingFingerprint,
    setTransaction,
    setOptionalTransaction,
    unsetTransaction,

    -- ** Tags
    setTag,
    setOptionalTag,
    setTagIfAbsent,
    removeTag,
    clearTags,

    -- ** Extras
    setExtra,
    setOptionalExtra,
    removeExtra,
    clearExtras,

    -- ** Contexts
    setContext,
    setOptionalContext,
    setContextIfAbsent,
    setBrowserContext,
    setOptionalBrowserContext,
    modifyBrowserContext,
    modifyExistingBrowserContext,
    setDeviceContext,
    setOptionalDeviceContext,
    modifyDeviceContext,
    modifyExistingDeviceContext,
    setTraceContext,
    setOptionalTraceContext,
    modifyTraceContext,
    modifyExistingTraceContext,
    setOsContext,
    setOptionalOsContext,
    modifyOsContext,
    modifyExistingOsContext,
    setAppContext,
    setOptionalAppContext,
    modifyAppContext,
    modifyExistingAppContext,
    setRuntimeContext,
    setOptionalRuntimeContext,
    modifyRuntimeContext,
    modifyExistingRuntimeContext,
    setContextValues,
    setOptionalContextValues,
    setContextValue,
    setOptionalContextValue,
    removeContextValue,
    modifyContextValues,
    removeContext,
    clearContexts,

    -- ** Breadcrumbs
    appendBreadcrumb,
    appendBreadcrumbs,
    prependBreadcrumb,
    firstBreadcrumb,
    lastBreadcrumb,
    eachBreadcrumb,
    setBreadcrumbs,
    modifyBreadcrumbs,
    clearBreadcrumbs,
    trimBreadcrumbs,

    -- ** Event processors
    setEventProcessor,
    addEventProcessor,
    unsetEventProcessor,
    lookupTag,
    lookupExtra,
    lookupContext,
    with,
    lookupAppContext,
    alterAppContext,
    lookupOsContext,
    alterOsContext,
    lookupRuntimeContext,
    alterRuntimeContext,
    lookupBrowserContext,
    alterBrowserContext,
    lookupDeviceContext,
    alterDeviceContext,
    lookupTraceContext,
    alterTraceContext,
    lookupContextValue,
    modifyExistingContextValue,
    alterContextValue,
    findFingerprintComponent,
    filterFingerprint,
    findBreadcrumb,
    filterBreadcrumbs,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Patrol qualified
import Patrol.Type.Context qualified as Patrol.Context
import Sentry.AppContext qualified
import Sentry.Breadcrumb (BreadcrumbUpdate)
import Sentry.Breadcrumb qualified
import Sentry.BrowserContext qualified
import Sentry.Context.Internal qualified
import Sentry.DeviceContext qualified
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Fingerprint.Internal (defaultFingerprintComponent)
import Sentry.Fingerprint.Internal qualified as Fingerprint
import Sentry.OsContext qualified
import Sentry.RuntimeContext (RuntimeContextUpdate)
import Sentry.RuntimeContext qualified
import Sentry.Scope.Internal (Scope, ScopeData (..), modifyScopeData)
import Sentry.TraceContext qualified
import Sentry.Update (Update (..))
import Sentry.Update qualified
import Sentry.User (UserUpdate)
import Sentry.User qualified
import Witch qualified

-- | A pending modification to a 'Scope'.
--
-- Updates can be chained with '<>' or collected in a list; the left operand is
-- applied first, so later updates win on conflicting scalar fields. Build one
-- with a smart constructor below; recover the wrapped
-- @'ScopeData' -> 'ScopeData'@ with 'Sentry.Update.runUpdate', or run it
-- against a base with 'Sentry.Update.run' — @'Witch.from' upd@ (or
-- @'Sentry.Update.run' upd 'Sentry.Update.empty'@) yields the 'ScopeData' the
-- update produces against an empty scope, the same as every other record.
type ScopeUpdate :: Type
type ScopeUpdate = Update ScopeData

-- | Apply an update to a 'Scope' as a single atomic 'IORef' update.
--
-- Accepts a single 'ScopeUpdate' or a list of them.
apply :: (MonadIO m, Witch.From a ScopeUpdate) => Scope -> a -> m ()
apply scope = modifyScopeData scope . Sentry.Update.run

-- | Internal builder shared by the smart constructors below.
edit :: (ScopeData -> ScopeData) -> ScopeUpdate
edit = Update

-- * Scalar field updates

-- | Set the 'Patrol.Type.Level.Level'.
setLevel :: Patrol.Level -> ScopeUpdate
setLevel l = edit \s -> s{level = Just l}

-- | Clear the 'Patrol.Type.Level.Level'.
unsetLevel :: ScopeUpdate
unsetLevel = edit \s -> s{level = Nothing}

-- | Establish the user on this scope from an empty one, replacing any user it
-- already had.
--
-- Accepts a 'Sentry.User.UserUpdate', a list of them, or a
-- 'Sentry.User.User' you already hold. Use 'modifyUser' to refine a user this
-- scope already has, or create one when absent.
--
-- The new value is forced before it is stored, so the update runs as part of
-- this single atomic scope modification rather than being retained as a thunk.
setUser :: (Witch.From a UserUpdate) => a -> ScopeUpdate
setUser upd =
  edit \s -> let !u = Sentry.Update.run upd Sentry.User.empty in s{user = Just u}

-- | This update replaces the local user with a supplied record, or removes the
-- assignment when given 'Nothing'. Removing it allows an inherited user to
-- appear in captures; 'Just' an empty user retains a local override.
setOptionalUser :: Maybe Sentry.User.User -> ScopeUpdate
setOptionalUser = maybe unsetUser setUser

-- | Remove the local user override.
unsetUser :: ScopeUpdate
unsetUser = edit \s -> s{user = Nothing}

-- | Modify the user, starting from empty when absent.
modifyUser :: (Witch.From a UserUpdate) => a -> ScopeUpdate
modifyUser upd = edit \s -> case s.user of
  Nothing -> Sentry.Update.run (setUser upd) s
  Just _ -> Sentry.Update.run (modifyExistingUser upd) s

-- | Modify an existing user.
modifyExistingUser :: (Witch.From a UserUpdate) => a -> ScopeUpdate
modifyExistingUser upd = edit \s -> case s.user of
  Nothing -> s
  Just u -> let !u' = Sentry.Update.run upd u in s{user = Just u'}

-- | Replace the local fingerprint.
setFingerprint :: [Text] -> ScopeUpdate
setFingerprint !fp = edit \s -> s{fingerprint = Just fp}

-- | Remove the local assignment.
unsetFingerprint :: ScopeUpdate
unsetFingerprint = edit \s -> s{fingerprint = Nothing}

-- | Transform the complete local list once, starting from [] when absent.
modifyFingerprint :: ([Text] -> [Text]) -> ScopeUpdate
modifyFingerprint f = edit \s -> let !result = f (maybe [] id s.fingerprint) in s{fingerprint = Just result}

-- | Skip absent fingerprints without evaluating the function.
modifyExistingFingerprint :: ([Text] -> [Text]) -> ScopeUpdate
modifyExistingFingerprint f = edit \s -> case s.fingerprint of
  Nothing -> s
  Just fp -> let !result = f fp in s{fingerprint = Just result}

-- | Append locally, creating when absent, preserving order and duplicates.
appendFingerprintComponent :: Text -> ScopeUpdate
appendFingerprintComponent !value = modifyFingerprint (Fingerprint.append value)

-- | Prepend locally, creating when absent, preserving duplicates.
prependFingerprintComponent :: Text -> ScopeUpdate
prependFingerprintComponent !value = modifyFingerprint (Fingerprint.prepend value)

-- | Ensure a default token locally, creating when absent. Existing tokens
-- retain their position and spelling; otherwise prepend the canonical token.
ensureDefaultFingerprint :: ScopeUpdate
ensureDefaultFingerprint = modifyFingerprint Fingerprint.ensureDefault

-- | Remove all recognized default tokens, skipping absence. Removing the last
-- component retains a present empty list.
removeDefaultFingerprint :: ScopeUpdate
removeDefaultFingerprint = modifyExistingFingerprint Fingerprint.removeDefault

-- | Store Just [], overriding lower scope layers, but not custom Event grouping.
clearFingerprint :: ScopeUpdate
clearFingerprint = setFingerprint []

-- | Set the transaction name.
setTransaction :: Text -> ScopeUpdate
setTransaction t = edit \s -> s{transaction = Just t}

-- | Clear the transaction name.
unsetTransaction :: ScopeUpdate
unsetTransaction = edit \s -> s{transaction = Nothing}

-- * Tag updates

-- | Insert (or overwrite) a tag at the given key.
setTag :: Text -> Text -> ScopeUpdate
setTag k v = edit \s -> s{tags = Map.insert k v s.tags}

-- | Insert a tag only when its key is absent.
setTagIfAbsent :: Text -> Text -> ScopeUpdate
setTagIfAbsent !key value = edit \s ->
  if Map.member key s.tags
    then s
    else let !result = Map.insert key value s.tags in s{tags = result}

-- | Remove the tag at the given key, if present.
removeTag :: Text -> ScopeUpdate
removeTag k = edit \s -> s{tags = Map.delete k s.tags}

-- | Clear all tags.
clearTags :: ScopeUpdate
clearTags = edit \s -> s{tags = Map.empty}

-- * Extra updates

-- | Insert (or overwrite) an extra value at the given key.
setExtra :: Text -> Aeson.Value -> ScopeUpdate
setExtra k v = edit \s -> s{extras = Map.insert k v s.extras}

-- | Remove the extra value at the given key, if present.
removeExtra :: Text -> ScopeUpdate
removeExtra k = edit \s -> s{extras = Map.delete k s.extras}

-- | Clear all extras.
clearExtras :: ScopeUpdate
clearExtras = edit \s -> s{extras = Map.empty}

-- * Context updates

-- | Insert (or overwrite) a context at the given key.
setContext :: Text -> Patrol.Context -> ScopeUpdate
setContext k v = edit \s -> s{contexts = Map.insert k v s.contexts}

-- | Insert a context only when its key is absent.
setContextIfAbsent :: Text -> Patrol.Context -> ScopeUpdate
setContextIfAbsent !key value = edit \s ->
  if Map.member key s.contexts
    then s
    else let !result = Map.insert key value s.contexts in s{contexts = result}

-- | Replace the context at @\"runtime\"@ with a runtime context, including
-- any custom context previously stored at that key.
--
-- Accepts a 'Sentry.RuntimeContext.RuntimeContextUpdate', a list of them, or a
-- 'Sentry.RuntimeContext.RuntimeContext' you already hold.
setRuntimeContext :: (Witch.From a RuntimeContextUpdate) => a -> ScopeUpdate
setRuntimeContext upd =
  let !rc = Sentry.Update.run upd Sentry.RuntimeContext.empty
   in setContext "runtime" (Patrol.Context.Runtime rc)

-- | Replace the context at the given key with a custom context built from
-- key/value pairs. Later entries win when a key occurs more than once.
--
-- An empty list stores an empty context, serialized as @{}@ at that key.
-- It can shadow an inherited context; use 'removeContext' to remove the entry
-- from this scope.
setContextValues :: Text -> [(Text, Aeson.Value)] -> ScopeUpdate
setContextValues k kvs = let !values = Map.fromList kvs in setContext k (Patrol.Context.Other values)

-- | Insert (or overwrite) a single value inside the custom context at the
-- given key, leaving its other entries alone.
--
-- See 'modifyContextValues' for what happens when the context is absent or is
-- a typed variant.
setContextValue :: Text -> Text -> Aeson.Value -> ScopeUpdate
setContextValue k key value = modifyContextValues k (Map.insert key value)

-- | Remove a single value from the custom context at the given key, leaving
-- its other entries alone. An absent local context stays absent; removing
-- its last field retains an empty context. Typed contexts are unchanged.
removeContextValue :: Text -> Text -> ScopeUpdate
removeContextValue k key = edit \s -> let !result = Sentry.Context.Internal.removeValue k key s.contexts in s{contexts = result}

-- | Transform the key/value payload of the custom context at the given key.
--
-- Two cases are worth stating outright:
--
-- * When this scope has no context at that key, the function runs against an
--   empty map and its result is stored. As with 'setContextValues', that
--   shadows any context inherited from an outer scope rather than reaching
--   through to it.
-- * When the entry is a typed variant ('Patrol.Type.Context.Runtime',
--   'Patrol.Type.Context.Os', …) rather than
--   'Patrol.Type.Context.Other', this changes nothing and the function is
--   never applied. Use 'setContextValues' to replace it outright.
modifyContextValues :: Text -> (Map Text Aeson.Value -> Map Text Aeson.Value) -> ScopeUpdate
modifyContextValues k f = edit \s -> let !result = Sentry.Context.Internal.modifyValues k f s.contexts in s{contexts = result}

-- | Remove the local context override.
removeContext :: Text -> ScopeUpdate
removeContext k = edit \s -> s{contexts = Map.delete k s.contexts}

-- | Clear all contexts.
clearContexts :: ScopeUpdate
clearContexts = edit \s -> s{contexts = Map.empty}

-- * Breadcrumb updates

-- | Append a 'Sentry.Breadcrumb.Breadcrumb' verbatim.
--
-- Accepts a 'Sentry.Breadcrumb.BreadcrumbUpdate', a list of them, or a whole
-- 'Sentry.Breadcrumb.Breadcrumb'; an update is run against
-- 'Patrol.Type.Breadcrumb.empty', since a breadcrumb is appended rather than
-- refined.
--
-- This is the pure mutation primitive: it does /not/ default the timestamp, run
-- 'Sentry.Client.Options.ClientOptions.beforeBreadcrumb', or trim to
-- 'Sentry.Client.Options.ClientOptions.maxBreadcrumbs'. Use
-- 'Sentry.addBreadcrumb' for the policy-applying, timestamp-defaulting
-- entry point.
appendBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> ScopeUpdate
appendBreadcrumb upd = edit \s ->
  let !crumb = Sentry.Update.run upd Sentry.Breadcrumb.empty
   in s{breadcrumbs = s.breadcrumbs Seq.|> crumb}

-- | Append several 'Sentry.Breadcrumb.Breadcrumb's verbatim, in order. See
-- 'appendBreadcrumb' for the caveats around policy and defaulting.
appendBreadcrumbs :: (Foldable f) => f Patrol.Breadcrumb -> ScopeUpdate
appendBreadcrumbs crumbs = edit \s -> s{breadcrumbs = s.breadcrumbs <> Seq.fromList (toList crumbs)}

-- | Clear all breadcrumbs.
clearBreadcrumbs :: ScopeUpdate
clearBreadcrumbs = edit \s -> s{breadcrumbs = mempty}

-- | Drop the oldest breadcrumbs so that at most @n@ remain. A non-positive @n@
-- clears them entirely.
trimBreadcrumbs :: Int -> ScopeUpdate
trimBreadcrumbs n = edit \s ->
  let len = Seq.length s.breadcrumbs
   in if len > n then s{breadcrumbs = Seq.drop (len - n) s.breadcrumbs} else s

-- * Event-processor updates

-- | Replace the scope's event processor with the given function.
--
-- Return the resulting event, or 'Nothing' to drop it.
setEventProcessor :: (CapturedEvent -> Maybe Patrol.Event) -> ScopeUpdate
setEventProcessor f = edit \s -> s{eventProcessor = f}

-- | Chain a new processor after the existing one.
--
-- The existing processor runs first, and the new processor sees its effect
-- as the next input event. If the existing
-- processor drops the event ('Nothing'), the new processor is not called.
-- Matches the left-to-right chaining of the 'ScopeData' 'Semigroup'.
addEventProcessor :: (CapturedEvent -> Maybe Patrol.Event) -> ScopeUpdate
addEventProcessor g = edit \s ->
  s
    { eventProcessor = \ce -> do
        event <- s.eventProcessor ce
        g ce{event}
    }

-- | Reset the scope's event processor to the default pass-through (no filtering
-- or mutation).
unsetEventProcessor :: ScopeUpdate
unsetEventProcessor = edit \s -> s{eventProcessor = \ce -> Just ce.event}

-- | Replace the entire @"os"@ payload from an update, list, or record.
setOsContext :: (Witch.From a Sentry.OsContext.OsContextUpdate) => a -> ScopeUpdate
setOsContext upd = let !record = Sentry.Update.run upd Sentry.OsContext.empty in setContext "os" (Patrol.Context.Os record)

-- | Replace the entire @"app"@ payload from an update, list, or record.
setAppContext :: (Witch.From a Sentry.AppContext.AppContextUpdate) => a -> ScopeUpdate
setAppContext upd = let !record = Sentry.Update.run upd Sentry.AppContext.empty in setContext "app" (Patrol.Context.App record)

-- | Modify the local OS context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyOsContext :: (Witch.From a Sentry.OsContext.OsContextUpdate) => a -> ScopeUpdate
modifyOsContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped True "os" Sentry.OsContext.empty project Patrol.Context.Os (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Os record) = Just record
    project _ = Nothing

-- | Modify the local OS context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingOsContext :: (Witch.From a Sentry.OsContext.OsContextUpdate) => a -> ScopeUpdate
modifyExistingOsContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped False "os" Sentry.OsContext.empty project Patrol.Context.Os (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Os record) = Just record
    project _ = Nothing

-- | Modify the local app context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyAppContext :: (Witch.From a Sentry.AppContext.AppContextUpdate) => a -> ScopeUpdate
modifyAppContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped True "app" Sentry.AppContext.empty project Patrol.Context.App (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.App record) = Just record
    project _ = Nothing

-- | Modify the local app context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingAppContext :: (Witch.From a Sentry.AppContext.AppContextUpdate) => a -> ScopeUpdate
modifyExistingAppContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped False "app" Sentry.AppContext.empty project Patrol.Context.App (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.App record) = Just record
    project _ = Nothing

-- | Modify the local runtime context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyRuntimeContext :: (Witch.From a Sentry.RuntimeContext.RuntimeContextUpdate) => a -> ScopeUpdate
modifyRuntimeContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped True "runtime" Sentry.RuntimeContext.empty project Patrol.Context.Runtime (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Runtime record) = Just record
    project _ = Nothing

-- | Modify the local runtime context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingRuntimeContext :: (Witch.From a Sentry.RuntimeContext.RuntimeContextUpdate) => a -> ScopeUpdate
modifyExistingRuntimeContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped False "runtime" Sentry.RuntimeContext.empty project Patrol.Context.Runtime (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Runtime record) = Just record
    project _ = Nothing

-- | Replace the entire @"browser"@ payload from an update, list, or record.
setBrowserContext :: (Witch.From a Sentry.BrowserContext.BrowserContextUpdate) => a -> ScopeUpdate
setBrowserContext upd = let !record = Sentry.Update.run upd Sentry.BrowserContext.empty in setContext "browser" (Patrol.Context.Browser record)

-- | Modify the local browser context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyBrowserContext :: (Witch.From a Sentry.BrowserContext.BrowserContextUpdate) => a -> ScopeUpdate
modifyBrowserContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped True "browser" Sentry.BrowserContext.empty project Patrol.Context.Browser (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Browser record) = Just record
    project _ = Nothing

-- | Modify the local browser context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingBrowserContext :: (Witch.From a Sentry.BrowserContext.BrowserContextUpdate) => a -> ScopeUpdate
modifyExistingBrowserContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped False "browser" Sentry.BrowserContext.empty project Patrol.Context.Browser (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Browser record) = Just record
    project _ = Nothing

-- | Replace the entire @"device"@ payload from an update, list, or record.
setDeviceContext :: (Witch.From a Sentry.DeviceContext.DeviceContextUpdate) => a -> ScopeUpdate
setDeviceContext upd = let !record = Sentry.Update.run upd Sentry.DeviceContext.empty in setContext "device" (Patrol.Context.Device record)

-- | Modify the local device context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyDeviceContext :: (Witch.From a Sentry.DeviceContext.DeviceContextUpdate) => a -> ScopeUpdate
modifyDeviceContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped True "device" Sentry.DeviceContext.empty project Patrol.Context.Device (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Device record) = Just record
    project _ = Nothing

-- | Modify the local device context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingDeviceContext :: (Witch.From a Sentry.DeviceContext.DeviceContextUpdate) => a -> ScopeUpdate
modifyExistingDeviceContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped False "device" Sentry.DeviceContext.empty project Patrol.Context.Device (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Device record) = Just record
    project _ = Nothing

-- | Replace the entire @"trace"@ payload from an update, list, or record.
setTraceContext :: (Witch.From a Sentry.TraceContext.TraceContextUpdate) => a -> ScopeUpdate
setTraceContext upd = let !record = Sentry.Update.run upd Sentry.TraceContext.empty in setContext "trace" (Patrol.Context.Trace record)

-- | Modify the local trace context, starting from empty when absent.
-- Other context variants are unchanged without evaluating the update.
modifyTraceContext :: (Witch.From a Sentry.TraceContext.TraceContextUpdate) => a -> ScopeUpdate
modifyTraceContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped True "trace" Sentry.TraceContext.empty project Patrol.Context.Trace (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Trace record) = Just record
    project _ = Nothing

-- | Modify the local trace context only when present; absent contexts skip the update.
-- Other context variants are unchanged without evaluating the update.
modifyExistingTraceContext :: (Witch.From a Sentry.TraceContext.TraceContextUpdate) => a -> ScopeUpdate
modifyExistingTraceContext upd = edit \s ->
  let !result = Sentry.Context.Internal.modifyTyped False "trace" Sentry.TraceContext.empty project Patrol.Context.Trace (Sentry.Update.run upd) s.contexts
   in s{contexts = result}
  where
    project (Patrol.Context.Trace record) = Just record
    project _ = Nothing

-- | Replace local breadcrumbs. Does not run capture policies.
setBreadcrumbs :: (Witch.From a Sentry.Breadcrumb.BreadcrumbsUpdate) => a -> ScopeUpdate
setBreadcrumbs upd = edit \s ->
  let !collection = Sentry.Update.run upd Sentry.Breadcrumb.emptyCollection
      !result = Seq.fromList collection.values
   in s{breadcrumbs = result}

-- | Edit only local breadcrumbs, without hooks, timestamps, or retention.
modifyBreadcrumbs :: (Witch.From a Sentry.Breadcrumb.BreadcrumbsUpdate) => a -> ScopeUpdate
modifyBreadcrumbs upd = edit \s ->
  let !collection = Sentry.Update.run upd (Sentry.Breadcrumb.Breadcrumbs (toList s.breadcrumbs))
      !result = Seq.fromList collection.values
   in s{breadcrumbs = result}

-- | Prepend local breadcrumb edit; no capture policies are run.
prependBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> ScopeUpdate
prependBreadcrumb = modifyBreadcrumbs . Sentry.Breadcrumb.prependBreadcrumb

-- | First local breadcrumb edit; no capture policies are run.
firstBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> ScopeUpdate
firstBreadcrumb = modifyBreadcrumbs . Sentry.Breadcrumb.firstBreadcrumb

-- | Last local breadcrumb edit; no capture policies are run.
lastBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> ScopeUpdate
lastBreadcrumb = modifyBreadcrumbs . Sentry.Breadcrumb.lastBreadcrumb

-- | Each local breadcrumb edit; no capture policies are run.
eachBreadcrumb :: (Witch.From a BreadcrumbUpdate) => a -> ScopeUpdate
eachBreadcrumb = modifyBreadcrumbs . Sentry.Breadcrumb.eachBreadcrumb

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalLevel :: Maybe Patrol.Level -> ScopeUpdate
setOptionalLevel = maybe unsetLevel setLevel

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalFingerprint :: Maybe [Text] -> ScopeUpdate
setOptionalFingerprint = maybe unsetFingerprint setFingerprint

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalTransaction :: Maybe Text -> ScopeUpdate
setOptionalTransaction = maybe unsetTransaction setTransaction

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalTag :: Text -> Maybe Text -> ScopeUpdate
setOptionalTag key = maybe (removeTag key) (setTag key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalExtra :: Text -> Maybe Aeson.Value -> ScopeUpdate
setOptionalExtra key = maybe (removeExtra key) (setExtra key)

-- | Replace this assignment with 'Just' a value, or remove it with 'Nothing'.
setOptionalContext :: Text -> Maybe Patrol.Context -> ScopeUpdate
setOptionalContext key = maybe (removeContext key) (setContext key)

-- | Replace the @"runtime"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalRuntimeContext :: Maybe Sentry.RuntimeContext.RuntimeContext -> ScopeUpdate
setOptionalRuntimeContext = maybe (removeContext "runtime") setRuntimeContext

-- | Replace a named context with custom values, or remove it with 'Nothing'.
-- 'Just' an empty list retains a present empty custom context.
setOptionalContextValues :: Text -> Maybe [(Text, Aeson.Value)] -> ScopeUpdate
setOptionalContextValues key = maybe (removeContext key) (setContextValues key)

-- | Assign or remove a custom-context field. Typed payloads are unchanged.
-- Removal never creates a context; deleting its last field retains it.
setOptionalContextValue :: Text -> Text -> Maybe Aeson.Value -> ScopeUpdate
setOptionalContextValue key fieldKey = maybe (removeContextValue key fieldKey) (setContextValue key fieldKey)

-- | Replace the @"os"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalOsContext :: Maybe Sentry.OsContext.OsContext -> ScopeUpdate
setOptionalOsContext = maybe (removeContext "os") setOsContext

-- | Replace the @"app"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalAppContext :: Maybe Sentry.AppContext.AppContext -> ScopeUpdate
setOptionalAppContext = maybe (removeContext "app") setAppContext

-- | Replace the @"browser"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalBrowserContext :: Maybe Sentry.BrowserContext.BrowserContext -> ScopeUpdate
setOptionalBrowserContext = maybe (removeContext "browser") setBrowserContext

-- | Replace the @"device"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalDeviceContext :: Maybe Sentry.DeviceContext.DeviceContext -> ScopeUpdate
setOptionalDeviceContext = maybe (removeContext "device") setDeviceContext

-- | Replace the @"trace"@ context with the supplied record, or remove the
-- entry with 'Nothing', regardless of its previous variant.
setOptionalTraceContext :: Maybe Sentry.TraceContext.TraceContext -> ScopeUpdate
setOptionalTraceContext = maybe (removeContext "trace") setTraceContext

-- | Look up a stored entry by key.
lookupTag :: Text -> ScopeData -> Maybe Text
lookupTag key record = Map.lookup key record.tags

-- | Look up a stored entry by key.
lookupExtra :: Text -> ScopeData -> Maybe Aeson.Value
lookupExtra key record = Map.lookup key record.extras

-- | Look up a stored entry by key.
lookupContext :: Text -> ScopeData -> Maybe Patrol.Context
lookupContext key record = Map.lookup key record.contexts

-- | Observe preceding edits to local metadata within the same atomic update.
-- Inherited metadata is not included. Use an optional setter to transform a
-- keyed assignment, including insertion or removal:
--
-- @
-- Sentry.Scope.with \\local ->
--   Sentry.Scope.setOptionalTag "region"
--     (fmap Text.toUpper (Sentry.Scope.lookupTag "region" local))
-- @
--
-- This example assumes qualified imports of @Sentry.Scope@ and @Data.Text as Text@.
-- Replacing @fmap Text.toUpper@ with a @Maybe Text -> Maybe Text@ function
-- also allows insertion and removal. Removing a local assignment may reveal
-- inherited metadata when the layers are merged.
with :: (Witch.From a ScopeUpdate) => (ScopeData -> a) -> ScopeUpdate
with = Sentry.Update.with

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupAppContext :: ScopeData -> Maybe Sentry.AppContext.AppContext
lookupAppContext record = case Map.lookup "app" record.contexts of
  Just (Patrol.Context.App value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterAppContext :: (Maybe Sentry.AppContext.AppContext -> Maybe Sentry.AppContext.AppContext) -> ScopeUpdate
alterAppContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "app" project Patrol.Context.App f record.contexts
   in record{contexts = result}
  where
    project (Patrol.Context.App value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupOsContext :: ScopeData -> Maybe Sentry.OsContext.OsContext
lookupOsContext record = case Map.lookup "os" record.contexts of
  Just (Patrol.Context.Os value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterOsContext :: (Maybe Sentry.OsContext.OsContext -> Maybe Sentry.OsContext.OsContext) -> ScopeUpdate
alterOsContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "os" project Patrol.Context.Os f record.contexts
   in record{contexts = result}
  where
    project (Patrol.Context.Os value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupRuntimeContext :: ScopeData -> Maybe Sentry.RuntimeContext.RuntimeContext
lookupRuntimeContext record = case Map.lookup "runtime" record.contexts of
  Just (Patrol.Context.Runtime value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterRuntimeContext :: (Maybe Sentry.RuntimeContext.RuntimeContext -> Maybe Sentry.RuntimeContext.RuntimeContext) -> ScopeUpdate
alterRuntimeContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "runtime" project Patrol.Context.Runtime f record.contexts
   in record{contexts = result}
  where
    project (Patrol.Context.Runtime value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupBrowserContext :: ScopeData -> Maybe Sentry.BrowserContext.BrowserContext
lookupBrowserContext record = case Map.lookup "browser" record.contexts of
  Just (Patrol.Context.Browser value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterBrowserContext :: (Maybe Sentry.BrowserContext.BrowserContext -> Maybe Sentry.BrowserContext.BrowserContext) -> ScopeUpdate
alterBrowserContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "browser" project Patrol.Context.Browser f record.contexts
   in record{contexts = result}
  where
    project (Patrol.Context.Browser value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupDeviceContext :: ScopeData -> Maybe Sentry.DeviceContext.DeviceContext
lookupDeviceContext record = case Map.lookup "device" record.contexts of
  Just (Patrol.Context.Device value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterDeviceContext :: (Maybe Sentry.DeviceContext.DeviceContext -> Maybe Sentry.DeviceContext.DeviceContext) -> ScopeUpdate
alterDeviceContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "device" project Patrol.Context.Device f record.contexts
   in record{contexts = result}
  where
    project (Patrol.Context.Device value) = Just value
    project _ = Nothing

-- | Return the canonical typed context, or 'Nothing' for absence or a mismatch.
lookupTraceContext :: ScopeData -> Maybe Sentry.TraceContext.TraceContext
lookupTraceContext record = case Map.lookup "trace" record.contexts of
  Just (Patrol.Context.Trace value) -> Just value
  _ -> Nothing

-- | Alter an absent or matching context. Mismatches skip the callback;
-- use an explicit setter to replace a different variant.
alterTraceContext :: (Maybe Sentry.TraceContext.TraceContext -> Maybe Sentry.TraceContext.TraceContext) -> ScopeUpdate
alterTraceContext f = Update \record ->
  let !result = Sentry.Context.Internal.alterTyped "trace" project Patrol.Context.Trace f record.contexts
   in record{contexts = result}
  where
    project (Patrol.Context.Trace value) = Just value
    project _ = Nothing

-- | Look up a custom field; typed payloads have no custom fields.
lookupContextValue :: Text -> Text -> ScopeData -> Maybe Aeson.Value
lookupContextValue key field record = Sentry.Context.Internal.lookupValue key field record.contexts

-- | Transform a present custom field. Missing fields and typed payloads skip the callback.
modifyExistingContextValue :: Text -> Text -> (Aeson.Value -> Aeson.Value) -> ScopeUpdate
modifyExistingContextValue key field f = alterContextValue key field (fmap f)

-- | Insert, replace, or remove a custom field. Typed payloads skip the callback.
-- Removing the final field retains an empty context; absent removals create nothing.
alterContextValue :: Text -> Text -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> ScopeUpdate
alterContextValue key field f = Update \record ->
  let !result = Sentry.Context.Internal.alterValue key field f record.contexts
   in record{contexts = result}

-- | Return the first matching fingerprint component.
findFingerprintComponent :: (Text -> Bool) -> ScopeData -> Maybe Text
findFingerprintComponent predicate record = record.fingerprint >>= Foldable.find predicate

-- | Keep matching components in order, preserving duplicates and optional presence.
filterFingerprint :: (Text -> Bool) -> ScopeUpdate
filterFingerprint predicate = modifyExistingFingerprint (filter predicate)

-- | Return the first matching local breadcrumb.
findBreadcrumb :: (Patrol.Breadcrumb -> Bool) -> ScopeData -> Maybe Patrol.Breadcrumb
findBreadcrumb predicate record = Foldable.find predicate record.breadcrumbs

-- | Keep matching local breadcrumbs in order, preserving duplicates.
-- This operation runs no capture policies.
filterBreadcrumbs :: (Patrol.Breadcrumb -> Bool) -> ScopeUpdate
filterBreadcrumbs predicate = modifyBreadcrumbs (Sentry.Breadcrumb.filterBreadcrumbs predicate)
