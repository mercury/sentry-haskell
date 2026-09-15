# Scopes and metadata

Scopes attach metadata to captures made with the active thread-local context.

Use the [README example](../README.md#scopes) to get started.

## Contents

- [Which scope should I use?](#which-scope-should-i-use)
- [How does metadata combine?](#how-does-metadata-combine)
- [What do setting, modifying, and removing do?](#what-do-setting-modifying-and-removing-do)
- [How do I remove metadata before sending?](#how-do-i-remove-metadata-before-sending)
- [What happens when an exception escapes?](#what-happens-when-an-exception-escapes)

## Which scope should I use?

| Scope | Typical metadata | Lifetime |
| --- | --- | --- |
| Global | Service-wide tags | Process |
| Isolation | Request user, request ID, job details | Request or task |
| Current | Details about one operation | Enclosed action |

Use `Sentry.configureGlobal` for process-wide metadata, `withIsolationScope`
at request or task boundaries, and `withScope` for narrower operations:

```haskell
import Sentry qualified
import Sentry.Scope qualified
import Sentry.User qualified

handleRequest :: IO ()
handleRequest =
  Sentry.withIsolationScope \requestScope -> do
    Sentry.updateScope requestScope
      [ Sentry.Scope.setUser [Sentry.User.setId "42"],
        Sentry.Scope.setTag "request_id" "req-123"
      ]
    Sentry.withScope \operationScope -> do
      Sentry.setTag operationScope "operation" "charge-card"
      Sentry.captureMessage_ Sentry.Info "Charging card"
    Sentry.captureMessage_ Sentry.Info "Request completed"
```

Both capture calls include the request user and ID, however only the first
includes the `operation` tag.

Leaving a bracketed `with{Isolation}Scope` restores the previous scope bindings:
- `withScope` clones the current layer, or creates it if absent
- `withIsolationScope` clones both the isolation and current layers and gives
  the action a handle to the isolation layer

Capture calls reference the active thread-local context, so forked threads do
not automatically inherit the metadata attached to the thread-local context
variable and must be attached manually using a helper function.

## How does metadata combine?

Capturing a message or an exception combines the scope layer in order from
global -> isolation -> current, with later layers overriding previously set
values on collision.

| Metadata | Combination rule |
| --- | --- |
| Tags and extras | Combine by key; later values win |
| User | A later user replaces the earlier user as a whole |
| Contexts | Combine by context name; a later payload replaces the earlier payload at that name |
| Level, fingerprint, transaction | Use the latest assigned value |
| Breadcrumbs | Append in layer order |

For example, a user assigned on the current scope replaces the isolation user:

```text
Isolation user: { id: "42", email: "alice@example.com" }
Current user:   { name: "Alice" }

Captured user:  { name: "Alice" }
```

The same replacement rule applies to a named context: assigning an `app`
context on the current scope replaces the isolation scope's `app` payload.

## What do setting, modifying, and removing do?

`Sentry.updateScope scope ...` edits metadata stored on that scope, with the
edit bundle applied atomically, in order, and not overriding any assignments
that come after it.

- `setUser` builds a user from empty, or accepts a replacement record, and
  replaces the local user
- `modifyUser` edits an existing local user; if none exists, it does nothing
- `unsetUser` removes the local user override, potentially revealing a user
  from another layer

Suppose the isolation scope holds Alice, including her email, and the current
scope has no user:

```haskell
Sentry.withScope \scope -> do
  Sentry.modifyUser scope (Sentry.User.setEmail "")
  Sentry.captureMessage_ Sentry.Info "Example"
```

The modification does nothing because the current scope has no local user.
The capture still includes Alice's email from the isolation scope.

Custom context edits are local too: `removeContext` removes the local scope's
override, which means a captured report may contain metadata from the layer
above it if any is set there.

`removeContextValue` leaves an absent local context absent; removing its last
field retains an empty context, which continues to override a context of the
same name in an earlier layer.

Custom field edits leave typed contexts unchanged; use the typed setters to
replace those payloads.

## How do I remove metadata before sending?

Use `beforeSend` to edit the merged event that will be delivered, its input is
a `CapturedEvent`. containing the event and capture metadata.

Return `Just event` to keep the resulting record or `Nothing` to drop it.

```haskell
import Data.Default (def)
import Sentry qualified
import Sentry.Event qualified

scrub :: Sentry.CapturedEvent -> Maybe Sentry.Event
scrub captured =
  Just $
    Sentry.Event.apply captured.event
      [ Sentry.Event.unsetUser,
        Sentry.Event.removeExtra "authorization"
      ]

options :: Sentry.ClientOptions
options = def{Sentry.beforeSend = Just scrub}
```

## What happens when an exception escapes?

If a synchronous exception escapes `withScope` or `withIsolationScope`, the
SDK attaches a snapshot of the merged scope metadata and rethrows it. An
exception already carrying a scope annotation keeps its existing snapshot,
preserving the innermost context.

The bracket itself does not send an event. A handler can later report the
exception with `Sentry.captureUnhandledException`, supplying a mechanism name
such as `"request-handler"`; capture uses the annotated metadata.

Asynchronous exceptions are rethrown without scope annotations, preserving
their identity and cancellation behavior. Capture them explicitly if they
should be reported.

## Fingerprints

Scope fingerprints replace lower layers, including when explicitly empty.
**Behavior change:** scope fingerprints no longer overwrite custom Event
fingerprints. Only an empty Event fingerprint or a singleton `"{{ default }}"`
(or `"{{default}}"`) defers to a present scope fingerprint. A default token
alongside custom components is custom grouping and stays unchanged.

Both `Sentry.Event` and `Sentry.Scope` expose `defaultFingerprintComponent`,
`setFingerprint`, `appendFingerprintComponent`, `prependFingerprintComponent`,
`modifyFingerprint`, `ensureDefaultFingerprint`, `removeDefaultFingerprint`,
and `clearFingerprint`. Append/prepend preserve duplicates without adding a
default token. Ensure preserves existing token spelling and position, otherwise
prepending the canonical token. Remove deletes all occurrences of both recognized
spellings. Modify transforms the complete list once. Assignments and computed
lists are forced to WHNF only; list elements are not deeply evaluated.

Scope modifications start from `[]` when absent; `modifyExistingFingerprint`
skips absence without evaluating its function. Append, prepend, and ensure create
a local assignment; remove skips absence. Clear stores `Just []`, while unset
stores `Nothing`; removing the last component retains `Just []`.

Builders read only the selected scope's stored data, including cloned values.
They never materialize other active layers. Unsetting a cloned value does not
restore the suspended outer current scope's value. To force normal grouping
despite contextual defaults, clear or replace the fingerprint in an event
processor after scope merging.

## Defaults without overwriting

`Sentry.Event.setTagIfAbsent` and `setContextIfAbsent` insert only when the key
is missing. Existing values, including empty tags or context payloads, win.
Unused proposed values are not evaluated; inserted values are forced to WHNF.

The matching `Sentry.Scope` builders check only the selected scope's stored
metadata (including cloned values). They do not check other active layers, so a
local default can override a lower-layer value at capture. To supply defaults
only when the merged event lacks a key, use the Event builders in a processor.

```haskell
Sentry.updateScope scope
  [ Sentry.Scope.setTagIfAbsent "alert_route" "default",
    Sentry.Scope.setTagIfAbsent "owner" "platform"
  ]
```
