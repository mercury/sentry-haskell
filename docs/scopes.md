# Scopes and metadata

Scopes attach metadata to captures made with the active thread-local context.

Use the [README example](../README.md#scopes) to get started.

## Contents

- [Which scope should I use?](#which-scope-should-i-use)
- [How are these layers merged?](#how-are-these-layers-merged)
- [How does scope metadata apply to an event?](#how-does-scope-metadata-apply-to-an-event)
- [What do setting, modifying, and removing do?](#what-do-setting-modifying-and-removing-do)
- [How do I remove metadata before sending?](#how-do-i-remove-metadata-before-sending)
- [What happens when an exception escapes?](#what-happens-when-an-exception-escapes)
- [Fingerprints](#fingerprints)
- [Defaults without overwriting](#defaults-without-overwriting)

## Which scope should I use?

| Scope | Typical metadata | Lifetime |
| --- | --- | --- |
| Global | Service-wide tags | Process |
| Isolation | Request user, request ID, job details | Request or task |
| Current | Details about one operation | Enclosed action |

Use `Sentry.configureGlobal` for process-wide metadata, `withIsolationScope`
at request or task boundaries, and `withScope` for narrower operations.

Use `Sentry.setTag` and `Sentry.setUser` to attach request-wide metadata without
a scope handle. They select the isolation scope automatically.
`Sentry.setTransaction` selects the current scope to name an operation.

These automatic operations leave metadata unchanged without a recording client.
Explicit edits through `Sentry.updateScope` remain available before initialization.

The examples assume GHC2024 with `BlockArguments`, `OverloadedStrings`, and
`OverloadedRecordDot` enabled. Run capture examples with a recording client,
such as inside `Sentry.withSentry` with a configured DSN.

```haskell
import Sentry qualified
import Sentry.Scope qualified
import Sentry.User qualified

handleRequest :: IO ()
handleRequest =
  Sentry.withIsolationScope \_ -> do
    Sentry.setUser [Sentry.User.setId "42"]
    Sentry.setTag "request_id" "req-123"
    Sentry.withScope \scope -> do
      Sentry.updateScope scope (Sentry.Scope.setTag "operation" "charge-card")
      Sentry.captureMessage_ Sentry.Info "Charging card"
    Sentry.captureMessage_ Sentry.Info "Request completed"
```

The automatic setters attach request-wide metadata to the isolation scope, so
both captures include the request user and ID.

The explicit `updateScope scope` call sets the `operation` tag for any events
captured within `withScope`.

Leaving `withScope` or `withIsolationScope` restores the previous scope bindings:

- `withScope` clones the `Current` scope, or creates it if absent.
- `withIsolationScope` clones both the `Isolation` and `Current` scopes, or
  creates them if either (or both) are absent.

> [!WARNING]
> Scope metadata is stored on a thread-local context variable, so threads do
> not automatically inherit scope metadata unless they have been forked by a
> helper function that propagates thread-local context.

## How are these layers merged?

The `captureException`, `captureMessage`, and `captureEvent` functions combine
scopes in order from `Global` -> `Isolation` -> `Current`, using these rules:

| Metadata | Combination rule |
| --- | --- |
| Tags and extras | Combine by key; on collision, the later scope's value wins. |
| User | The last assigned user replaces earlier users as a whole. |
| Contexts | Combine by name; the later scope's payload replaces the entire earlier payload. |
| Level, fingerprint, transaction | Use the last assigned value; absent values preserve earlier assignments. |
| Breadcrumbs | Append in scope order: `Global`, `Isolation`, `Current`. |

An explicitly empty user or fingerprint still counts as an assignment.

For example, a user assigned on the current scope replaces the isolation user:

```text
Isolation user: { id: "42", email: "alice@example.com" }
Current user:   { name: "Alice" }

Merged user:    { name: "Alice" }
```

The same replacement rule applies to a named context: assigning an `app`
context on the current scope replaces the isolation scope's `app` payload.

## How does scope metadata apply to an event?

After combining the scope layers, capture functions apply the merged `ScopeData`
to an event according to the following rules:

| Metadata | Precedence |
| --- | --- |
| User | A user assigned directly to the event wins, including an empty user record; otherwise, use the scope's user. |
| Transaction | A nonempty name on the event wins; an empty name defers to a transaction from the scope. |
| Level | A level present on the scope overrides the level already present on an event. |
| Tags and extras | Combine by key; on collision, the value present on the scope replaces the one on the event. |
| Contexts | Combine by name; a scope payload replaces the entire event payload at that name. |
| Fingerprint | A custom event fingerprint wins, while an empty list or a singleton `{{ default }}`/`{{default}}` value defers to a fingerprint present on the scope. |
| Breadcrumbs | The event's breadcrumbs precede the scope's breadcrumbs. |

The scope's event processor receives the merged event inside a `CapturedEvent`,
which also contains the original exception when available.

Integration processors and `beforeSend` hooks run after the scope's event
processor, and can also modify or drop the event.

The delivered event can therefore contain metadata that isn't shown by
`readMergedScope`, which merges scope layers without running any of these
event processors.

Use `Sentry.Test` to assert which metadata reaches the transport in a unit test.

## What do setting, modifying, and removing do?

`Sentry.updateScope scope [ .. ]` atomically applies the edits specified in
its argument list to the scope handle, in list order.

### Setting and modifying users

The `Sentry.Scope` user builders edit only the selected scope's local user:

- `setUser` replaces the user with a supplied record or a user built from empty.
- `setOptionalUser` replaces the local user with `Just user`, or removes the
  assignment with `Nothing`, allowing an inherited user to appear in captures.
- `modifyUser` edits the user, starting from an empty record when none exists.
- `modifyExistingUser` edits the user only when one already exists locally.
- `unsetUser` removes the local assignment, allowing an inherited user to appear.

Suppose the isolation scope contains Alice's ID and name, and the current
scope has no user set on it:

```haskell
Sentry.withScope \scope -> do
  Sentry.updateScope scope $
    Sentry.Scope.modifyUser
      [Sentry.User.setEmail "alice@example.com"]
  Sentry.captureMessage_ Sentry.Info "Example"
```

`modifyUser` creates a local user containing only Alice's email address. That
user replaces the isolation user at capture, so the event has no user ID or name.

To preserve Alice's ID and name, copy the merged user into the current scope
before editing it:

```haskell
do
  metadata <- Sentry.readMergedScope
  Sentry.withScope \scope -> do
    Sentry.updateScope scope
      [ Sentry.Scope.setOptionalUser metadata.user,
        Sentry.Scope.modifyExistingUser
          [Sentry.User.setEmail "alice@example.com"]
      ]
    Sentry.captureMessage_ Sentry.Info "Example"
```

This update copies the merged user into the current scope, then sets its email
address. The captured event includes Alice's original ID and name alongside the
new email. If the merged scopes have no user, `setOptionalUser Nothing` removes
any local assignment and `modifyExistingUser` leaves it absent. Use `modifyUser`
instead if you want to create a user containing the email in that case.

The copy is a snapshot: subsequent changes to the isolation scope's user won't
affect this scope's user.

Scope edits do not modify inherited users. To remove user information from the
final event regardless of its source, use `Sentry.Event.unsetUser` in
[`beforeSend`](#how-do-i-remove-metadata-before-sending).

### Modifying contexts

Custom context edits are local too: `removeContext` removes the local scope's
override, allowing a context from an earlier layer to appear in captures.

`removeContextValue` leaves an absent local context absent; removing its last
field retains an empty context, which continues to override a context of the
same name in an earlier layer.

Custom field edits leave typed contexts unchanged; use the typed setters to
replace those payloads.

## How do I remove metadata before sending?

Use `beforeSend` to edit the merged event before delivery. Its input is a
`CapturedEvent` containing the event and capture metadata.

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
Custom event fingerprints take priority over scope fingerprints. Only an empty
event fingerprint or a singleton `"{{ default }}"`
(or `"{{default}}"`) defers to a present scope fingerprint. A default token
alongside custom components is custom grouping and stays unchanged.

Both `Sentry.Event` and `Sentry.Scope` expose `defaultFingerprintComponent`,
`setFingerprint`, `appendFingerprintComponent`, `prependFingerprintComponent`,
`modifyFingerprint`, `ensureDefaultFingerprint`, `removeDefaultFingerprint`,
and `clearFingerprint`. Append/prepend preserve duplicates without adding a
default token. Ensure preserves existing token spelling and position, otherwise
prepending the canonical token. Remove deletes all occurrences of both recognized
spellings. Modify transforms the complete list once.

Scope modifications start from `[]` when absent; `modifyExistingFingerprint`
leaves absent fingerprints unchanged. Append, prepend, and ensure create
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

## Optional assignments

Use `setOptionalX` to copy a value that may be absent. `Just value` replaces the
assignment; `Nothing` removes it. These builders accept concrete optional values,
such as `Maybe User`, so bare `Nothing` needs no type annotation. Existing
`setX`, `unsetX`, and keyed `removeX` operations remain available.

For example, with qualified imports of `Sentry` and `Sentry.Scope`, and an
existing scope handle `scope`:

```haskell
Sentry.updateScope scope
  [ Sentry.Scope.setOptionalTag "region" (Just "eu"),
    Sentry.Scope.setOptionalTransaction Nothing,
    Sentry.Scope.setOptionalFingerprint (Just [])
  ]
```

Removing an assignment affects only local metadata and may reveal inherited
metadata. Assigning an empty user, empty fingerprint, or empty custom context
retains a present local value. `setOptionalContextValues key (Just [])` therefore
differs from `setOptionalContextValues key Nothing`.

Optional typed-context setters replace the canonical entry with `Just record`
and remove it with `Nothing`, regardless of its previous variant. Optional
custom-context field setters leave typed payloads unchanged; deleting a final
custom field retains its empty context. Optional headers retain the existing
ASCII case-insensitive replacement and removal rules.

Automatic optional setters target isolation, except transaction naming, which
targets current. They skip their arguments without a recording client. Explicit
scope operations remain usable without initialization; context-based `*At`
operations skip absent targets without creating scopes.
