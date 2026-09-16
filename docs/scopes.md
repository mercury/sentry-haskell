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
- [Optional assignments](#optional-assignments)

## Which scope should I use?

| Scope | Typical metadata | Lifetime |
| --- | --- | --- |
| Global | Service-wide tags | Process |
| Isolation | Request user, request ID, job details | Request or task |
| Current | Details about one operation | Enclosed action |

Use `Sentry.configureGlobal` for process-wide metadata, `withIsolationScope`
at request or task boundaries, and `withScope` for narrower operations.

Use `Sentry.setTag` and `Sentry.setUser` to attach request-wide metadata without
a scope handle; they select the isolation scope automatically.

Leaving `withScope` or `withIsolationScope` restores the previous scope bindings:

- `withScope` clones the `Current` scope, or creates it if absent.
- `withIsolationScope` clones both the `Isolation` and `Current` scopes, or
  creates them if either (or both) are absent.

> [!WARNING]
> Scope metadata is stored on a thread-local context variable, so threads do
> not automatically inherit scope metadata unless they have been forked by a
> helper function that propagates thread-local context.

Using the following snippet as an example:

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
      Sentry.updateScope scope $ Sentry.Scope.setTag "operation" "charge-card"
      Sentry.captureMessage_ Sentry.Info "Charging card"
    Sentry.captureMessage_ Sentry.Info "Request completed"
```

The automatic setters attach request-wide metadata to the isolation scope, so
both `captureMessage_` calls include the request user and ID.

The explicit `updateScope scope` call, however, only sets the `operation` tag
for the `"Charging card"` message.

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

`Sentry.updateScope scope [ .. ]` atomically applies the updates specified in
its argument list to the scope handle, in list order.

### Transforming a local value

Use `Sentry.Scope.with` when an update depends on a value already stored on the
scope.

For example, consider an update that sets a `"region"` tag composed with an
update that capitalizes all `"region"` tags:

```haskell
Sentry.updateScope scope
  [ Sentry.Scope.setTag "region" "eu",
    Sentry.Scope.with \local ->
      Sentry.Scope.setOptionalTag "region"
        (fmap Text.toUpper (Sentry.Scope.lookupTag "region" local))
  ]
```

The callback sees the preceding assignment of `"eu"`, so the final local tag is
`"EU"`.

### Setting and modifying users

The `Sentry.Scope` user builders update only the selected scope's local user:

- `setUser` replaces the user with a supplied record or a user built from empty.
- `setOptionalUser` replaces the local user with `Just user`, or removes the
  assignment with `Nothing`.
- `modifyUser` updates the user, starting from an empty record when none exists.
- `modifyExistingUser` updates the user only when one already exists locally.
- `unsetUser` removes the local assignment.

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
before updating it:

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

Updating one scope does not modify users stored on other scopes. To remove user
information from the final event regardless of its source, use `Sentry.Event.unsetUser` in
[`beforeSend`](#how-do-i-remove-metadata-before-sending).

The user builders also provide `with`, for example if you needed to strip
text around a user's name:

```haskell
Sentry.updateScope scope $
  Sentry.Scope.modifyExistingUser $
    Sentry.User.with \user ->
      Sentry.User.setName (Text.strip user.name)
```

### Modifying contexts

`removeContext` removes the named context from the scope being updated.

`removeContextValue` removes a field from a custom context on the scope being
updated, leaving an empty context when its last field is removed. If the context
is absent, the scope is unchanged.

Use `lookupContextValue` to read a custom field, `modifyExistingContextValue`
to transform one that is present, or `alterContextValue` to insert, replace, or
remove it.

Typed contexts have their own helpers; for example, remove a local app context
if it has no version:

```haskell
Sentry.updateScope scope $
  Sentry.Scope.alterAppContext \existing ->
    case existing of
      Just app | Text.null app.appVersion -> Nothing
      _ -> existing
```

## How do I remove metadata before sending?

Use `beforeSend` to update the merged event before delivery, return `Just event`
to keep the resulting record or `Nothing` to drop it:

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

To transform a present value, combine `with`, an optional setter, and `fmap`, as
in the [region example](#transforming-a-local-value).

For example, this trims a local region tag and removes it if it becomes blank:

```haskell
Sentry.updateScope scope $
  Sentry.Scope.with \local ->
    Sentry.Scope.setOptionalTag "region" $
      case Text.strip <$> Sentry.Scope.lookupTag "region" local of
        Just "" -> Nothing
        region -> region
```

This example also uses `Data.Text` qualified as `Text`.

Assigning an empty user, empty fingerprint, or empty custom context keeps that
assignment on the scope being updated. For example, `setOptionalContextValues
key (Just [])` assigns an empty custom context, while `setOptionalContextValues
key Nothing` removes it.

Optional typed-context setters replace the canonical entry with `Just record`
and remove it with `Nothing`, regardless of its previous variant. Optional
custom-context field setters leave typed payloads unchanged; deleting a final
custom field retains its empty context. Optional headers retain the existing
ASCII case-insensitive replacement and removal rules.

Automatic optional setters target isolation, except transaction naming, which
targets current. They skip their arguments without a recording client. Explicit
scope operations remain usable without initialization; context-based `*At`
operations skip absent targets without creating scopes.
