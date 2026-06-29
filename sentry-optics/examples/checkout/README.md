# `checkout` — scope ⇄ OpenTelemetry demo

A runnable tour of the optional optics API (`sentry-optics`) wired up to
OpenTelemetry.

The same Sentry scope that drives captured events is also processed to enrich
OTel spans with additional metadata via a `SpanProcessor` (see `Otel.hs`): when
the span ends, it reads the ambient scope and adds scope attributes under the
`sentry.*` namespace

```sh
cabal run sentry-optics:checkout
```

The workflow runs two checkouts under nested spans:

- `checkout`, the isolation scope carries the transaction, `feature` tag, and user
- `charge`, the inner span inside a forked `withScope` that adds a `step` tag
  and a level
- `receipt`, a sibling span that ends *after* the forked scope is popped

The processor makes that location difference visible: `charge` carries the
`sentry.level` / `sentry.tag.step` attributes, while its sibling `receipt` does
not:

```text
● checkout
  sentry.tag.feature = payments
  sentry.transaction = checkout
  sentry.user.email  = alice@example.com
  ├─ charge
  │     sentry.level       = error
  │     sentry.tag.feature = payments
  │     sentry.tag.step    = charge
  │     sentry.transaction = checkout
  │     sentry.user.email  = alice@example.com
  └─ receipt
        sentry.tag.feature = payments
        sentry.transaction = checkout
        sentry.user.email  = alice@example.com

● checkout
  sentry.tag.feature = payments
  sentry.transaction = checkout
  sentry.user.email  = bob@example.com
  ├─ charge
  │     sentry.level       = info
  │     sentry.tag.feature = payments
  │     sentry.tag.step    = charge
  │     sentry.transaction = checkout
  │     sentry.user.email  = bob@example.com
  └─ receipt
        sentry.tag.feature = payments
        sentry.transaction = checkout
        sentry.user.email  = bob@example.com
```
