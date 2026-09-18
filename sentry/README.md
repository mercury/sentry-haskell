# Sentry Haskell SDK: `sentry`

This package provides the default HTTP transports and the opinionated `Sentry`
entry point (default transport + environment-variable resolution) for
applications integrating Sentry. See the top-level README for usage.

## Assembling a transport

The built-in transports are compositions of public pieces. If you need to send
envelopes some other way (a different HTTP client, a queue, a file), you can
reuse the parts that implement SDK policy instead of reimplementing them. The
pieces separate body preparation, protocol delivery, and SDK policy, and
compose in that order.

`Sentry.Transport.Encoding.encode` turns an envelope into an `EncodedBody`.
Its `bytes` are the exact bytes that will be sent, and its `compression`
records how they were encoded, so a request builder can derive
`Content-Encoding` from the body rather than being told separately. If you
serialize envelopes yourself, wrap your bytes with `fromBytes` and pass the
encoding you actually used; the length is measured for you either way.

Build the request next. For HTTP/1.1, build a template once per DSN with
`HTTP.Request.prepare` and attach each body to it with `attach`. For HTTP/2,
build an endpoint with `HTTP2.Connection.mkEndpoint` and a native request with
`buildRequest`. Both take `Content-Encoding` from the body they are given.

Send the request with `HTTP.Sync.sendRequest` or `HTTP2.Connection.sendRequest`,
through a manager you own. Each consumes the response and returns an
`HTTP.Delivery.Outcome` that still carries the status and response headers.
Neither retries or replays an envelope. Any manager you supply stays yours to
close, and an executor should be shut down before the manager it sends through.

Then apply policy. `HTTP.Delivery.interpretNow` turns that HTTP outcome into a
generic `Delivery.Outcome`: whether the attempt was accepted, how a rejection
should be accounted for, and any rate-limit deadlines the response announced.
Use it rather than the pure `interpret`, which is for when you already have
the timestamp the response was observed at. A relative `Retry-After` is only
correct when measured from the response. A non-HTTP sender can construct a
`Delivery.Outcome` directly, including absolute rate-limit deadlines, without
inventing an HTTP status. `Accepted` means the sender took responsibility for
the envelope under its own contract, not that Sentry stored it.

Finally, hand the result to an executor: return a `Delivery.Outcome` from the
callback you pass to `Executor.Async.new`, or use `HTTP.Sync.buildWithSender`
for a synchronous transport. Either way the SDK keeps doing the accounting:
filtering rate-limited items, attaching pending client reports, applying the
limits the response announced, and recording locally accountable rejections.
Don't repeat that work in your callback.

`Executor.Async.clientReportConfig` builds standalone client-report envelopes
for a DSN if you need them. The executor keeps its queue and worker private and
exposes only `send`, `flush`, `shutdown`, and `recordDiscards` through its
`Transport` instance.

## Recording what each delivery costs

`Sentry.Transport.Instrument.observing` wraps the backend-specific part of a
send (a prepared body to an outcome) and hands an observer what the attempt
cost: the envelope, the prepared size, the compression, the elapsed seconds,
and the sender's own outcome.

Given a `manager` and a DSN `template` from `HTTP.Request.prepare`, and a
`record` function that files the measurement somewhere:

```haskell
sender :: Patrol.Envelope -> IO Delivery.Outcome
sender =
  Instrument.observing record Encoding.Gzip (\body ->
    HTTP1.sendRequest manager mempty (Request.attach template body))
    >=> HTTPDelivery.interpretNow
```

The envelope is encoded, and forced, before the timer starts; see
`EncodedBody.size`'s Haddock for why forcing is what finishes serialization and
compression. The observer runs under `catchAny`, so instrumentation can
neither fail a send nor report its own failures through the transport it
observes.

The outcome type is the sender's own, so an HTTP observer still sees the status
and response headers while the executor receives generic delivery policy. That
translation is `HTTPDelivery.interpretNow`, composed on the end above; the
executor accepts only a `Delivery.Outcome`, so omitting it is a type error
rather than a silent mistake.
