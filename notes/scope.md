## `Sentry.Scope` design notes

Sentry provides a specification (of sorts) for implementing "scopes" as part of SDK state management[^1], they further specify that SDKs using OpenTelemetry for instrumentation **SHOULD** store scopes on the OpenTelemetry Context[^2].

For us, this would mean copying the [`OpenTelemetry.Context` design](https://github.com/iand675/hs-opentelemetry/blob/adc464b0a45e56a983fa1441be6e432b50c29e0e/api/src/OpenTelemetry/Context.hs#L96-L110) and using `isolationScopeKey` & `currentScopeKey` as keys that can retrieve & manipulate them as we do `Span` & `Baggage` objects attached to the context.

This also means trying to adhere to the specified behavior on forking scopes when a new OTel span or context is created[^3]. While we can probably support span lifecycle hooks by way of [`SpanProcessor`](https://hackage-content.haskell.org/package/hs-opentelemetry-api-0.3.1.0/docs/OpenTelemetry-Processor-Span.html#t:SpanProcessor) & `spanProcessorOnStart`, there isn't an equivalent lifecycle interface for contexts.

I think span creation is the only hook that _really_ matters when it comes to scope forking, though; the only problem is we now have to be careful about tracking how context is attached, adjusted, detached, etc. on thread boundaries.

We also need to figure out how scopes get handled in the context of walking back up the callstack. Right now we use `AnnotatedException` to accumulate metadata that can be converted into a `Patrol.Event -> Patrol.Event` modifier, but if we're bought in on OTel then we can just turn our `checkpoint` functions into `inSpan` calls that fork the scope & applies a `Scope -> Scope` transformation.

The only open question is how we ensure that unhandled exceptions capture and relay the correct context when dealing with an unhandled exception.

```haskell
try $ withScope do
  throwIO $ userError "boom!"
```

We want `withScope` to freeze its scope if an unhandled exception passes through it while making its way up the call-graph, however if someone catches and handles the exception _before_ it hits a handler then we want to discard the scope captured during the thrown exception.

Maybe the mechanism here is as simple as just using `AnnotatedException` to capture all three scopes and collapse them down into a `FrozenScope` that gets attached as an `Annotation` and propagated back up the call graph.

[^1]: https://develop.sentry.dev/sdk/foundations/state-management/scopes/
[^2]: https://develop.sentry.dev/sdk/foundations/state-management/scopes/#otel-context-alignment
[^3]: https://develop.sentry.dev/sdk/foundations/state-management/scopes/#forking-hooks
