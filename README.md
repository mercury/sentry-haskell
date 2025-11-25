## (Unofficial) Sentry SDK for Haskell

This project is heavily inspired architecturally and in terms of intent by the official [sentry-rust](https://github.com/getsentry/sentry-rust) SDK.

## Development

### Profiling

Defaults for profiling executables are provided in [`cabal.project.profiling`](./cabal.project.profiling), and can be selected (along with the appropriate GHC RTS opts) with the command runner's `profiling`.

For example, the following command will benchmark the [`sentry`] library and produce `sentry/time.prof`:
```shell
$ just profiling=true bench sentry
```

[`sentry`]: ./sentry
