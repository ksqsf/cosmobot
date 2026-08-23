# cosmobot-plugin-sdk

Standalone Haskell SDK for executable cosmobot plugins. The SDK handles the
newline-delimited JSON-RPC protocol on stdin/stdout; plugin logs must go to
stderr. See the [full plugin protocol and host guide](../cosmobot/doc/Plugins.md).

## Install and build

Add this package to the plugin project's `cabal.project`:

```cabal
packages: .
          ../cosmobot-plugin-sdk
```

Then add `cosmobot-plugin-sdk` to the executable's `build-depends` and build:

```console
cabal build -j
```

To build and test this checkout directly:

```console
cabal build -j exe:echo-plugin
cabal test -j sdk-spec --test-options=--hide-successes
```

## Minimal plugin

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Cosmobot.Plugin

main :: IO ()
main = serve "1.0.0" [Chat] $ do
  command "!echo" "Echo the supplied text." $ \request -> do
    _ <- reply (arguments request)
    pure ""
  tool "echo_text" "Echo text as an agent tool." (text "text") pure
  pure ()
```

`Plugin` is applicative and deliberately has no `Monad` instance. Keep the
final `pure ()` so `ApplicativeDo` can accumulate declarations. Invocation
handlers use the separate `Handler` monad; `io` lifts native plugin state and
synchronization operations.

Use `serve` for stateless plugins. Use `serveWith` when initialization returns
state that every declaration closes over:

```haskell
main =
  serveWith "1.0.0" [Chat]
    (pure "Hello")
    (const (pure ()))
    $ \greeting -> do
      command "!hello" "Reply using plugin state." $ \request ->
        reply (greeting <> " " <> arguments request) >> pure ""
      pure ()
```

Initialization failures are permanent by default. Call
`transientStartup "service unavailable"` to explicitly request a supervised
retry. For a retryable failure after startup, call
`transientProcessFailure "connection lost"`; it logs to stderr and exits with
status 75.

## Runtime contracts

- Declare only the capabilities used by handlers: `Chat`, `LLM`, `Agent`, or
  `Media`. Calls through an undeclared capability fail locally.
- The host supplies a required positive `timeoutSeconds` for every invocation.
  Timing starts when the SDK enters the handler, so time spent in plugin-owned
  locks counts. A timeout cancels and waits for the handler before replying.
- On shutdown the SDK cancels and waits for active handlers, calls `finalize`
  exactly once after successful initialization, then sends the shutdown reply.
- Host callbacks are valid only during their current invocation. Concurrent
  invocations are the default; plugins synchronize their own mutable state.

## Bundle layout

The bundle directory and executable name are the plugin ID:

```text
plugins/
└── echo/
    ├── echo
    └── config.toml
```

```toml
[plugin]
required = false
sandboxed = true
route_timeout_seconds = 10
tool_timeout_seconds = 300
restart_limit = 3
```

Cosmobot exposes that configuration file through `COSMOBOT_PLUGIN_CONFIG`.
