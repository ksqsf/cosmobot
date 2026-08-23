# External Plugin System

## Summary

Implement executable plugin bundles discovered under a configurable directory. Each plugin communicates with cosmobot through bidirectional newline-delimited JSON-RPC, may hot-load routes and agent tools, and uses a standalone first-party Python or Haskell SDK. Plugins cannot call one another.

## Bundles and configuration

- Main config: `[plugins] plugin_dir = "plugins"`; resolve relative paths beside the main config file.
- Bundles are immediate subdirectories only: `plugins/<id>/<id>` plus `plugins/<id>/config.toml`.
- Every installed bundle is enabled and loaded at startup. Missing, unreadable, or malformed bundle configuration aborts startup.
- Bundle `[plugin]` contains `required = false`, `sandboxed = true`, `route_timeout_seconds = 10`, `tool_timeout_seconds = 300`, and `restart_limit = 3`; other tables belong to the plugin.
- The same config file is exposed as `COSMOBOT_PLUGIN_CONFIG`; configuration contents never enter protocol logs or manifests.
- `!plugin/load <id>` discovers a newly installed bundle. Unloaded plugins load again on restart. There is no watcher or persistent enable/disable state.

## Author SDKs

- Ship standalone Haskell `Cosmobot.Plugin` and Python packages with no cosmobot-internal API dependency.
- Haskell `Plugin` is Applicative, deliberately not Monad, and accumulates ordered declarations and all validation errors. It provides typed argument combinators, `command`, `tool`, stateless `serve`, and bracketed `serveWith` initialization/finalization.
- Python uses async handlers, decorators, and dataclass-derived tool schemas.
- Commands derive route ID, help, stripped arguments, allowed access, and stop disposition. Returning text is a successful tool result. Model-visible tool names are `<plugin>__<tool>`.
- Plugin authors own mutable state and synchronization. Invocations are concurrent by default; there is no host queue, lock, retained context, or background-task API.

## Protocol and host APIs

- JSON-RPC 2.0 over stdin/stdout, one object per line, concurrent IDs, synchronized writes, 1 MiB maximum line, stderr-only logs.
- Host methods: `plugin.initialize`, `plugin.route.invoke`, `plugin.tool.invoke`, `plugin.shutdown`.
- Initialization returns protocol/plugin versions, routes, filters, tools, schemas, and requested capabilities. Validate the complete manifest before atomically publishing registrations.
- Grant requested capabilities automatically, but reject calls to undeclared capability groups.
- Route invocation sends normalized `IncomingMessage` plus unstable `raw`; filters are bounded boolean expressions over command, prefix, platform, event, chat kind, reply, mention, and access.
- Route failure/timeout logs and continues routing. Invalid tool arguments are permanent argument failures; ordinary invocation exceptions are transient failures.
- Tool runners bind to a process generation. Invocation callbacks expire when the handler returns. Timeouts begin when SDK handler execution begins, including plugin-owned lock time.
- MVP callback groups: `chat` (reply/current referenced message), `llm` (one-shot text), `agent` (one blocking run with current context, configured limits, built-in tools), `media` (canonical ref, MIME, size, public URL, local path).
- Exclude memory, scheduler, raw database, filesystem, process, platform transport, streaming agent, steering, and plugin-to-plugin APIs.

## Sandbox and lifecycle

- Default sandbox execution uses Bubblewrap. Mount bundle read-write at `/plugin`, media read-only at `/media`, private tmpfs `/tmp`, minimal read-only runtime libraries/certificates/DNS/devices, shared network, cleared environment, dropped capabilities, disabled nested user namespaces, process group, and parent-death termination.
- Run `/plugin/<id>` in `/plugin` with `COSMOBOT_PLUGIN_CONFIG=/plugin/config.toml`. Never fall back to unsandboxed execution.
- `sandboxed = false` runs normally with host paths and cosmobot's OS identity.
- Remove registrations before unload/reload, cancel and await active invocations, call finalize exactly once after successful initialization, then terminate the process group after a five-second grace period.
- Unexpected exit/signal/EOF/protocol-loop failure is permanent unless the SDK explicitly reports transient failure. Retry transient failures using `restart_limit` and capped 1/2/4-second backoff; retain a three-exits-in-60-seconds breaker.
- Optional permanently failed/exhausted plugins are removed. Required failures abort startup or terminate the bot. Required plugins cannot unload; reload is allowed but failed replacement terminates cosmobot. Normal structured shutdown finalizes all initialized plugins.

## Cosmobot integration

- Add boring `Bot.Effect.Plugin` operations for status, load/unload/reload, route dispatch, dynamic help, and generation-bound tool snapshots.
- Keep protocol/process/generation/manifest/invocation/sandbox/supervision state in `Bot.Plugin.*`; superuser command policy is `Bot.Handler.Plugin`.
- Add `!plugin/list`, `!plugin/load <id>`, `!plugin/reload <id>`, `!plugin/unload <id>`.
- Add one dynamic route gateway after built-in routes and before ask/conversation routes. Dynamic help is queried for every `!help`.
- Snapshot active plugin tools when each agent run starts; existing runs keep generation-bound runners.
- Compose the interpreter and host callbacks in `Bot.Main`; do not leak process/protocol concerns into handlers or agent core.
- Update Cabal declarations/project, example config, protocol documentation, and both SDK examples.

## Verification

- Add deterministic plugin tests for discovery/config, manifest/framing, initialization/callbacks, malformed messages, timeouts/shutdown, concurrency/context expiry/cancellation/finalize order/generation isolation, retry/permanent failure/crash breaker, route/help continuation, dynamic tools, and Bubblewrap arguments/path translation/no fallback.
- Share protocol fixtures across host and SDK tests.
- Run `cabal test -j plugin-spec agent-spec config-spec filter-spec --test-options=--hide-successes`, both SDK test suites, `cabal build -j exe:cosmobot`, and `git diff --check`.
- Review architecture/dependency direction, dependency surface, protocol contract, abstraction smells, and process/resource lifecycle; fix all high/medium findings or document why out of scope.

## Assumptions

- MVP sandbox platform is Linux with Bubblewrap. Unsandboxed executables are trusted.
- No watched directory, registry database, static manifest, native shared library, remote transport, persistent enabled list, streaming-agent API, or inter-plugin API.
- Reload is unload then load, not zero-downtime multi-generation swapping.
