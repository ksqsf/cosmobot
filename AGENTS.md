You are a Haskell engineer working on cosmobot. Favor correctness, explicit data flow, small algebraic modules, and abstractions that make the code clearer in practice.

The workspace contains three Cabal packages:

- `cosmobot`: the bot library, executable, and tests;
- `cosmobot-plugin-sdk`: the plugin SDK;
- `cosmocode`: the RPC coding TUI.

## Architecture

Preserve this dependency direction:

`platform event -> core message -> route -> handler -> effect -> interpreter/concrete capability`

- Handlers own user-visible policy. They may call effects, but must not perform platform transport, database, LLM HTTP, or local-process work directly.
- Keep concrete integrations in chat drivers, storage, LLM transport, memory files, or `Bot.System.*`.
- Keep `cosmobot/app/Main.hs` declarative: load configuration, construct stores/interpreters, register routes, and start drivers.
- Keep algorithmic/domain modules independent of databases, filesystems, HTTP, processes, and platform APIs. Put those dependencies at the infrastructure edge.

### Module ownership

- `Bot.Core.*`: platform-neutral messages, routes, replies, conversations, histories, and trees.
- `Bot.Handler.*`: commands and conversation flows.
- `Bot.Effect.*`: narrow GADTs, smart constructors, and small adapters only. Put interpreters and state machines beside their owning implementation.
- `Bot.Chat.Driver.*`: platform APIs and normalized incoming messages. `Bot.Chat.*` owns shared chat behavior.
- `Bot.Agent.*`: agent calculus, runtime, tools, and middleware. Tools live in `Bot.Agent.Tools.*`; cross-cutting behavior lives in `Bot.Agent.Middleware.*`.
- `Bot.LLM.*`: LLM configuration, wire types, transport, retry, streaming, and test interpreters.
- `Bot.Storage.*`: Selda tables, queries, persistence rules, and SQLite wiring.
- `Bot.AgentAudit.*`, `Bot.ChatLog.*`, `Bot.Scheduler.*`, `Bot.Memory`, and `Bot.Resource.*`: their respective domain behavior; durable queries remain in `Bot.Storage.*`.
- `Bot.System.*`: local executable and OS integrations.
- `Bot.Config`: top-level configuration assembly only; owner parsers and schemas live beside their consumers.

Avoid import cycles by passing narrow callbacks or data instead of importing an effect facade back into its implementation.

## Haskell and effects

- Work in `Eff es`; add `IOE :> es` only at real external boundaries.
- Prefer `effectful` concurrency, process, timeout, reference, and filesystem capabilities over raw `base` APIs. Do not add `Control.Exception`, `Control.Concurrent`, `System.Directory`, or `System.IO` when the effectful equivalent covers the operation.
- Use `trySync`/`catchSync` for ordinary failures and `bracket`, `mask`, `finally`, or `onException` for lifecycle safety. Never classify or swallow async exceptions as normal control flow.
- Express multi-step state changes as pure plans, component-owned operations, or bracket-style helpers rather than imperative acquire/use/cleanup choreography.
- Use `aeson` for JSON, `Toml.Schema` and the local TOML machinery for configuration, and Selda through `Bot.Storage.Prelude` for queryable data.
- Add abstractions only when they remove real duplication or isolate an external system. Keep unrelated refactors separate from behavior changes.
- For Haskell changes, use the local `haskell` skill and its `ghcid`/`.ghcid-errors` fast-feedback loop when practical.

## Agent runtime

- Change `Bot.Agent.Core` only for the generic calculus (`Program`, `Step`, `AgentEvent`, `TurnState`, `Runtime`). Keep persistence, audit, media, chat logging, platform linking, and handler policy outside it.
- Keep programs, runtimes, tools, and observers polymorphic in carrier `m`; specialize to `Eff es` at application boundaries.
- Start root and child agents through `Bot.Effect.Agent.withRun`. Origin and resource ownership are interpreter metadata installed with `withAgentMetadata`, not alternate runner APIs.
- Use `TurnState` only for data spanning model/tool turns. Keep middleware-private state lexical and pass dynamic middleware context through the runtime HList, not general-purpose `Context` fields.
- Pick the narrowest middleware hook: `modelInputTranscript`, `aroundAgentRun`, `aroundModelTurn`, `aroundToolTurn`, or `aroundToolCall`.
- Preserve full tool results for the immediate next model turn, but omit them from later canonical/durable conversation state. Keep media projection, audit projection, conversation storage, and chat-message linking as separate middleware responsibilities.
- Capture tool-emitted platform messages through chat interposition; do not return platform message ids in `ToolResult`.

Test agent policy in `test/AgentSpec.hs`, calculus laws in `test/ProgramSpec.hs`, and deterministic fault/cancellation behavior in `test/FailureSpec.hs`.

## Concurrency, resources, and identity

- Use qualified `Bot.Effect.Concurrency` for application background work. Its interpreter uses `Effectful.Concurrent.Async`.
- Register a child before it can run. Manager exit must cancel and await every live child; exceptional exit propagates the top-level exception with `cancelWith` before awaiting.
- Treat async creation/register/start as one masked lifecycle operation and clean up partially acquired handles.
- Use `Bot.Resource` for person-owned, long-running in-memory objects. Scope them by `(platform, chatId, senderId)` and retain the creating agent run id.
- Never let managed objects escape `Resource.withResource`. Destruction makes the object unavailable, cancels and awaits users, then cleans up; restore an explicit removal if cleanup fails so it can be retried.
- Keep resource registration out of the concurrency manager, and add only resource kinds that exist now.
- Do not conflate chat and sender identity. Person state keys by platform/sender; room state keys by platform/chat. Message ids require platform and chat scope. Reject missing required identity instead of guessing.

## Configuration system

Configuration is an owner-defined, typed, restart-activated system.

### Ownership and schema

- Each owner module defines a `Bot.Config.Schema.ConfigSchema source runtime` containing its real parser plus inspection metadata. Its `FromValue` instance delegates to that schema; owner-specific cross-field validation stays beside it.
- Every option declares path segments, label, description, owner, typed kind, default, constraints, source getter, and effective-runtime getter. Use path arrays; never encode or parse dotted path strings.
- Supported kinds are boolean, integer, number, text, enum, secret, homogeneous list, and mixed text/integer identity values/lists.
- Sections declare explicit human-readable labels and groups. Preserve acronyms such as `LLM`, `RPC`, `ACP`, `QQ`, `S3`, and `GC`; never derive UI titles by capitalizing path segments.
- Optional sections model optional chat drivers. Repeatable sections model arbitrary named LLM chat, image, and audio providers.
- Secrets use `Schema.Secret`. JSON, diagnostics, logs, diffs, and `Show` expose only `configured`/`unset`, never credential text. Do not derive secret-bearing `Show` instances.
- `Bot.Config` assembles owner schemas and retains both startup source/runtime state and the normalized active runtime configuration.

Config locations remain:

- drivers: `[driver.qq]`, `[driver.telegram]`, `[driver.matrix]`, `[driver.discord]`;
- handlers: `[handler.*]`;
- LLMs: `[llm]` and named provider tables beneath it.

Driver access lists and superusers belong to their driver. Do not restore legacy top-level driver, `saucenao`, `handlers`, or handler-owned platform-whitelist tables.

When adding or changing an option, update the owner parser/schema, `Bot.Config` assembly if necessary, `config.example.toml`, the runtime consumer, and focused config/RPC/frontend tests. Each example option must be represented exactly once in the schema.

### RPC and file lifecycle

- `Bot.RPC.Config` owns RPC-server settings; authenticated administration lives in `Bot.RPC.Configuration` and is wired explicitly from `Main`.
- `config.get` returns schema version 2, current and active revisions, source diagnostics, complete grouped schema, source/effective/default values, backup metadata, and restart activation. Invalid current TOML falls back to the active snapshot and disables editing.
- `config.validate`, `config.update`, and `config.rollback` use SHA-256 revision checks. Changes are a closed union of set/remove, secret replace/clear, and section add/remove operations. Omitted secrets remain unchanged.
- Apply all requested changes in memory, reparse through the real typed schemas, and write only a fully valid result. No-op updates do not write or rotate backups.
- `Bot.Config.Edit` preserves unrelated TOML bytes/comments using located syntax. Replace only changed value/section spans, safely quote dynamic provider names, and reject unsupported inline or non-contiguous source shapes instead of reformatting them.
- Serialize update/rollback with the application lock. Reject symlink and non-regular targets; securely write beside the source, preserve metadata, and maintain one recoverable `<config>.cosmobot.bak`.
- Stable public errors and redacted semantic diffs are part of the protocol contract. Structured logs contain operation, revisions, and changed paths only.
- `admin.restart` acknowledges over WebSocket before invoking the message-free lifecycle restart. All configuration changes require an explicit restart.

### Cosmoscope

- Generate navigation and controls from schema metadata and option kinds. Group provider families and use row-based editors for lists/identity lists.
- Keep drafts client-side, validate before Apply, and never auto-merge revision conflicts. Secret replacement/removal, rollback, and restart require explicit actions.
- Adding a provider or optional section must immediately expose its typed draft controls; removing and re-adding an existing provider cancels the removal.
- Never mix live configuration with fixtures or a Demo fallback. An unsupported server schema gets a clear unavailable state.
- Do not periodically replace the configuration page or drafts. Ignore stale refresh/validation responses and preserve the selected section across reconnects.

## Change and review rules

- Handler admission starts in `Bot.Core.Route`; compose predicates instead of duplicating checks. Use the existing non-blocking fork pattern for LLM/platform work.
- Platform request/response details stay in drivers or dispatch glue.
- Agent tools update `Bot.Agent.Tools.*`, shared schemas in `Bot.Agent.Tools.Common`, `defaultTools`, and focused tests. Parse arguments with `AesonTypes.parseEither`.
- Persistence belongs in component-owned `Bot.Storage.*`; model queryable state as columns rather than opaque JSON.
- Add new modules to the relevant Cabal library/executable/test stanza.
- Treat frontend/backend contract mismatches as blockers: implement the called method or hide the UI path.
- For substantial RPC, web, storage, or lifecycle work, review architecture/dependency direction, public protocol, secret leakage, and resource cleanup. Fix all high/medium findings or document why one is out of scope, then rerun relevant checks.
- Subagents get disjoint scopes, file/line findings, and verification commands; reconcile contracts before integration.

## Verification

Use `-j` for Cabal builds/tests and `--test-options=--hide-successes` for tests.

- configuration/RPC: `cabal test -j config-spec rpc-spec --test-options=--hide-successes`;
- concurrency/resource: `cabal test -j concurrency-spec resource-spec --test-options=--hide-successes`;
- agent calculus/runtime/failures: `cabal test -j program-spec agent-spec failure-spec --test-options=--hide-successes`;
- scheduler/chat log: `cabal test -j scheduler-spec chat-log-spec --test-options=--hide-successes`;
- executable/config/module wiring: `cabal build -j exe:cosmobot`;
- Cosmoscope: run lint, typecheck, unit/build checks, and focused Playwright coverage for changed flows.

Always run `git diff --check`. Keep unrelated untracked files out of commits.

## Journald debugging

Query structured fields directly, for example `journalctl -u cosmobot.service PLATFORM=qq CHAT_ID=123` or `journalctl -u cosmobot.service AGENT_RUN_ID=run-id`. Combine fields to intersect; use `-f`, `-o json-pretty`, or `-o verbose` as needed. Use `journalctl -N` and `journalctl -F FIELD` to discover the current field catalog instead of maintaining a duplicate list here. `PRIORITY=7` selects debug only, while `-p debug` includes higher severities. Structured fields are available through the native journald scribe when `JOURNAL_STREAM` is present; otherwise logs fall back to stderr.

## Cosmocode

`cosmocode` is the TUI client for cosmobot RPC, aimed primarily at coding tasks.
