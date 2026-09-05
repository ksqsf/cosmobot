Favor correctness, explicit data flow, small algebraic modules, and abstractions that make the code clearer in practice.

Packages: `cosmobot` (bot), `cosmobot-plugin-sdk` (plugin SDK), and `cosmocode` (RPC coding TUI).

## Architecture

Preserve this dependency direction:

`platform event -> core message -> route -> handler -> effect -> interpreter/concrete capability`

- Handlers own user-visible policy. They may call effects, but must not perform platform transport, database, LLM HTTP, or local-process work directly.
- Keep `Bot.Main` declarative: assemble configuration, stores/interpreters, routes, and drivers. `cosmobot/app/Main.hs` handles CLI dispatch.
- Keep algorithmic/domain modules independent of databases, filesystems, HTTP, processes, and platform APIs.

### Module ownership

- `Bot.Core.*`: platform-neutral messages, routes, replies, conversations, histories, and trees.
- `Bot.Effect.*`: narrow GADTs, smart constructors, and small adapters only. Put interpreters and state machines beside their owning implementation.
- `Bot.Chat.Driver.*`: platform APIs and normalized incoming messages. `Bot.Chat.*` owns shared chat behavior.
- `Bot.Agent.*`: agent calculus, runtime, tools, and middleware. Tools live in `Bot.Agent.Tools.*`; cross-cutting behavior lives in `Bot.Agent.Middleware.*`.
- `Bot.LLM.*`: LLM configuration, wire types, transport, retry, streaming, and test interpreters.
- `Bot.Storage.*`: component-owned durable queries, Selda tables, and SQLite wiring. Model queryable state as columns.
- `Bot.System.*`: local executable and OS integrations.

Pass narrow callbacks or data across implementation boundaries to keep imports acyclic.

## Haskell and effects

- Work in `Eff es`; add `IOE :> es` only at real external boundaries.
- Prefer `effectful` concurrency, process, timeout, reference, and filesystem capabilities where available.
- Use `trySync`/`catchSync` for ordinary failures and `bracket`, `mask`, `finally`, or `onException` for lifecycle safety. Never classify or swallow async exceptions as normal control flow.
- Express multi-step state changes as pure plans, component-owned operations, or bracket-style helpers.
- Use `aeson` for JSON, `Toml.Schema` and the local TOML machinery for configuration, and Selda through `Bot.Storage.Prelude` for queryable data.
- Add abstractions only when they remove real duplication or isolate an external system. Keep unrelated refactors separate from behavior changes.
- Preserve exported operations that complete an intentional interface; lack of in-repo callers alone does not establish dead code.
- For Haskell changes, use the local `haskell` skill and its `ghcid`/`.ghcid-errors` fast-feedback loop when practical.

## Agent runtime

- Keep `Bot.Agent.Core` limited to the generic calculus; persistence, audit, media, chat logging, platform linking, and handler policy belong outside it.
- Keep programs, runtimes, tools, and observers polymorphic in carrier `m`; specialize to `Eff es` at application boundaries.
- Start root and child agents through `Bot.Effect.Agent.withRun`. Origin and resource ownership are interpreter metadata installed with `withAgentMetadata`.
- Use `TurnState` only for data spanning model/tool turns. Keep middleware-private state lexical and pass dynamic middleware context through the runtime HList.
- Pick the narrowest middleware hook; consult `Runtime` in `cosmobot/lib/Bot/Agent/Core.hs` for available boundaries.
- Keep the immediate next-model transcript separate from the more aggressively compacted canonical/durable transcript; respect each view's tool-result limits. Keep media projection, audit projection, conversation storage, and chat-message linking as separate middleware responsibilities.
- Capture tool-emitted platform messages through chat interposition; do not return platform message ids in `ToolResult`.

Test agent policy, calculus laws, and deterministic failures/cancellation in `cosmobot/test/{Agent,Program,Failure}Spec.hs`, respectively.

## Concurrency, resources, and identity

- Use qualified `Bot.Effect.Concurrency` for application background work.
- Register a child before it can run. Manager exit must cancel and await every live child; exceptional exit propagates the top-level exception with `cancelWith` before awaiting.
- Treat async creation/register/start as one masked lifecycle operation and clean up partially acquired handles.
- Use `Bot.Resource` for person-owned, long-running in-memory objects. Scope them by `(platform, chatId, senderId)` and retain the creating agent run id.
- Never let managed objects escape `Resource.withResource`. Destruction makes the object unavailable, cancels and awaits users, then cleans up; restore an explicit removal if cleanup fails so it can be retried.
- `Bot.Resource` owns resource registration.
- Person state keys by platform/sender; room state keys by platform/chat. Message ids require platform and chat scope. Reject missing required identity.

## Configuration system

### Ownership and schema

- Each owner module defines a `Bot.Config.Schema.ConfigSchema source runtime` containing its real parser plus inspection metadata. Its `FromValue` instance delegates to that schema; owner-specific cross-field validation stays beside it.
- Supply complete option metadata, defaults/constraints, and source/effective getters using existing schema kinds; preserve mixed text/integer identities. Use path arrays.
- Sections declare explicit human-readable labels and groups, preserving acronyms such as `LLM`, `RPC`, `ACP`, `QQ`, `S3`, and `GC`.
- Optional sections model optional chat drivers. Repeatable sections model arbitrary named LLM chat, image, and audio providers.
- Secrets use `Schema.Secret`. JSON, diagnostics, logs, diffs, and `Show` expose only `configured`/`unset`, never credential text. Do not derive secret-bearing `Show` instances.
- `Bot.Config` only assembles owner schemas and retains startup source/runtime state and normalized active configuration; parsers live beside their consumers.

Use `[driver.*]`, `[handler.*]`, and `[llm]`/named provider tables as in `cosmobot/config.example.toml`.

Driver access lists and superusers belong to their driver.

When changing options, update the owner parser/schema, assembly if necessary, `cosmobot/config.example.toml`, consumers, and focused config/RPC/frontend tests. Each example option must appear exactly once in the schema.

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

- Handler admission starts in `Bot.Core.Route`; compose predicates. Use the existing non-blocking fork pattern for LLM/platform work.
- Agent tools update `Bot.Agent.Tools.*`, shared schemas in `Bot.Agent.Tools.Common`, `defaultTools`, and focused tests. Parse arguments with `AesonTypes.parseEither`.
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

Query structured fields directly, e.g. `journalctl -u cosmobot.service AGENT_RUN_ID=run-id`; discover fields with `journalctl -N`/`-F FIELD`. Native journald fields require `JOURNAL_STREAM`; otherwise logs use stderr.
