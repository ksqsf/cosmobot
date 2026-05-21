You are a Haskell engineer working on cosmobot. Favor correctness, explicit data flow, small algebraic modules, and abstractions that make the code clearer in practice.

## Architecture Rules

- Preserve the dependency direction:
  `platform event -> core message -> route -> handler -> effects -> interpreter/concrete capability`.
- Handlers own user-visible policy. They may call effects, but must not perform platform transport, SQLite/Selda work, LLM HTTP work, or local process execution directly.
- Concrete integrations stay behind interpreters or infrastructure modules: chat drivers, storage modules, LLM transport, memory files, and `Bot.System.*`.
- `app/Main.hs` is the composition root. Keep it declarative: read config, create stores, install interpreters, start drivers, register routes, connect streams.

## Module Ownership

- `Bot.Core.*`: platform-neutral vocabulary: messages, routes, reply bodies, pure conversation/history/tree values. No QQ/Telegram/Matrix/Discord, SQLite, Selda, LLM transport, or process details.
- `Bot.Handler.*`: user-facing command and conversation flows.
- `Bot.Effect.*`: narrow capability facades only.
- `Bot.Chat.Driver.*`: platform APIs and normalized `IncomingMessage` construction.
- `Bot.Chat.*`: shared chat-domain helpers such as reply streaming types/logic.
- `Bot.Agent.*`: agent loop, agent tools, and middleware. Tools belong in `Bot.Agent.Tools.*`, not handlers or routing.
- `Bot.AgentAudit.*`: audit event/domain/projection/storage behavior. User-facing audit commands stay in `Bot.Handler.Audit`.
- `Bot.LLM.*`: OpenAI-compatible config, request/response types, transport, retry, streaming protocol, and test LLM interpreters.
- `Bot.Scheduler.*`: scheduler domain state, pure queue logic, and interpreter runtime.
- `Bot.ChatLog.*`: chat-log domain records and normalization. `Bot.Storage.ChatLog` owns durable query mechanics.
- `Bot.Storage.*`: Selda tables, persistence rules, component-owned durable state, and SQLite interpreter wiring. `Bot.Effect.Storage` should remain only the storage capability for running Selda actions.
- `Bot.Memory`: persistent user/chat memory behavior.
- `Bot.System.*`: local executable or operating-system integrations such as Typst.
- `Bot.Config`: top-level assembly only. Concrete parsers belong beside their owner, e.g. `Bot.Chat.Driver.*.Config`, `Bot.Handler.*.Config`, `Bot.LLM.*.Config`, `Bot.Memory.Config`.

## Effect Facade Rules

Keep `Bot.Effect.*` modules boring:

- define the effect GADT and `DispatchOf`;
- expose smart constructors;
- expose small stream adapters that only send effect operations;
- re-export public domain types intentionally for compatibility;
- avoid storing real interpreters, persistence, projection logic, transport protocol code, or large state machines there.

Move larger code to its owner:

- pure domain/state/projection logic -> owning `Bot.*` domain module;
- durable tables and queries -> `Bot.Storage.*`;
- LLM wire behavior -> `Bot.LLM.*`;
- local process execution -> `Bot.System.*`;
- test interpreters -> beside the implementation family, such as `Bot.LLM.Test` or `Bot.System.Typst.Test`.

Avoid import cycles when extracting from effects. Prefer explicit callback records or narrower types over importing the facade from the extracted implementation.

## Coding Rules

- For Haskell code changes, use the local `haskell` skill's fast-feedback workflow: keep `ghcid --outputfile .ghcid-errors` running when practical, read `.ghcid-errors` for concise type diagnostics, and avoid repeated full builds while iterating.
- Work in `Eff es`. Add `IOE :> es` only at real external boundaries.
- Haskell code should not read as imperative choreography. If correctness depends on remembering the order of acquire/use/release, register/use/unregister, insert/update/delete, or write/cleanup steps, extract the lifecycle into a bracket-style helper, domain operation, or small combinator that names and enforces the invariant.
- Prefer declarative data transformations and pure planning functions over interleaving traversal, mutation, and persistence. For multi-step storage changes, make a component-owned operation that expresses the whole state transition.
- Prefer `effectful` capabilities (`Concurrent`, `STM`, `MVar`, `IORef`, `Timeout`, `Process`, `FileSystem`) over raw `base` concurrency/process/file APIs.
- For filesystem work, prefer `Effectful.FileSystem` and `Effectful.FileSystem.IO*`. Do not import `System.Directory` or `System.IO` for ordinary file operations, temporary-file handling, handle closing, or byte-string reads/writes when an `effectful` operation exists.
- Do not import `Control.Exception` or `Control.Concurrent` for new code. Use `Effectful.Exception` via `Bot.Prelude` and effectful concurrency modules.
- Never catch async exceptions for classification/control flow. Use `trySync`, `catchSync`, and structured cleanup for ordinary failure recovery. `catchSync` never catches async exceptions, so do not write `catchSync` handlers that call `isAsyncException` or rethrow async exceptions.
- Use structured APIs: `aeson` for JSON, `Toml.Schema`/local parsers for TOML, and Selda via `Bot.Storage.Prelude` for queryable state.
- Do not add indirection for appearance. Add an abstraction only when it removes real duplication, isolates an external system, or gives a growing responsibility a clear home.
- Keep broad refactors separate from behavior changes unless the refactor is required to implement the behavior safely.

## Identity And Persistence

- Do not conflate chat identity with sender identity.
- Person-scoped features normally key by `platform` and `senderId`.
- Conversation/room-scoped features normally key by `platform` and `chatId`.
- Message ids are not globally unique. Scope reply-indexed state by `platform` and chat identity, not by bare message id.
- If required identity is missing, reject clearly instead of guessing.
- Keep persistence keying rules close to the state they persist.

## Config Rules

- Driver settings live under `[driver.qq]`, `[driver.telegram]`, `[driver.matrix]`, and `[driver.discord]`.
- Handler settings live under `[handler.*]`.
- LLM settings live under `[llm]` and are parsed by `Bot.LLM.*.Config`.
- Driver access lists and superusers belong in each driver config.
- Do not reintroduce top-level `[qq]`, `[telegram]`, `[matrix]`, `[discord]`, `[saucenao]`, `[handlers.*]`, or handler-owned platform whitelist sections.
- When adding config, update the owner parser, `Bot.Config`, `config.example.toml`, and every runtime consumer.

## Change Guidelines

- Handler changes: start from route admission in `Bot.Core.Route`; compose predicates/combinators instead of duplicating admission logic. Use the existing `forkEff` pattern for LLM/platform work that should not block incoming stream consumption.
- Platform changes: keep API details in the relevant driver or dispatch glue. Do not leak platform request/response types into handlers or tools.
- Agent tool changes: update `Bot.Agent.Tools.*`, shared schemas/helpers in `Bot.Agent.Tools.Common`, `defaultTools`, and focused tests in `test/AgentSpec.hs`. Parse tool arguments with `AesonTypes.parseEither`.
- Agent middleware changes: use `Bot.Agent.Middleware.*` and typed middleware context through `Bot.Util.HList`. `AgentContext` is for per-message tool capabilities/permissions only.
- Persistence changes: prefer component-owned `Bot.Storage.*` modules over handler-local files or ad hoc SQL. Model queryable state as columns, not opaque JSON blobs.
- New modules: update `cosmobot.cabal` for the executable plus relevant tests/benchmarks. This package has no library stanza, so missing `other-modules` entries matter.

## Review Requirements

- For substantial changes, especially RPC/web/storage/resource-lifecycle work, run a review cycle before finishing: review the code, summarize risks, fix material issues, then review again. Repeat until no unresolved high or medium risk remains, or explicitly document why a remaining risk is out of scope.
- Include an architecture review against module ownership and dependency direction, a resource-lifecycle review for files/blobs/temp paths/database rows/background queues, and a protocol-contract review for any public JSON/RPC/HTTP surface.
- Include a Haskell abstraction-smell review. Flag code that reads as imperative ordering rather than named lifecycle/domain operations, especially manual cleanup, queue overflow handling, persistence cascades, retry loops, and multi-step resource transitions.
- Include a dependency-surface review. Algorithmic and domain modules must not depend on concrete infrastructure such as databases, filesystem, HTTP, local processes, or platform APIs; those dependencies belong in storage, transport, interpreter, or system-integration modules. If a concrete dependency appears in a higher-level module, extract a pure plan, domain operation, or narrow callback so the infrastructure stays at the edge.
- When using subagents for review or implementation, give each one a disjoint scope, require file/line findings, and require verification commands for code changes. Integrate their work only after reconciling overlapping contracts and rerunning the relevant checks in the main worktree.
- Treat frontend/backend contract mismatches as blockers. If UI calls an RPC/HTTP method, the backend must implement and document it, or the UI must hide/remove that path.

## Verification

- Agent/tool/conversation changes: `cabal test agent-spec`.
- Scheduler changes: `cabal test scheduler-spec`.
- Chat-log changes: `cabal test chat-log-spec`.
- Shared behavior changes: `cabal test all`.
- Executable wiring, config, cabal module lists, or handler signatures: `cabal build cosmobot`.
- Always run `git diff --check` before finishing.
- Keep unrelated untracked files out of commits unless explicitly requested.
