You are a proficient Haskell engineer working on cosmobot. Favor correctness, explicit data flow, small algebraic modules, and pleasing abstractions that can make the code actually clearer.

cosmobot is a unified chatbot framework. It is intentionally small enough to keep in one executable package, but module boundaries matter: keep platform details behind effects, keep user-facing behavior in handlers, keep agent tools inside the agent layer, and keep persistence rules close to the state they persist.

## Message Flow

Data enters through a concrete chat driver, is normalized into `IncomingMessage`, and passes through route admission. If no route matches, the message is ignored. If a route matches, it enters a handler for user-facing behavior.

Handlers do not provide real capabilities themselves. A handler applies command or conversation policy, then calls effects such as `Chat`, `LLM`, `Scheduler`, `AgentAudit`, or chat-log effects. Effect interpreters provide the concrete capability: they may call platform APIs, run LLM requests, use `IOE`, or read/write through Storage or Memory systems.

Keep this direction of dependency intact:

platform event -> core message -> route -> optional handler -> effects -> concrete capability

When a handler invokes the agent path, the nested flow is:

handler -> LLM effect -> agent loop -> LLM transport -> optional tool calls -> agent audit/observation -> agent result -> handler reply

## Architecture

cosmobot is organized around one dependency rule: user-facing behavior is written against normalized domain values and effects, while concrete platforms, transports, and persistence stay behind interpreters. This keeps a command handler from becoming a QQ/Telegram/Matrix client, a SQLite client, or an OpenAI client.

Read the system in layers:

1. `Core` is the shared vocabulary.
   It defines the platform-neutral values that every other layer agrees on: incoming message identity, route admission, reply-body directives, the pure `Conversation` value sent to the LLM, and the pure `ConversationTree` shape used to link replies. Core should be small and algebraic. It should not know how QQ, Telegram, Matrix, SQLite, Selda, or an LLM transport works.

2. `Handler` is the use-case layer.
   A handler is where a matched message becomes user-visible behavior: ask a question, continue a conversation, halt a stream, run a command, or reject an invalid request. Handlers make policy decisions and call effects. They should not perform platform transport, persistence, or LLM HTTP work directly.

3. `Effect` is the capability boundary.
   Effects express what handlers need: send chat messages, read chat logs, call an LLM, schedule work, record agent audit events, and so on. Interpreters decide how those capabilities are implemented. An interpreter may use `IOE`, call a platform API, run the OpenAI-compatible transport, or read/write through Storage or Memory.

4. `Chat`, `Storage`, `Memory`, and LLM transport are infrastructure.
   Chat drivers translate between concrete platforms and the normalized chat model. Storage owns durable persistence mechanics, including typed Selda tables and component-owned persistence modules such as `Bot.Storage.Conversation`. Memory owns persistent user/chat memory files. LLM transport owns request/response JSON and streaming protocol details. These modules are allowed to know external APIs and file/database formats because they are the boundary to those systems.

5. `Agent` is a domain engine used behind the LLM capability.
   The agent loop is not a message-routing layer. It is invoked by handler behavior through the LLM/agent path, manages conversation context, consumes LLM streaming responses, and handles tool calls requested by the model. Cross-cutting agent behavior lives in `Bot.Agent.Middleware.*`: observation/audit event emission, tool progress messages, tool-turn limits, failure recovery, and conversation context compaction. Tool implementations are agent internals grouped by domain, not top-level message handlers.

6. `Main` is the composition root.
   `app/Main.hs` reads config, creates storage, installs effect interpreters, starts chat drivers, registers routes, and connects incoming streams. It should stay mostly declarative: construct the graph, then run it.

The split exists so changes land where their reason lives:

- New platform behavior belongs in `Bot.Chat.Driver.*` and wiring, not in handlers.
- New user-visible commands belong in `Bot.Handler.*`, route composition, and focused tests.
- New agent tools belong in `Bot.Agent.Tools.*` and the agent tests, not in route admission.
- New persistent state belongs in a component-owned Storage module, Memory, AgentAudit, or the domain that owns the state rules. Keep table definitions and keying rules close to the code that queries them.
- New LLM wire behavior belongs in `Bot.Effect.LLM` and the agent path, not embedded inside a handler.

Concrete module ownership should follow those layers. Put shared message, route, pure conversation/tree values, and reply-body concepts in `Bot.Core.*`; user-visible flows in `Bot.Handler.*`; LLM transport and request shaping in `Bot.Effect.LLM`; agent audit events and query views in `Bot.Effect.AgentAudit`; platform adapters in `Bot.Chat.Driver.*`; SQLite connection mechanics in `Bot.Storage.SQLite`; typed table prelude/helpers in `Bot.Storage.Prelude`; component persistence such as stored conversation nodes in `Bot.Storage.*`; and persistent memory behavior in `Bot.Memory` or `Bot.Effect.Memory`.

## Design Rules

Use `effectful` for application capabilities and `streaming` for anything stream-like (incoming-message streams and LLM text streams). Keep `IOE :> es` explicit in effect interpreters and IO-bound helpers; use `liftIO` only at the boundary where real IO happens.

Prefer structured APIs over string manipulation: `aeson` for protocol JSON, `Toml.Schema` and local config parsers for TOML, and Selda tables/queries through `Bot.Storage.Prelude` for persisted state. Avoid new JSON payload collections for queryable state; model columns explicitly so the data can be queried and migrated.

Do not add indirection just to make the code look layered. Add an abstraction only when it removes real duplication, isolates an external system, or gives a growing responsibility a clear home.

Agent middleware is the extension point for cross-cutting agent behavior. Keep the core runner focused on model/tool recursion and conversation transitions. Put lifecycle observation in `Bot.Agent.Middleware.Observation`, conversation compaction in `Bot.Agent.Middleware.ContextCompaction`, and tool policy/progress/failure wrappers in `Bot.Agent.Middleware.Tools`. Middleware should exchange data through the typed `MiddlewareContext`/`Bot.Util.HList` carrier instead of adding ad hoc fields to `AgentRun` or `AgentContext`.

Middleware composition is written with `(.)`: the leftmost middleware is the outermost wrapper and receives the call first. Producers of typed middleware context must be outside consumers. For example, `withToolLimit` produces `ToolLimitContext`, so it wraps `withObservation`, which reads it. Keep this ordering explicit and let types enforce it.

`AgentContext` is for per-message tool capabilities and permissions only. Do not add handler callbacks or observation/audit plumbing to it. User-visible agent progress that belongs to middleware should use existing effects such as `Chat` from that middleware, not callbacks threaded through the core runner.

## Identity And Scope

Do not conflate chat identity with sender identity. Features scoped to people should normally key by `platform` and `senderId`; features scoped to conversations or rooms should key by `platform` and `chatId`.

Persisted user-visible state must be scoped defensively. If a feature cannot identify the required person, chat, or platform, reject clearly instead of guessing.

Message ids are not globally unique. Conversation state and any reply-indexed state should scope message ids by `platform` and the conversation chat identity, not by the bare message id.

Chat drivers decide platform-specific digest fields such as allowed chat, superuser sender, and configured-bot mention. Handler config should not parse mentions or duplicate platform whitelists.

## Configuration

`Bot.Config` is the top-level assembler for runtime config. Concrete parsers belong next to the domain that owns the setting: driver parsers under `Bot.Chat.Driver.*.Config`, handler parsers under `Bot.Handler.*.Config`, LLM config under `Bot.Effect.LLM.Config`, agent config under `Bot.Agent.Config`, and memory config under `Bot.Memory.Config`.

Keep `config.toml` section ownership explicit:

- Chat platform settings live under `[driver.qq]`, `[driver.telegram]`, and `[driver.matrix]`.
- Handler settings live under sections such as `[handler.ask]` and `[handler.saucenao]`.
- LLM settings live under `[llm]`; agent behavior belongs under agent-owned config.
- QQ conversation access belongs in `allowed_groups` and `allowed_users`; Telegram access belongs in `allowed_chats`; Matrix access belongs in `allowed_rooms`.
- Administrator access belongs in each driver's `superusers`.

Do not reintroduce top-level `[qq]`, `[telegram]`, `[matrix]`, `[saucenao]`, `[handlers.*]`, or handler-owned platform whitelist sections.

When adding config, update the owning `*.Config` parser, `Bot.Config`, `config.example.toml`, and every call site that consumes the changed runtime config. If the config affects OpenAI-compatible requests, update request serialization too.

## Change Guidelines

For handler behavior, start from route admission in `Bot.Core.Route`. Handlers should compose shared predicates and route combinators instead of reimplementing admission logic. If the work may call LLM or platform APIs, follow the existing `forkEff` pattern so incoming stream consumption stays responsive.

For platform behavior, keep API and transport details in `Bot.Chat.Driver.QQ`, `Bot.Chat.Driver.Telegram`, `Bot.Chat.Driver.Matrix`, or dispatch glue in `Bot.Chat.Driver`. Do not leak platform-specific request/response types into handlers or agent tools.

For agent tools, put implementations in the appropriate `Bot.Agent.Tools.*` module, keep shared schema and argument helpers in `Bot.Agent.Tools.Common`, update `defaultTools`, parse arguments with `AesonTypes.parseEither`, and add focused coverage in `test/AgentSpec.hs`. Set `Tool.noisy = True` only for tools whose execution can take long enough that the user needs an immediate progress message; noisy tool announcements are handled by middleware and should include the audit id when observation produced one.

For agent observation and audit behavior, keep event emission in `Bot.Agent.Middleware.Observation`, persistent audit records and query views in `Bot.Effect.AgentAudit`, and user-visible audit commands in `Bot.Handler.Audit`. `RecordEvent` may return the persisted audit id; Observation converts that into its own `ObservationContext` for later middleware. Do not put audit formatting or query policy inside tool implementations.

For agent conversation compaction, keep the behavior in `Bot.Agent.Middleware.ContextCompaction`. The current policy treats 50 messages as the hard limit and preserves the most recent 20 messages verbatim, compacting older history through the `LLM` effect into one summary message. If a compaction boundary would leave leading `tool` messages in the preserved tail, include those tool results in the compacted prefix to preserve OpenAI-compatible tool-call history.

For persistence, prefer Storage or Memory ownership over handler-local files or bespoke SQL. Keep keying rules close to the state they persist, and use normalized identities. A component that owns durable state should expose focused operations from its own module, while `Bot.Effect.Storage` stays a small effect for running storage actions rather than a registry of every table.

For new modules, update `cosmobot.cabal` for the executable and relevant test suites. Prefer shared `common` module-list stanzas over copying the same `other-modules` block into multiple components.

Avoid broad refactors while implementing behavior. Keep architecture cleanup separate from feature changes when practical.

## Current Pressure Points

- `Bot.Effect.LLM` still carries several responsibilities: public LLM effect API, OpenAI-compatible request/response types, SSE streaming transport, tool-call JSON, and image-generation request shaping. Be careful when extending it; cohesive transport/request splits are preferable to more unrelated helpers.
- `Bot.Handler.Ask` remains the densest user-facing flow. Conversation start, continuation, reply handling, private/mention behavior, streaming presentation, and `!halt` rules should stay platform-neutral and effect-driven.
- `Bot.Agent` owns the core loop and default middleware composition. Keep it from becoming a grab bag of callbacks; when adding cross-cutting behavior, prefer a focused `Bot.Agent.Middleware.*` module and typed middleware context.
- Conversation persistence lives in `Bot.Storage.Conversation`. `Bot.Core.Conversation` owns the pure immutable LLM history and reply tree shape; do not move stores, tables, active stream handles, cache policy, or database ids back into Core.
- Config ownership is distributed across focused `*.Config` modules, but `Bot.Config` is still the integration point. Keep it as assembly glue, not a place for concrete section semantics.
- Chat drivers share a conceptual contract but necessarily differ in platform APIs. Keep normalized digest construction consistent across drivers, especially access, superuser, bot mention, sender, and chat identity fields.
- Test suites cover several domains but still have repeated message/effect fixtures. If new tests copy more `IncomingMessage` or interpreter setup, introduce shared test helpers instead.

## Verification

- Run `cabal test agent-spec` for agent/tool/conversation changes.
- Run `cabal test all` before committing behavior changes that touch shared modules.
- Run `cabal build cosmobot` after changing executable wiring, config, cabal module lists, or handler signatures.
- Run `git diff --check` before commit.
- Keep unrelated untracked files out of commits unless explicitly requested.
