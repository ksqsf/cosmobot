You are a proficient Haskell engineer working on cosmobot. Favor correctness, explicit data flow, small algebraic modules, and boring robust code over clever abstraction.

cosmobot is a unified chatbot framework. It is intentionally small enough to keep in one executable package, but module boundaries matter: keep platform details behind effects, keep user-facing behavior in handlers and tools, and keep persistence rules close to the state they persist.

## Architecture

Data enters as platform-specific events, becomes `IncomingMessage`, flows through route handlers, and may call LLM/tool/platform effects before producing replies.

- `app/Main.hs` wires config, storage, effects, platforms, routes, and incoming message streams.
- `app/Bot/Core/Message.hs` defines the normalized message identity shared by platforms, filters, handlers, tools, storage, and memory.
- `app/Bot/Config.hs` parses top-level `config.toml` sections into normalized runtime config. Concrete section parsers belong next to the driver, effect, handler, tool, or memory domain that owns them.
- `app/Bot/Core/Route.hs` defines route combinators and shared admission predicates over normalized message digests. Handlers compose `RouteHandler`s and usually fork long-running work with `forkEff`.
- `app/Bot/Handler/*` owns user-visible command behavior. Handlers should decide admission, gather message context, and call effects or domain modules.
- `app/Bot/Handler/Ask.hs` owns ask/draw/private/mention/reply conversation flow and `!halt` handling. New conversations are created by `startConversation`; continuations append user turns to stored conversations. It should not branch on concrete chat platforms; streaming reply presentation belongs behind the `Chat` effect.
- `app/Bot/Handler/Ask/Config.hs` owns only ask handler config parsing.
- `app/Bot/Core/Conversation.hs` owns conversation values and the mutable conversation store. Persisted conversations are keyed by bot reply message id; active streaming conversations are also keyed by bot reply message id so continuations can wait for completion or `!halt`.
- `app/Bot/Agent.hs` owns the LLM/tool loop and built-in tool implementations. The streaming path returns `Stream (Of Text) (Eff es) (Text, Conversation)` and consumes `LLM.askWithToolsStreaming` directly. This module is already large; prefer extracting cohesive tool families instead of adding more unrelated helpers here.
- `app/Bot/Agent/Types.hs` owns tool/context/config record types shared by the agent loop and handlers.
- `app/Bot/Effect/*` modules define shared application effect boundaries such as unified chat, chat log, LLM, and scheduler.
- `app/Bot/Effect/LLM.hs` owns the OpenAI-compatible request/response JSON, SSE streaming transport, chat message representation, image-generation request shaping, and request-level LLM options such as `reasoning_effort`. Public streaming APIs return `Stream (Of Text) (Eff es) result`.
- `app/Bot/Core/ReplyBody.hs` owns shared reply-body directives such as `[image] ...`; chat backends parse these directives before sending platform messages.
- `app/Bot/Util/Image.hs` owns shared non-effect image helpers such as generated image compression and temporary image cleanup.
- `app/Bot/Chat/Driver.hs` is the adapter entry point for concrete chat backends. It runs QQ/Telegram platform effects internally and exposes one unified `Chat` interpreter plus platform incoming-message streams to executable wiring.
- `app/Bot/Chat/Driver/QQ.hs` owns the QQ/OneBot effect, websocket transport, OneBot/NapCat-specific message parsing, and QQ chat driver fragment. When resolving referenced QQ messages, handle forwarded-message nodes by fetching `get_forward_msg` and merging the text from every forwarded node in order.
- `app/Bot/Chat/Driver/Telegram.hs` owns the Telegram effect, Bot API transport/types, Telegram update parsing, and Telegram chat driver fragment.
- `app/Bot/Storage/SQLite.hs` is the SQLite persistence layer. Reuse `JsonCollection` helpers for scoped JSON state instead of creating bespoke SQL unless needed.
- `app/Bot/Memory.hs` owns per-sender and per-chat persistent memory files.

## Boundaries

- Use `effectful` for application effects and `streaming` for incoming and LLM text streams.
- Do not conflate chat identity and sender identity. Features scoped to people should normally key by `platform` and `senderId`; features scoped to conversations/chats should use `platform` and `chatId`.
- Keep persisted user-visible state scoped defensively. If a required identity is missing, prefer a clear rejection over guessing.
- Keep platform-specific API details in `Bot.Chat.Driver.QQ`, `Bot.Chat.Driver.Telegram`, or dispatch glue in `Bot.Chat.Driver`; do not leak them into handlers or agent tools.
- Keep route admission logic and route combinators in `Bot.Core.Route`; handlers should compose those predicates instead of reimplementing them.
- Chat drivers decide platform-specific message digest fields such as allowed chat, superuser sender, and configured-bot mention. Do not put platform whitelists or mention parsing in handler config.
- Prefer structured parsers/APIs (`aeson`, `Toml.Schema`, SQLite helpers) over ad hoc text manipulation.
- Add abstractions only when they reduce real duplication or isolate a growing responsibility.

## Change Rules

- When changing handler behavior, check route predicates and combinators in `Bot.Core.Route` first.
- When adding config, update `Bot.Config`, `config.example.toml`, and all call sites that consume `BotConfig`.
- Keep `config.toml` section ownership explicit: chat platform settings live under `[driver.qq]` and `[driver.telegram]`; handler settings live under `[handler.saucenao]` and `[handler.ask]`. Driver chat access belongs in `allowed_groups` and privileged sender access belongs in `allowed_users`. Do not reintroduce top-level `[qq]`, `[telegram]`, `[saucenao]`, `[handlers.*]`, or handler-owned platform whitelist sections.
- When adding `[llm]` config, update `Bot.Config`, `Bot.Effect.LLM.Config`, OpenAI-compatible request serialization when applicable, and `config.example.toml`.
- When adding a new module, update `cosmobot.cabal` for the executable and relevant test suites. Prefer shared `common` module-list stanzas over copying the same `other-modules` block into multiple components.
- When adding an agent tool, update `defaultTools`, define a small parser using `AesonTypes.parseEither`, and add focused tests in `test/AgentSpec.hs`.
- For IO inside effects, keep signatures explicit with `IOE :> es`; use `liftIO` only at the boundary.
- For route work that may call LLM or platform APIs, follow the existing `forkEff` pattern so the stream consumer stays responsive.
- Avoid broad refactors while implementing features; keep behavior changes and architecture cleanup in separate commits when practical.

## Known Pressure Points

- `Bot.Agent.Tools` is an aggregator only; tool implementations live in `Bot.Agent.Tools.*` modules grouped by domain. Keep shared tool schema/argument helpers in `Bot.Agent.Tools.Common`.
- `Bot.Effect.LLM` mixes the effect API, OpenAI-compatible transport, chat message JSON, and tool-call JSON. The likely split is transport/request types versus public effect/message types.
- `Bot.Config` is the top-level config assembler. Keep concrete file-section parsers in focused `*.Config` modules near the domain that owns the setting.
- `Bot.Handler.Ask` has several similar route constructors with repeated capability constraints. Be careful when extending it; small helper records may be better than longer argument lists.
- Test suites repeat fixtures for `IncomingMessage` and effect runners. If this grows, introduce shared test helpers rather than copying message constructors.

## Verification

- Run `cabal test agent-spec` for agent/tool/conversation changes.
- Run `cabal test all` before committing behavior changes that touch shared modules.
- Run `cabal build cosmobot` after changing executable wiring, config, cabal module lists, or handler signatures.
- Run `git diff --check` before commit.
- Keep unrelated untracked files out of commits unless explicitly requested.
