# External plugins

Cosmobot loads executable plugin bundles from `[plugins].plugin_dir` at startup. A relative directory is resolved beside the main configuration file. Only immediate subdirectories are bundles, and a bundle named `echo` must contain executable `echo` and `config.toml` files.

```toml
[plugins]
plugin_dir = "plugins"
```

```text
plugins/
└── echo/
    ├── echo
    └── config.toml
```

The reserved lifecycle table is:

```toml
[plugin]
required = false
sandboxed = true
route_timeout_seconds = 10
tool_timeout_seconds = 300
restart_limit = 3
```

All other tables belong to the plugin and remain available through `COSMOBOT_PLUGIN_CONFIG`. Cosmobot does not send their contents over the protocol. Installed bundles load on every restart. Superusers can use `!plugin/list`, `!plugin/load <id>`, `!plugin/reload <id>`, and `!plugin/unload <id>`; required plugins cannot be unloaded.

## Protocol

The process reads and writes JSON-RPC 2.0 objects as UTF-8, one object per line, with a 1 MiB line limit. Standard output is protocol-only; logs go to standard error. Requests may overlap, so IDs must be correlated and writes synchronized.

Cosmobot sends `plugin.initialize`, `plugin.route.invoke`, `plugin.tool.invoke`, and `plugin.shutdown`. Initialization returns:

```json
{
  "protocolVersion": "1.0.0",
  "pluginVersion": "1.0.0",
  "routes": [],
  "filters": {},
  "tools": [],
  "requestedCapabilities": ["chat"]
}
```

Host request parameters are:

| Method | Parameters |
| --- | --- |
| `plugin.initialize` | `{protocolVersion, pluginId}` |
| `plugin.route.invoke` | `{invocationId, routeId, message, arguments, timeoutSeconds}` |
| `plugin.tool.invoke` | `{invocationId, tool, message, arguments, timeoutSeconds}` |
| `plugin.shutdown` | `null` |

`timeoutSeconds` is a positive integer. SDKs start it immediately before entering the handler, cancel and await timed-out handlers, then return an invocation error. On shutdown they cancel and await every active handler, run the registered finalizer exactly once, and only then reply. An initialization failure is permanent unless its JSON-RPC error contains `data.transient: true`. After successful initialization, an explicit transient process failure exits with status 75; other exits are permanent.

Routes refer to a filter ID and contain `{id, help, filter, disposition, access}`. Filter nodes are bounded expressions using `all`, `any`, `not`, `command`, `prefix`, `platform`, `event`, `chatKind`, `reply`, `mention`, or `access`. Tools contain `{name, description, schema}` and are exposed to models as `<plugin>__<tool>`.

During a handler, a plugin may call only capabilities declared at initialization:

| Method | Parameters | Result |
| --- | --- | --- |
| `chat.reply` | `{invocationId, text}` | platform reply value |
| `chat.referenced` | `{invocationId}` | referenced message or `null` |
| `llm.complete` | `{invocationId, prompt}` | text |
| `agent.run` | `{invocationId, prompt}` | text |
| `media.resolve` | `{invocationId, ref}` | `{canonicalReference, mimeType, size, publicUrl, localPath}` |

`media.resolve` accepts only an existing canonical `media:mf_...` reference. The invocation ID expires when its handler returns; the host cancels and awaits callbacks still owned by that invocation. Normalized `message` fields use lowercase `platform` (`qq`, `telegram`, `matrix`, `discord`, `rpc`, `acp`), `eventKind` (`created`, `deleted`), and `kind` (`private`, `group`, `channel`, or a platform value). All other named fields are nullable or arrays according to `IncomingMessage`; `raw` is deliberately unstable.

Invalid request parameters use JSON-RPC `-32602`. Undeclared capabilities and expired invocations use host errors. Tool argument errors with `-32602` become permanent argument failures; other invocation errors are transient. Oversized frames are rejected without entering normal routing. Responses and requests may arrive in any order.

## Sandboxing

Sandboxed bundles run through Bubblewrap with the bundle writable at `/plugin` except for the read-only `/plugin/config.toml`, the media cache read-only at `/media`, a private `/tmp`, shared networking, a rebuilt environment, dropped capabilities, disabled nested user namespaces, and parent-bound process-group cleanup. Sandbox setup failure never falls back to unsandboxed execution. Set `sandboxed = false` only for trusted executables that should inherit the cosmobot OS user's access.

## SDKs and echo demo

- The [Haskell SDK](../../cosmobot-plugin-sdk/README.md) is a standalone package exposing `Cosmobot.Plugin`. Its declaration DSL is Applicative (not Monad); invocation handlers are monadic.
- The [Python SDK](../../cosmobot-plugin-python/README.md) is standalone and dependency-free, with async decorators and dataclass-derived tool schemas.
- `examples/plugins/echo/` replaces the former built-in `!echo` route. Run `make echo-plugin` at the repository root to create a self-contained zipapp bundle under `plugins/echo`; the sandbox never depends on host site-packages or a virtualenv.

Plugins own their state and synchronization. The host deliberately provides no cross-plugin calls, retained invocation context, execution queue, background-task API, raw database, arbitrary filesystem/process access, memory, scheduler, streaming agent, or steering API.
