# cosmobot-plugin

Dependency-free Python SDK for executable cosmobot plugins.

## Install

From this directory:

```sh
python3 -m pip install .
```

For development without installing:

```sh
PYTHONPATH=src python3 examples/echo.py
```

## Minimal echo plugin

```python
from dataclasses import dataclass, field

from cosmobot_plugin import Context, Plugin, serve

plugin = Plugin("1.0.0", ["chat"])


@plugin.command("!echo", "Repeat the supplied text.")
async def echo_command(context: Context, arguments: str) -> None:
    await context.reply(arguments)


@dataclass
class EchoArgs:
    text: str = field(metadata={"description": "Text to repeat."})


@plugin.tool("echo", "Repeat text.")
async def echo_tool(context: Context, arguments: EchoArgs) -> str:
    return arguments.text


serve(plugin)
```

Handlers are async functions registered by decorators. Tool parameters come
from the annotated dataclass; its fields, required values, defaults, and
`description` metadata become the JSON schema. Returning text from a tool
creates a successful tool result. Models see its name as `<plugin-id>__echo`.

## Runtime rules

- Declare only the host callbacks used by the plugin: `chat`, `llm`, `agent`,
  or `media`. An undeclared capability is rejected.
- The host supplies `timeoutSeconds` for every invocation. The SDK validates
  it and cancels and awaits a timed-out handler; time spent in plugin locks
  counts toward the timeout.
- A `Context` and its callbacks are valid only until that handler returns.
- Use `serve_with(plugin, initialize, finalize)` for owned state. The state is
  available as `context.state`; after successful initialization,
  `finalize(state)` runs exactly once during shutdown.
- stdout is reserved for JSON-RPC. Send application logs to stderr.
- For an explicitly retryable process failure, import
  `transient_process_failure` from `cosmobot_plugin` and call it; it logs to
  stderr and exits with 75.

## Bundle

Install the executable and configuration as one immediate child of the
configured plugin directory:

```text
plugins/
└── echo/
    ├── echo          # executable with a Python shebang
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

Make `echo` executable. Cosmobot exposes the configuration path through
`COSMOBOT_PLUGIN_CONFIG`; tables other than `[plugin]` belong to the plugin.

See [External plugins and protocol](../cosmobot/doc/Plugins.md) for bundle
lifecycle, filters, host callbacks, sandboxing, and the JSON-RPC contract.
