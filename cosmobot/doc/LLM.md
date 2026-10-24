# LLM providers

Select a named chat provider with `llm.chat`. The `api` setting chooses the wire
protocol, independently of the model name. Existing configurations default to
`api = "openai"` and keep their existing behavior.

| Setting | Meaning |
| --- | --- |
| `api` | `openai` for Chat Completions, `anthropic` for native Messages |
| `base_url` | API root including its version, without the method path |
| `model` | Model identifier accepted by the selected endpoint |
| `api_key` | Credential; configuration inspection exposes only configured/unset |
| `reasoning_effort` | OpenAI `reasoning_effort` or Anthropic `output_config.effort`; empty omits native effort |
| `thinking` | Optional `adaptive`, for Anthropic only; omitted uses the model default |
| `anthropic_auth` | `api_key` (default) sends `x-api-key`; `bearer` sends `Authorization: Bearer` |
| `max_tokens` | Positive native output limit, default `16384`; unused by OpenAI chat |
| `timeout` | Positive HTTP response timeout in seconds, default `60` |

Native providers require explicit `base_url` and `model`. Supported effort
levels and output limits depend on the model. For older Anthropic models without
effort support, set `reasoning_effort = ""` and omit `thinking`.

## Direct Anthropic

```toml
[llm]
chat = "claude"

[llm.chat_provider.claude]
api = "anthropic"
base_url = "https://api.anthropic.com/v1"
api_key = "YOUR_ANTHROPIC_KEY"
model = "claude-opus-5-5"
thinking = "adaptive"
reasoning_effort = "high"
max_tokens = 16384
timeout = 120
```

## Anthropic format through OpenRouter

The `openrouter_claude` table in [config.example.toml](../config.example.toml)
uses `api = "anthropic"`, `base_url = "https://openrouter.ai/api/v1"`,
`anthropic_auth = "bearer"`, and `model = "anthropic/claude-opus-5.5"`.
Supply an OpenRouter key in `api_key` and set `llm.chat = "openrouter_claude"`.
The transport appends `/messages`; unlike Anthropic SDK configuration, the
Cosmobot base URL already includes `/v1`.

Both forms support streamed text, local function tools, HTTP/base64 image
inputs, and cache-aware usage. Signed thinking blocks are preserved in stored
transcripts and replayed during tool turns. Incomplete or malformed streams,
unsupported output blocks, and unsupported stop reasons (including output-limit
truncation) fail before tools execute. Server-side tools and manual thinking
budgets are not implemented. Authentication headers are not forwarded across
redirects.

All configuration changes require a restart, including changes made through
Cosmoscope. Its controls come from the same typed schema. Image generation,
image editing, and speech providers continue to use OpenAI APIs.

## Implementation

`Bot.Effect.LLM` is the provider-neutral interface. `Bot.LLM` owns dispatch,
media resolution, and result normalization. Shared configuration, retry policy,
and HTTP/SSE lifecycle live in `Bot.LLM.Config`, `Bot.LLM.Retry`, and
`Bot.LLM.Stream`. `Bot.LLM.OpenAI` and `Bot.LLM.Anthropic` own their respective
wire formats. Provider transcript state is excluded from OpenAI requests/logs.

References: [Anthropic streaming](https://platform.claude.com/docs/en/build-with-claude/streaming),
[Anthropic thinking](https://platform.claude.com/docs/en/build-with-claude/thinking),
and [OpenRouter Messages](https://openrouter.ai/docs/api/api-reference/anthropic-messages/create-messages).
