# LLM providers

Select a named chat provider with `llm.chat`. The `api` setting chooses the wire
protocol, independently of the model name. Existing configurations default to
`api = "openai"` and keep their existing behavior.

| Setting | Meaning |
| --- | --- |
| `api` | `openai` for Chat Completions, `anthropic` for native Messages, `gemini` for native GenerateContent |
| `base_url` | API root including its version, without the method path |
| `model` | Model identifier accepted by the selected endpoint |
| `api_key` | Credential; configuration inspection exposes only configured/unset |
| `reasoning_effort` | OpenAI `reasoning_effort`, Anthropic `output_config.effort`, or Gemini `generationConfig.thinkingConfig.thinkingLevel`; empty omits native effort |
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

## Direct Gemini

Select `llm.chat = "gemini"` and supply a Google Gemini key in the example's
`[llm.chat_provider.gemini]` table. The endpoint is
`https://generativelanguage.googleapis.com/v1beta`; the transport appends
`/models/{model}:streamGenerateContent?alt=sse` and sends `x-goog-api-key`.

For Gemini 2.5, use `reasoning_effort = ""` to omit `thinkingLevel`.
For models supporting it (Gemini 3), set a supported level such as `low` or
`high`. Omit the Anthropic-only `thinking` setting.

Text, local function tools, image inputs, and token usage are supported.
Thought signatures stay attached to their original parts across tool turns and
stored transcripts. Missing function call IDs get unique local IDs; only native
IDs are echoed to Google. Imported historical calls use Google's documented
signature-validation sentinel. Blocked, malformed, and truncated responses fail
before tools execute.

HTTP image URLs are downloaded without the Gemini credential and encoded as
inline PNG/JPEG/GIF/WebP data. Typed base64 data URIs and Gemini Files API URIs
are also accepted. Inline requests are limited to 20 MB; use Files API URIs for
larger inputs. Vertex AI authentication, server-side tools, audio/video input,
and generated media are outside this chat transport.

## Implementation

`Bot.Effect.LLM` is the provider-neutral interface. `Bot.LLM` owns dispatch,
media resolution, and result normalization. Shared configuration, retry policy,
and HTTP/SSE lifecycle live in `Bot.LLM.Config`, `Bot.LLM.Retry`, and
`Bot.LLM.Stream`. `Bot.LLM.OpenAI`, `Bot.LLM.Anthropic`, and `Bot.LLM.Gemini` own their respective
wire formats. Provider transcript state is excluded from OpenAI requests/logs and summary
prompts.

References: [Anthropic streaming](https://platform.claude.com/docs/en/build-with-claude/streaming),
[Anthropic thinking](https://platform.claude.com/docs/en/build-with-claude/thinking),
[Gemini GenerateContent](https://ai.google.dev/api/generate-content),
[Gemini thought signatures](https://ai.google.dev/gemini-api/docs/generate-content/thought-signatures),
and [OpenRouter Messages](https://openrouter.ai/docs/api/api-reference/anthropic-messages/create-messages).
