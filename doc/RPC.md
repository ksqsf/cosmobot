# Cosmobot RPC Protocol

Cosmobot can expose a local WebSocket RPC endpoint from `cosmobot serve`.
The RPC service is intended for local browser chat, audit inspection, and CLI
queries against the running daemon. The wire envelope is JSON-RPC 2.0 over
WebSocket, using the Haskell `jsonrpc` package for protocol types.

## Configuration

RPC is configured under `[rpc]` in `config.toml`:

```toml
[rpc]
enabled = false
host = "127.0.0.1"
port = 38765
token = ""
static_dir = "web/dist"
```

`enabled` defaults to `false`. When `enabled = true`, `token` must be non-empty.
The default host is loopback-only. `static_dir` is the directory served at `/`;
if `static_dir/index.html` is absent, `/` falls back to `web/rpc.html`.

## Authentication

Clients authenticate during the WebSocket handshake with either a query token:

```text
ws://127.0.0.1:38765/?access_token=TOKEN
```

The preferred WebSocket endpoint is:

```text
ws://127.0.0.1:38765/rpc?access_token=TOKEN
```

The legacy root WebSocket path remains accepted for compatibility.

or an HTTP header:

```text
Authorization: Bearer TOKEN
```

Unauthorized connections are rejected with HTTP 401.

## Envelopes

Requests are JSON-RPC 2.0 objects with `jsonrpc: "2.0"`, `id`, `method`, and
optional `params`:

```json
{"jsonrpc":"2.0","id":"1","method":"audit.recent","params":{"limit":20}}
```

Successful responses include `result`:

```json
{"jsonrpc":"2.0","id":"1","result":[]}
```

Failed responses include `error`:

```json
{"jsonrpc":"2.0","id":"1","error":{"code":-32601,"message":"Unknown RPC method: x","data":{"code":"method_not_found"}}}
```

Notifications have no `id`:

```json
{"jsonrpc":"2.0","method":"audit.event","params":{}}
```

Standard JSON-RPC numeric error codes are used. Cosmobot's stable textual error
code is preserved in `error.data.code` where applicable.

## Audit Methods

### `audit.recent`

Returns recent audit records. `limit` is optional and defaults to `20`.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.recent","params":{"limit":50}}
```

### `audit.get`

Returns one audit record by audit id. The preferred parameter is `audit_id`;
`id` is also accepted.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.get","params":{"audit_id":123}}
```

### `audit.conversation`

Returns audit records associated with one message id.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.conversation","params":{"message_id":"telegram-42"}}
```

### `audit.conversation_messages`

Returns audit records associated with multiple message ids.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.conversation_messages","params":{"message_ids":["m1","m2"]}}
```

### `audit.subscribe`

Acknowledges audit live updates. Current broadcasts are global to connected RPC
clients; this method does not create per-client subscription state yet.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.subscribe","params":{}}
```

Result:

```json
{"subscribed":true}
```

Persisted audit records are broadcast as `audit.event` notifications:

```json
{"jsonrpc":"2.0","method":"audit.event","params":{ /* AgentAuditRecord JSON */ }}
```

Live audit events report newly persisted records. Query methods keep the normal
audit storage behavior, including stale running-tool marking.

## Chat Methods

RPC chat is exposed to the bot as the virtual `PlatformRPC` platform. Incoming
messages use the normal route, agent, memory, chat-log, and audit path.

RPC chat sessions are private chats. The RPC sender is allowed and treated as a
superuser because possession of the RPC token is the authorization boundary.

### `chat.open_session`

Creates a virtual chat session. `label` is optional.

```json
{"jsonrpc":"2.0","id":"1","method":"chat.open_session","params":{"label":"browser"}}
```

Result:

```json
{"sessionId":"browser-1"}
```

Blank labels are ignored and use the base name `session`.

### `chat.send`

Injects a user message into the virtual RPC chat session.

```json
{
  "id": "2",
  "jsonrpc": "2.0",
  "method": "chat.send",
  "params": {
    "sessionId": "browser-1",
    "text": "!ask hello",
    "imageUrls": [],
    "replyToMessageId": null
  }
}
```

Accepted aliases:

- `sessionId` or `session_id`
- `imageUrls` or `image_urls`
- `replyToMessageId` or `reply_to_message_id`

Result:

```json
{"sessionId":"browser-1","messageId":"rpc-1"}
```

The sent user message is also broadcast as a `chat.message` notification with
`sender: "user"`.

## Chat Notifications

Bot replies are sent as `chat.message` notifications:

```json
{
  "method": "chat.message",
  "jsonrpc": "2.0",
  "params": {
    "sessionId": "browser-1",
    "messageId": "rpc-2",
    "text": "reply text"
  }
}
```

Editable reply stream updates are sent as `chat.message_update`:

```json
{
  "method": "chat.message_update",
  "jsonrpc": "2.0",
  "params": {
    "sessionId": "browser-1",
    "messageId": "rpc-2",
    "text": "updated reply text"
  }
}
```

User-originated chat notifications include `sender`, `imageUrls`, and
`replyToMessageId` fields. Bot-originated reply notifications currently include
`sessionId`, `messageId`, and `text`.

## CLI

The CLI reads `[rpc]` from `config.toml` and connects to the running daemon:

```sh
cosmobot rpc audit recent --limit 20
cosmobot rpc audit show 123
cosmobot rpc audit conversation MESSAGE_ID
cosmobot rpc call METHOD JSON
```

Responses are printed as pretty JSON.

Use `cosmobot rpc --config FILE ...` to read a config file other than
`config.toml`.

## Browser UI

When RPC is enabled, the same HTTP port serves the browser UI from
`static_dir`. A Svelte/Vite build should place its output in `web/dist`; while
that build is absent, `/` serves the legacy `web/rpc.html` client. The browser
client connects to `/rpc`, creates chat sessions, sends prompts, shows chat
reply updates, lists recent audit records, subscribes to the live audit feed,
and loads audit record details.

`/attachments/<id>` is reserved for token-protected attachment downloads. Until
the durable attachment store is wired in, authorized requests return HTTP 501
and unauthorized requests return HTTP 401.

## Limitations

All connected RPC clients currently receive broadcast notifications. Browser
chat filters messages by session id on the client side.

The virtual RPC chat driver supports replies, streamed reply edits, audio/file
fallback text, and mentions. It does not support deleting messages, fetching old
message content, member lookups, avatars, group member listing, or member title
changes.
