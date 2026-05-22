# Cosmobot RPC Protocol

Cosmobot can expose a local WebSocket RPC endpoint from `cosmobot serve`.
The RPC service is intended for local chat, audit inspection, and CLI queries
against the running daemon. The wire envelope is JSON-RPC 2.0 over WebSocket,
using the Haskell `jsonrpc` package for protocol types.

## Configuration

RPC is configured under `[rpc]` in `config.toml`:

```toml
[rpc]
enabled = false
host = "127.0.0.1"
port = 38765
token = ""
attachment_dir = "attachments"
attachment_max_bytes = 26214400
```

`enabled` defaults to `false`. When `enabled = true`, `token` must be non-empty.
The default host is loopback-only. Cosmobot serves the WebSocket RPC endpoint at
`/rpc` and authenticated attachment bytes under `/attachments/<id>`.

## Authentication

Clients authenticate with `Authorization: Bearer TOKEN` during the WebSocket
handshake. Query-string tokens are not accepted for WebSocket authentication.
HTTP attachment reads also use the bearer token header. Unauthorized WebSocket
connections or attachment requests are rejected with HTTP 401.

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
{"jsonrpc":"2.0","id":"1","method":"chat.open_session","params":{"label":"local"}}
```

Result:

```json
{"sessionId":"local-1"}
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
    "sessionId": "local-1",
    "text": "!ask hello",
    "imageUrls": [],
    "attachments": [],
    "replyToMessageId": null
  }
}
```

Accepted aliases:

- `sessionId` or `session_id`
- `imageUrls` or `image_urls`
- `replyToMessageId` or `reply_to_message_id`

Uploaded image attachments are also exposed to handlers as
`IncomingMessage.imageUrls`. Audio and file attachments remain in message
history as `attachments` and are summarized in the RPC message context passed to
handlers.

Result:

```json
{"sessionId":"local-1","messageId":"rpc-1"}
```

The sent user message is also broadcast as a `chat.message` notification with
`sender: "user"`.

Sending to a session id that does not exist fails with textual error code
`not_found`; no message is persisted or broadcast.

### `chat.fork`

Creates a new session whose history starts from a message in an existing
session. The fork stores `parentSessionId` and `parentMessageId` and reads parent
history immutably through that message.

### `chat.delete_session`

Deletes a session and its stored messages. If other sessions were forked from
the deleted session, deletion cascades to those descendant sessions and their
messages because forked sessions depend on parent history.

### `chat.upload_attachment`

Stores an RPC attachment before sending it in chat:

```json
{
  "id": "2",
  "jsonrpc": "2.0",
  "method": "chat.upload_attachment",
  "params": {
    "name": "notes.txt",
    "mediaType": "text/plain",
    "kind": "file",
    "size": 5,
    "data": "aGVsbG8="
  }
}
```

`data` is base64 without a data-URL prefix. The decoded byte length must match
`size` when `size` is provided and must not exceed `rpc.attachment_max_bytes`.

Result:

```json
{"id":"att-123","attachmentId":"att-123","name":"notes.txt","mediaType":"text/plain","kind":"file","size":5,"url":"/attachments/att-123"}
```

### `chat.delete_attachment`

Deletes an uploaded attachment only while it is still unreferenced by persisted
messages:

```json
{"jsonrpc":"2.0","id":"3","method":"chat.delete_attachment","params":{"attachmentId":"att-123"}}
```

Result:

```json
{"id":"att-123","attachmentId":"att-123","deleted":true}
```

## Chat Notifications

Bot replies are sent as `chat.message` notifications:

```json
{
  "method": "chat.message",
  "jsonrpc": "2.0",
  "params": {
    "sessionId": "local-1",
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
    "sessionId": "local-1",
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

`/attachments/<id>` serves uploaded attachment bytes when authorized by the same
bearer token used by RPC clients. Unauthorized requests return HTTP 401; missing
attachments return HTTP 404.

## Limitations

All connected RPC clients currently receive broadcast notifications. Clients
that only need one chat should filter messages by session id.

The virtual RPC chat driver supports replies, streamed reply edits, audio/file
fallback text, and mentions. It does not support deleting messages, fetching old
message content, member lookups, avatars, group member listing, or member title
changes.
