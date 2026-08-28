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
allowed_browser_origins = []
```

`enabled` defaults to `false`. When `enabled = true`, `token` must be non-empty.
The default host is loopback-only. Cosmobot serves the WebSocket RPC endpoint at
`/rpc`. Uploaded media is stored in the shared media cache; RPC responses return
the public URL produced by the media interpreter.

## Authentication

Clients authenticate with `Authorization: Bearer TOKEN` during the WebSocket
handshake. Query-string tokens are not accepted for WebSocket authentication.
Unauthorized WebSocket connections are rejected with HTTP 401.

Browser clients cannot set the `Authorization` header. An origin listed exactly
in `rpc.allowed_browser_origins` may connect and must send
`admin.authenticate` with `{"token":"…"}` as its first request within ten
seconds. The connection is not registered and no other method is dispatched
until authentication succeeds. Authentication gets one attempt per connection;
the token remains valid until the RPC configuration changes or the server
restarts.

`admin.capabilities` returns the server version, supported methods and topics,
permissions, and feature availability. Cosmoscope uses it to select live or
explicitly labelled demo data per page.

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

Responses and notifications may be interleaved on one WebSocket connection.
Clients must route responses by `id` instead of assuming that the next frame is
the response to the most recent request.

Inbound WebSocket frames and assembled messages are limited to 35,018,072
bytes. Connections that exceed either limit are closed before JSON decoding.

Standard JSON-RPC numeric error codes are used. Cosmobot's stable textual error
code is preserved in `error.data.code` where applicable.

## Event subscriptions

New connections receive no broadcast notifications until they subscribe. Three
topic kinds are available internally: one chat session, the global non-audit
event stream, and global audit events. Direct JSON-RPC responses are always sent
only to the requesting connection.

`events.subscribe` subscribes the connection to the global non-audit stream,
including chat and agent-lifecycle notifications for every RPC session:

```json
{"jsonrpc":"2.0","id":"1","method":"events.subscribe","params":{}}
```

`events.unsubscribe` removes that subscription. Both methods return
`{"subscribed":true}` or `{"unsubscribed":true}`, respectively. Audit events
remain independently selectable through `audit.subscribe`; subscribing to both
streams provides global observation without duplicate audit notifications.

## Audit Methods

### `audit.recent`

Returns recent audit records. `limit` is optional, defaults to `20`, and must
be between `1` and `500`.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.recent","params":{"limit":50}}
```

### `audit.get`

Returns one audit record by audit id. The preferred parameter is `audit_id`;
`id` is also accepted.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.get","params":{"audit_id":123}}
```

### `audit.thread`

Returns audit records associated with one platform/chat-scoped message id.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.thread","params":{"platform":"discord","chat_id":"1152921504606846976","message_id":"7"}}
```

### `audit.thread_messages`

Returns audit records associated with multiple message ids in one platform/chat.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.thread_messages","params":{"platform":"discord","chat_id":"1152921504606846976","message_ids":["m1","m2"]}}
```

Use a decimal string for `chat_id`; legacy numeric input remains accepted.
`AgentThreadLinked.linkedMessageKey.chatId` is likewise returned as a decimal
string so identifiers larger than JavaScript's safe integer range remain exact.

### `audit.subscribe`

Subscribes this connection to live updates for all persisted agent audit
records. Other connected clients do not receive those updates unless they also
subscribe.

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

### `audit.unsubscribe`

Removes the connection's audit subscription:

```json
{"jsonrpc":"2.0","id":"2","method":"audit.unsubscribe","params":{}}
```

Result:

```json
{"unsubscribed":true}
```

## Thread Methods

Thread methods inspect the platform conversation graph. They are separate from
RPC console sessions: a thread is keyed by platform, chat, and platform-native
message identity.

### `thread.list`

Returns a page of persisted thread summaries plus `total`, `nodes`, `leaves`,
and `platforms` totals computed over the complete filtered result. Each summary
includes the stable storage thread id, platform/chat-scoped root and latest keys,
a root text preview, node count, and branch-tip count. `offset` defaults to `0`
and `limit` defaults to `25` (maximum `200`). Optional `query` and `platform`
filters are applied before totals and pagination.

```json
{"jsonrpc":"2.0","id":"1","method":"thread.list","params":{"offset":0,"limit":25,"query":"hello","platform":"rpc"}}
```

### `thread.get`

Returns one persisted thread and its ordered nodes. Each node contains its
message key, optional parent key, and the stored transcript segment for that
node. Clients reconstruct context at a node by concatenating segments along its
parent path.

```json
{"jsonrpc":"2.0","id":"2","method":"thread.get","params":{"threadId":42}}
```

### `thread.active`

Returns in-memory agent threads that are currently running, including task and
run ids, prompt, linked message keys, pending steer count, and the current model
transcript. This is a snapshot method; clients may poll it for monitoring.

```json
{"jsonrpc":"2.0","id":"3","method":"thread.active","params":{}}
```

### `thread.halt`

Cancels one active thread through the structured Concurrency handle and persists
the transcript produced so far. It returns `halted: false` when that task is no
longer active.

```json
{"jsonrpc":"2.0","id":"4","method":"thread.halt","params":{"taskId":17}}
```

## Console lifecycle notifications

RPC console sessions emit lightweight, session-scoped notifications for an
interactive client. `chat.reasoning_start` and `chat.reasoning_end` delimit a model turn;
they do not contain private reasoning text. Tool results are likewise omitted.

```json
{"jsonrpc":"2.0","method":"chat.message_done","params":{"sessionId":"work-1","messageId":"message-2"}}
{"jsonrpc":"2.0","method":"chat.reasoning_start","params":{"sessionId":"work-1","runId":"run-1","turn":1}}
{"jsonrpc":"2.0","method":"chat.reasoning_end","params":{"sessionId":"work-1","runId":"run-1","turn":1,"answerKind":"tool_request"}}
{"jsonrpc":"2.0","method":"chat.tool_call_start","params":{"sessionId":"work-1","runId":"run-1","turn":1,"toolCallId":"call-1","toolName":"run_bash"}}
{"jsonrpc":"2.0","method":"chat.tool_call_end","params":{"sessionId":"work-1","runId":"run-1","turn":1,"toolCallId":"call-1","toolName":"run_bash","status":"ok"}}
```

`chat.message_done` commits the final streamed assistant text previously sent
by `chat.message` and `chat.message_update`.

## Manager Methods

Possession of the RPC bearer token grants superuser resource access. Resource
methods can therefore inspect or modify resources owned through any chat
platform.

### Concurrency

| Method | Parameters | Result |
|---|---|---|
| `concurrency.list` | none | `entries`: all known tasks |
| `concurrency.lookup` | `id` | `entry`: the task or `null` |
| `concurrency.cancel` | `id` | `id` and `cancelled` |
| `concurrency.await` | `id` | waits for completion, then returns `id` and `awaited` |

Task entries contain `id`, `label`, `status`, `error`, `startedAt`, and
`finishedAt`. `status` is one of `running`, `completed`, `failed`, or
`cancelled`; `error` is populated only for failed tasks. Running tasks are
always listed; finished task history is retained for 12 hours.

```json
{"jsonrpc":"2.0","id":"1","method":"concurrency.lookup","params":{"id":42}}
```

### Resources

| Method | Parameters | Result |
|---|---|---|
| `resource.list` | none | `resources`: all available resources |
| `resource.detail` | `id` | `id` and `detail` |
| `resource.destroy` | `id` | `id` and `destroyed` |
| `resource.rename` | `id`, `newId` | the renamed `id` |
| `resource.keep_alive` | `id` | `id` and `refreshed` |
| `resource.make_permanent` | `id` | `id` and `permanent` |
| `resource.list_associated` | concurrency `id` | associated resource `id` and `type` entries |
| `resource.destroy_associated` | concurrency `id` | per-resource cleanup `results` |

`resource.rename` also accepts `new_id`. Resource list entries contain `id`,
`type`, `sessionId`, `description`, `probe`, and `remainingLifeMinutes`.
`remainingLifeMinutes` is `null` for permanent resources. `probe` contains an
`ok` boolean and either `result` or `error`.

`resource.list_associated` returns the IDs and types of every resource that a
subsequent `resource.destroy_associated` call would target, including resources
hidden from the ordinary user-facing resource list.

```json
{"jsonrpc":"2.0","id":"2","method":"resource.keep_alive","params":{"id":"sandbox-1"}}
```

Resource failures use textual error codes such as `not_found`,
`invalid_params`, `already_exists`, `unavailable`, and `resource_error` in
`error.data.code`.

## Plugin Methods

Plugin lifecycle methods use the same manager as the chat commands. The list
contains loaded plugins only; installed but stopped bundles are not discoverable
through this RPC surface.

| Method | Parameters | Result |
|---|---|---|
| `plugin.list` | none | `plugins`: loaded plugin statuses |
| `plugin.load` | `pluginId` | the loaded plugin status |
| `plugin.reload` | `pluginId` | the new plugin status and generation |
| `plugin.unload` | `pluginId` | `pluginId` and `unloaded` |

A plugin status contains `pluginId`, `version`, `generation`, `required`,
`sandboxed`, `routeCount`, and `toolCount`. Plugin IDs use the same validated
identifier syntax as chat lifecycle commands. Required plugins cannot be
unloaded. Lifecycle failures use `plugin_operation_failed` and do not expose
server filesystem paths.

```json
{"jsonrpc":"2.0","id":"3","method":"plugin.reload","params":{"pluginId":"echo"}}
```

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

### `chat.subscribe`

Subscribes this connection to notifications for one existing chat session:

```json
{"jsonrpc":"2.0","id":"2","method":"chat.subscribe","params":{"sessionId":"local-1"}}
```

Result:

```json
{"subscribed":true}
```

`session_id` is accepted as an alias. A missing session returns `not_found`.
`chat.unsubscribe` accepts the same parameters and returns
`{"unsubscribed":true}`. Cosmocode subscribes automatically after opening or
resuming its active session.

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
    "attachments": []
  }
}
```

Accepted aliases:

- `sessionId` or `session_id`
- `imageUrls` or `image_urls`

The server resumes the durable transcript identified by `sessionId`. Ordinary
clients do not select a parent message; use `chat.fork` to branch a session.

Uploaded image attachments are also exposed to handlers as
`IncomingMessage.imageUrls`. Audio and file attachments remain in message
history as `attachments` and are summarized in the RPC message context passed to
handlers.

Result:

```json
{"sessionId":"local-1","messageId":"rpc-1"}
```

The sent user message is also published as a `chat.message` notification with
`sender: "user"` to subscribers of that session and the global event stream.

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
`size` when `size` is provided and must not exceed 25 MiB. Uploaded bytes are
stored in the shared media cache and the returned `attachmentId` is a
`media:<file_id>` reference.

Result:

```json
{"id":"media:mf_abc","attachmentId":"media:mf_abc","mediaRef":"media:mf_abc","fileId":"mf_abc","name":"notes.txt","mediaType":"text/plain","kind":"file","size":5,"url":"https://media.example.com/cosmobot-media/sha256.png"}
```

### `media.stats`

Returns media cache counts, configured GC policy, and a bounded newest-first
list. Each list entry includes a `platforms` provenance array, `sourceKinds`
tags (`chat`, `generated-image`, `tool-result`, or `sandbox`), source references, and separately cached
platform-native references. `platforms` may be empty and may contain more than
one platform. `limit` defaults to `50` and is capped at `500`.
The `stats.files` and `stats.totalBytes` values aggregate the complete cache;
they are not affected by the list `limit`. Stats also include the complete
distinct `platforms` and `mimeTypes` values used to populate filters.

```json
{"jsonrpc":"2.0","id":"3","method":"media.stats","params":{"limit":20}}
```

### `media.search`

Searches the complete media cache, applies every supplied condition, then
returns at most `limit` entries (default `200`, maximum `500`). Conditions are
combined with AND; values within `platforms`, `mimeTypes`, or `sourceKinds` are
combined with OR. `withoutPlatform` includes chat media with no platform.

```json
{"jsonrpc":"2.0","id":"4","method":"media.search","params":{"query":"report","platforms":["matrix","qq"],"withoutPlatform":false,"mimeTypes":["application/pdf"],"sourceKinds":["chat"],"limit":200}}
```

### `media.resolve_source`

Looks up a media cache entry by source id and returns its `media:<file_id>`
reference when known.

```json
{"jsonrpc":"2.0","id":"5","method":"media.resolve_source","params":{"sourceRef":"telegram:file-123"}}
```

Result:

```json
{"sourceRef":"telegram:file-123","mediaId":"media:mf_abc","fileId":"mf_abc"}
```

### `media.get`

Returns one cached media entry by `mediaId` or `fileId`, including source refs,
source-kind tags, platform refs, public URL, and local cache path.

```json
{"jsonrpc":"2.0","id":"5","method":"media.get","params":{"mediaId":"media:mf_abc"}}
```

### `media.delete`

Deletes one media id, its source-kind/platform/source-reference associations,
and its local cached file when the file is not shared by another media row.

```json
{"jsonrpc":"2.0","id":"6","method":"media.delete","params":{"mediaId":"media:mf_abc"}}
```

### `media.gc`

Runs media cache GC manually. Omitting `maxAgeSeconds` uses the server's
configured retention period; explicitly passing `0` forces collection of all
unreferenced objects regardless of age. Media file ids referenced by RPC chat
history are retained in either mode.

```json
{"jsonrpc":"2.0","id":"4","method":"media.gc","params":{"maxAgeSeconds":604800}}
```

## Chat Notifications

Bot replies are sent as `chat.message` notifications:

```json
{
  "method": "chat.message",
  "jsonrpc": "2.0",
  "params": {
    "sessionId": "local-1",
    "messageId": "message-2",
    "sender": "assistant",
    "text": "reply text",
    "imageUrls": [],
    "attachments": [],
    "replyToMessageId": "message-1",
    "parentMessageId": "message-1"
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
    "messageId": "message-2",
    "sender": "assistant",
    "text": "updated reply text",
    "imageUrls": [],
    "attachments": [],
    "replyToMessageId": "message-1",
    "parentMessageId": "message-1"
  }
}
```

Both notifications carry the complete stored chat-message shape. Cached
`media:...` image refs and files sent through the chat driver's upload operation
are published as attachment objects with dereferenceable public URLs; clients
must not treat a `media:...` identifier as a browser URL.

## CLI

The CLI reads `[rpc]` from `config.toml` and connects to the running daemon:

```sh
cosmobot rpc audit recent --limit 20
cosmobot rpc audit show 123
cosmobot rpc audit thread MESSAGE_ID
cosmobot rpc media stats --limit 20
cosmobot rpc media resolve-source SOURCE_REF
cosmobot rpc media get MEDIA_ID_OR_FILE_ID
cosmobot rpc media delete MEDIA_ID_OR_FILE_ID
cosmobot rpc media gc --max-age-seconds 604800
cosmobot rpc call METHOD JSON
```

Use `--host`, `--port`, or `--token` after `rpc` to override the `[rpc]`
settings from the config file:

```sh
cosmobot rpc --host 127.0.0.1 --port 38765 --token "$TOKEN" media stats
```

Responses are printed as pretty JSON.

Use `cosmobot rpc --config FILE ...` to read a config file other than
`config.toml`.

Uploaded media bytes are not served by the RPC HTTP app. Configure
`[media].public_base_url` and optional `[media.s3]` settings when RPC clients
need dereferenceable public URLs.

## Limitations

Audit subscriptions deliver new records only. Clients that need durable catch-up
must query the audit methods after connecting.

The virtual RPC chat driver supports replies, streamed reply edits, audio/file
fallback text, and mentions. It does not support deleting messages, fetching old
message content, member lookups, avatars, group member listing, or member title
changes.
