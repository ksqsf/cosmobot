# Magic Numbers

This document records constants that are not configurable through `config.toml` but affect runtime policy. Protocol opcodes, HTTP/JSON-RPC status codes, cryptographic constants, unit conversions, and test data are excluded. For an inline literal without a binding, the owning function is named instead.

## Concurrency, Storage, and Caching

- `Bot.Concurrency.Manager.finishedTaskRetention` : Retains finished tasks for 12 hours, long enough to investigate recent failures without allowing task history to grow forever; active tasks are never pruned by age.
- `Bot.Util.Stream.streamQueueCapacity` : Buffers 1,024 events per merged input stream, balancing short bursts against bounded memory use.
- `Bot.Scheduler.Interpreter.scheduledMessageQueueCapacity` : Buffers 1,024 scheduled messages so brief consumer delays do not block the scheduler.
- `Bot.ACP.State.acpClientQueueCapacity` : Queues at most 256 outbound items per ACP client, bounding memory used by slow clients.
- `Bot.RPC.State.rpcClientQueueCapacity` : Queues at most 256 outbound items per RPC client, bounding memory used by slow clients.
- `Bot.HTTP.sharedManagerConnectionCount` : Allows 64 connections in the shared HTTP manager, enough for concurrent tools and drivers without excessive socket use.
- `Bot.Storage.SQLite.sqlitePoolSize` : Uses four SQLite connections because writes are serialized and a larger pool would add little useful concurrency.
- `Bot.Storage.SQLite.sqliteBusyTimeoutMilliseconds` : Waits up to five seconds for a busy SQLite database, allowing short transactions to finish without blocking indefinitely.
- `Bot.Storage.Thread.maxCachedThreads` : Caches at most four conversation-thread trees, limiting how many large transcripts remain resident.
- `Bot.Media.S3.publicObjects` : Holds 512 public object URLs in the LRU cache, reducing repeated signing and lookup without creating an unbounded cache.
- `Bot.Resource.reclaimExpired` : Checks for expired resources every 100 ms, making reclamation prompt without busy-waiting.
- `Cosmocode.Terminal.Brick.runTerminalIO` : Gives the TUI event channel a capacity of 256, absorbing short UI bursts while bounding its backlog.

## Retries, Timeouts, and Lifecycles

- `Bot.LLM.OpenAI.Retry.maxLLMRetries` : Retries an OpenAI-compatible request at most three times, covering transient failures without replaying it for too long.
- `Bot.LLM.OpenAI.Retry.retryDelaySeconds` : Uses `2^n` seconds as the retry-delay floor while honoring a larger `Retry-After`, preventing immediate retry pressure on the server.
- `Bot.Media.Interpreter.mediaNormalizeTimeoutMicroseconds` : Allows 15 seconds to normalize remote media, then preserves the original reference so message processing is not blocked.
- `Bot.Effect.ChatLog.chatLogRecordTimeoutMicroseconds` : Allows one second for a chat-log write because logging failure should not delay a chat reply.
- `Bot.Plugin.Manager.startBundle` : Allows ten seconds for plugin initialization RPC, giving the process time to start while detecting broken plugins promptly.
- `Bot.Plugin.Manager.stopRunning` : Allows five seconds for plugin shutdown before structured cleanup takes over.
- `Bot.Plugin.Manager.cleanupTransport` : Allows five seconds for plugin process exit before handles are closed.
- `Bot.Plugin.Manager.transportWriteTimeoutSeconds` : Allows five seconds for a plugin transport write so blocked stdin cannot stall the manager.
- `Bot.Plugin.Manager.processExitDrainMicroseconds` : Drains trailing output for 100 ms after plugin exit, preserving small amounts of buffered data.
- `Bot.Plugin.Manager.startAndPublish` : Treats three exits within 60 seconds as a startup crash loop, preventing endless rapid restarts.
- `Bot.Plugin.Manager.restartTransientProcess` : Caps runtime plugin restart backoff at four seconds and stops after three recent exits, slowing repeated failures while retaining quick recovery.
- `Bot.Resource.Sandbox.commandExitGraceSeconds` : Gives sandbox commands a ten-second exit grace period so normal cleanup can precede forced termination.
- `Bot.Resource.Python.cleanupGraceSeconds` : Gives Python workers five seconds to complete protocol shutdown and process cleanup.
- `Bot.Resource.Python.orphanMarginSeconds` : Adds a ten-second margin before orphan cleanup so workers finishing near their deadline are not killed prematurely.
- `Bot.Resource.Python.Sandbox.healthCheck` : Allows five seconds for each sandbox health check, failing promptly when an external process is unhealthy.
- `Bot.Agent.Tools.Shell.runBashTool` : Runs shell commands for 30 seconds by default, covering ordinary tool work without allowing unbounded execution.
- `Bot.Agent.Tools.Shell.parseCommandCall` : Waits ten seconds by default for a background command, balancing interaction latency against its chance of completing.
- `Bot.Agent.Tools.Shell.processExitGraceMicroseconds` : Gives a timed-out shell process five seconds to exit, followed by at most ten seconds for final termination.
- `Bot.Agent.Tools.Emacs.emacsEvalTool` : Allows ten seconds by default for Emacs evaluation or startup, preventing ordinary Lisp operations from occupying a tool call indefinitely.
- `Bot.Agent.Tools.Emacs.validTimeout` : Caps the Emacs tool timeout at 60 seconds so callers cannot create excessively long blocking operations.
- `Bot.Handler.Safebooru.safebooruOptions` : Allows 15 seconds for a Safebooru request so a slow external site does not block the handler.
- `Bot.Agent.Tools.Web.webRequestOptions` : Allows 15 seconds for a web-tool HTTP request, bounding external fetch latency.
- `Bot.Agent.Middleware.Typing.typingNotificationTimeoutMillis` : Gives typing notifications a 30-second lifetime, covering long model turns while still expiring automatically.
- `Bot.Agent.Middleware.Typing.typingNotificationRefreshMicroseconds` : Refreshes typing notifications every 20 seconds, before their 30-second expiry without making excessive API calls.

## Chat Drivers

- `Bot.Chat.Driver.Discord.discordReconnectDelayMicroseconds` : Waits five seconds before reconnecting the Discord Gateway, preventing a tight reconnect loop.
- `Bot.Chat.Driver.QQ.qqActionTimeoutMicroseconds` : Allows 40 seconds for a OneBot action, accommodating media operations while reclaiming lost requests.
- `Bot.Chat.Driver.QQ.qqReconnectDelayMicroseconds` : Waits five seconds before reconnecting QQ, preventing rapid retries against the server.
- `Bot.Chat.Driver.QQ.qqConnectionCloseTimeoutMicroseconds` : Allows two seconds to close an old QQ connection so replacement cannot remain blocked.
- `Bot.Chat.Driver.QQ.qqConnectionThreadStopTimeoutMicroseconds` : Allows two seconds for a QQ connection worker to stop before replacement continues.
- `Bot.Chat.Driver.QQ.qqHeartbeatCheckMicroseconds` : Checks QQ heartbeats every 15 seconds, detecting disconnects promptly without excessive polling.
- `Bot.Chat.Driver.QQ.qqHeartbeatTimeout` : Treats 90 seconds without a QQ heartbeat as a disconnect, tolerating several transient delays.
- `Bot.Chat.Driver.Telegram.telegramPollingRetryDelayMicroseconds` : Waits five seconds after Telegram polling fails, preventing immediate retries.
- `Bot.Chat.Driver.Telegram.telegramLongPollTimeoutSeconds` : Uses a 30-second Telegram server-side long-poll timeout to reduce requests at low traffic.
- `Bot.Chat.Driver.Telegram.telegramLongPollResponseTimeoutMicroseconds` : Adds ten seconds of network grace to the 30-second Telegram long poll.
- `Bot.Chat.Driver.Telegram.telegramApiResponseTimeoutMicroseconds` : Allows ten seconds for non-polling Telegram API requests.
- `Bot.Chat.Driver.Matrix.matrixRefreshMarginMilliseconds` : Refreshes Matrix tokens 60 seconds early so they do not expire during a request.
- `Bot.Chat.Driver.Matrix.matrixLongPollSyncTimeoutMilliseconds` : Uses a 30-second server-side timeout for Matrix long polling.
- `Bot.Chat.Driver.Matrix.matrixSyncResponseTimeoutMicroseconds` : Uses a 40-second Matrix sync client deadline, including ten seconds of transport grace.
- `Bot.Chat.Driver.Matrix.matrixApiResponseTimeoutMicroseconds` : Allows ten seconds for ordinary Matrix API requests.
- `Bot.Chat.Driver.Matrix.matrixMediaDownloadResponseTimeoutMicroseconds` : Allows 60 seconds for Matrix media downloads to accommodate larger files.
- `Bot.Chat.Driver.Matrix.matrixReloginAttempts` : Attempts Matrix credential recovery at most 12 times, covering prolonged transient failure while still terminating eventually.
- `Bot.Chat.Driver.Matrix.matrixReloginDelaySeconds` : Waits 180 seconds between Matrix relogin attempts to avoid hammering the authentication endpoint.
- `Bot.Chat.Driver.Matrix.matrixRetryDelayMicroseconds` : Waits five seconds after an ordinary Matrix sync failure.

## Frame, Output, and Memory Limits

- `Bot.Memory.memoryLimitChars` : Limits persistent memory for non-superusers to 1,000 characters, bounding prompt and disk growth.
- `Bot.RPC.Server.defaultUploadMaxBytes` : Sets the default RPC upload limit to 25 MiB, accommodating ordinary media without accepting huge requests.
- `Bot.Plugin.Protocol.maxFrameBytes` : Limits host-side plugin JSON lines to 1 MiB, bounding the framing buffer.
- `Cosmobot.Plugin.maxLineBytes` : Limits Haskell plugin SDK JSON lines to 1 MiB, matching the host wire contract.
- `cosmobot_plugin.plugin.MAX_LINE_BYTES` : Limits Python plugin SDK JSON lines to 1 MiB, matching the host wire contract.
- `Bot.Resource.Python.Protocol.maxRpcBytes` : Limits Python worker RPC frames to 4 MiB, accommodating tool payloads while bounding individual allocations.
- `Bot.Resource.Python.Protocol.maxControlBytes` : Limits Python worker control frames to 8 KiB because control messages should remain small.
- `Bot.Resource.Python.Protocol.maxCompletedBytes` : Limits completed Python results to 1 MiB so returned data cannot overwhelm agent context.
- `cosmobot_worker.MAX_STDOUT_BYTES` : Retains at most 1 MiB of Python worker stdout and truncates the remainder.
- `cosmobot_worker.MAX_STDERR_BYTES` : Retains at most 64 KiB of Python worker stderr because diagnostics usually need less space.
- `Bot.Resource.Sandbox.defaultOutputByteLimit` : Retains 1 MiB of generic sandbox output by default, enough for diagnostics without unbounded accumulation.
- `Bot.Resource.Sandbox.podmanRunArgs` : Limits a generic sandbox to 1 GiB of memory, one CPU, and 256 PIDs for basic resource isolation.
- `Bot.Resource.Python.Sandbox.bwrapArguments` : Limits the Python sandbox `/work` tmpfs to 64 MiB, bounding temporary-file use.
- `Bot.Resource.Python.Sandbox.start` : Limits the Python sandbox to 64 open file descriptors, constraining resource abuse.
- `Bot.Resource.Python.Sandbox.readChildPid` : Limits the bubblewrap info payload to 4 KiB because this control data should remain small.
- `Bot.Agent.Tools.Shell.defaultOutputByteLimit` : Retains 1 MiB of shell output when the caller gives no limit, matching the sandbox default.
- `Bot.Agent.Tools.Media.defaultReadSize` : Reads 4,096 characters of media text by default, controlling ordinary tool-result size.
- `Bot.Agent.Tools.Media.maxReadSize` : Reads at most 16,384 media-text characters per call; larger content should be paged.
- `Bot.Agent.Middleware.ToolResultCompaction.maxToolResultPreviewChars` : Retains 4,096 characters in durable and later-turn tool-result previews.
- `Bot.Agent.Middleware.ToolResultCompaction.maxImmediateToolResultChars` : Uses media-backed compaction when the immediate model view exceeds 10,000 characters.
- `Bot.Log.base64LogPrefixChars` : Logs only the first 96 characters of base64 data URLs, enough for identification without emitting the payload.
- `Bot.LLM.Types.llmExceptionSummary` : Shows at most 500 characters of exceptions and decoding failures so errors do not flood logs or chat.
- `Bot.LLM.Types.httpBodySummary` : Shows at most 4,000 characters of an LLM error response, retaining diagnostics while limiting exposure.
- `Bot.Resource.conciseException` : Shows at most 500 characters of resource failure detail, keeping RPC and tool errors compact.

## Agent and Tool Policy

- `Bot.Agent.Middleware.ContextCompaction.recentMessageWindow` : Keeps the 20 most recent messages during context compaction, balancing continuity against compression.
- `Bot.Agent.Tools.Transcript.maxRecursiveDepth` : Limits recursive transcript analysis to three levels, preventing recursive explosion.
- `Bot.Agent.Tools.Transcript.maxRecursiveQueries` : Limits recursive transcript analysis to 32 queries, bounding total work.
- `Bot.Agent.Tools.Transcript.recursiveQuery` : Limits recursive transcript child agents to six turns and enables context compaction at 1,000,000 characters so analysis remains bounded.
- `Bot.Resource.SubAgent.sendPrompt` : Limits managed subagents to eight turns and enables context compaction at 1,000,000 characters, allowing slightly longer but still bounded workflows.
- `Bot.Agent.Tools.Transcript.parseCall` : Reads at most 200 transcript messages per call and defaults to 20.
- `Bot.Agent.Tools.Transcript.maxMatches` : Returns at most 100 transcript-search matches per call and defaults to 20.
- `Bot.Agent.Tools.Transcript.queryTranscript` : Limits a transcript query range to 200,000 characters; larger ranges should be split.
- `Bot.Agent.Tools.Transcript.searchTranscript` : Retains 1,000 characters per transcript-search snippet, providing context without returning the full message.
- `Bot.Agent.Middleware.Python.maxNestedBatchCalls` : Accepts at most 16 nested tool calls per Python middleware batch, bounding fan-out.
- `Bot.Agent.Middleware.Python.maxNestedToolCallIdChars` : Limits nested tool-call IDs to 256 characters, bounding protocol metadata.
- `Bot.Agent.Types.maxPythonWallTimeoutSeconds` : Caps configured Python wall timeouts at one hour, preventing nearly permanent worker calls.
- `Bot.Agent.Tools.Chat.validSenderLogLimit` : Returns at most 100 messages per chat search, bounding SQL and context costs.
- `Bot.Agent.Tools.Web.webSearchTool` : Lets callers request at most 20 web-search results, preventing provider and context overload.
- `Bot.Agent.Tools.Web.webFetchTool` : Lets callers request at most about 200,000 web-fetch tokens, preventing unbounded page input.
- `Bot.Agent.Tools.Web.exaSearch` : Requests 1,000-character provider highlights, retaining relevant passages rather than whole pages.
- `Bot.Resource.Types.ttlFromMinutes` : Requires a managed-resource TTL of at least five minutes, avoiding rapid creation and immediate reclamation.
- `Bot.Resource.validateResourceName` : Limits resource identifiers to 64 characters, keeping storage keys, tool output, and shell paths manageable.
- `Bot.Handler.Safebooru.maxRequestedImages` : Allows at most five images per Safebooru command, limiting external requests and platform flooding.
- `Bot.Handler.Safebooru.storedImageLimit` : Retains the 20 most recent Safebooru images per scope, supporting deduplication with bounded state.
- `Bot.Handler.Audit.recentAuditLimit` : Shows 20 recent audit entries by default, keeping chat output readable.
- `Bot.RPC.Audit.parseLimit` : Returns 20 recent audit entries by default, avoiding a large query when no limit is supplied.
- `Bot.RPC.Server.parseMediaStatsParams` : Returns 50 media-stat rows by default, balancing overview detail against payload size.

## Chat Display Limits

- `Bot.Chat.Driver.defaultChunkedMessageLimit` : Limits generic outbound-message chunks to 4,000 characters as a safe default when a platform has no specialized value.
- `Bot.Chat.Driver.RPC.editChunkChars` : Updates an editable RPC reply every 1,200 accumulated characters, reducing edit frequency.
- `Bot.Chat.Driver.RPC.messageOutPolicy` : Limits RPC reply chunks to 4,000 characters, controlling individual event payloads.
- `Bot.Chat.Driver.ACP.editChunkChars` : Updates an editable ACP reply every 1,200 accumulated characters, reducing update-notification frequency.
- `Bot.Chat.Driver.ACP.messageOutPolicy` : Limits ACP reply chunks to 4,000 characters, controlling client-update payloads.
- `Bot.Chat.Driver.Discord.discordEditChunkChars` : Updates a streaming Discord reply every 512 characters, balancing responsiveness against rate limits.
- `Bot.Chat.Driver.Discord.discordMessageTextLimit` : Limits Discord message chunks to 2,000 characters, matching the platform limit.
- `Bot.Chat.Driver.Telegram.telegramEditChunkChars` : Updates a streaming Telegram reply every 512 characters, balancing responsiveness against edit count.
- `Bot.Chat.Driver.Telegram.telegramMessageTextLimit` : Limits chunks on the new Telegram rich-message path to 30,000 characters, preventing oversized payloads.
- `Bot.Chat.Driver.Telegram.telegramLegacyMessageTextLimit` : Limits legacy Telegram messages to 4,096 characters, matching the platform limit.
- `Bot.Chat.Driver.Matrix.matrixEditChunkChars` : Updates a streaming Matrix reply every 128 characters for finer-grained live output.
- `Bot.Chat.Driver.Matrix.matrixStreamingMessageLimit` : Limits streaming Matrix messages to 4,000 characters, preventing oversized events.
- `Bot.Chat.Driver.QQ.qqForwardMessageThreshold` : Switches QQ text over 1,000 characters to a forward message for better long-form display and transport stability.
- `Bot.Chat.Driver.QQ.qqForwardNodeLimit` : Limits QQ forward messages to 2,000 nodes, preventing oversized forwarding structures.
- `Bot.Handler.Admin.scriptOutput` : Shows at most 3,000 characters per startup-action body in admin output, preventing oversized management replies.
- `Bot.Handler.Ask.renderActiveThreads` : Shows 20 characters of each prompt in active-thread listings so multiple threads remain easy to scan.
- `Cosmocode.Terminal.Brick.drawUi` : Displays five TUI editor rows, leaving most of the screen for the transcript.
