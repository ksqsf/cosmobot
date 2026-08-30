import { z } from 'zod'
import type { ActiveThread, AuditRecord, ChatAttachment, ChatLogSummary, ChatLogWindow, ChatMessage, ChatSession, MediaDetail, MediaGcResult, MediaItem, MediaSnapshot, Plugin, Resource, StoredThreadMessage, Task, ThreadDetail, ThreadMessageKey, ThreadRunTarget, ThreadSnapshot, ThreadSummary, TokenUsage, ToolCallTrace } from '@/types/domain'

export const taskSchema = z.object({
  id: z.number().int().positive(),
  label: z.string(),
  status: z.enum(['running', 'completed', 'failed', 'cancelled']),
  error: z.string().nullable(),
  startedAt: z.string(),
  finishedAt: z.string().nullable(),
}) satisfies z.ZodType<Task>
export const concurrencyListSchema = z.object({
  entries: z.array(taskSchema),
})
export const concurrencyLookupSchema = z.object({ entry: taskSchema.nullable() })
export const concurrencyCancelSchema = z.object({ id: z.number().int().positive(), cancelled: z.boolean() })

const toolCallTraceSchema = z.object({
  id: z.string(),
  name: z.string(),
  arguments: z.string(),
}) satisfies z.ZodType<ToolCallTrace>
const tokenUsageSchema = z.object({
  prompt_tokens: z.number().int().nonnegative(),
  completion_tokens: z.number().int().nonnegative(),
  total_tokens: z.number().int().nonnegative(),
  prompt_tokens_details: z.object({ cached_tokens: z.number().int().nonnegative() }).optional(),
}) satisfies z.ZodType<TokenUsage>
export const threadMessageKeySchema = z.object({
  platform: z.enum(['PlatformQQ', 'PlatformTelegram', 'PlatformMatrix', 'PlatformDiscord', 'PlatformRPC', 'PlatformACP']),
  chatId: z.string().regex(/^-?\d+$/).nullable(),
  messageId: z.string(),
}) satisfies z.ZodType<ThreadMessageKey>
const threadSummarySchema = z.object({
  threadId: z.number().int().positive(),
  latestPreview: z.string(),
  rootKey: threadMessageKeySchema,
  latestKey: threadMessageKeySchema,
  chatDisplayName: z.string().nullable(),
  nodeCount: z.number().int().positive(),
  leafCount: z.number().int().nonnegative(),
}) satisfies z.ZodType<ThreadSummary>
const storedThreadContentPartSchema = z.object({
  type: z.string(),
  text: z.string().optional(),
  image_url: z.union([z.string(), z.object({ url: z.string() })]).optional(),
}).loose()
const storedThreadMessageSchema = z.object({
  role: z.string(),
  content: z.union([z.string(), z.array(storedThreadContentPartSchema)]).nullable().optional(),
  tool_calls: z.array(z.object({
    id: z.string(),
    type: z.string(),
    function: z.object({ name: z.string(), arguments: z.string() }),
  })).optional(),
  tool_call_id: z.string().nullable().optional(),
}) satisfies z.ZodType<StoredThreadMessage>
export const threadListSchema = z.object({
  threads: z.array(threadSummarySchema),
  total: z.number().int().nonnegative(),
  nodes: z.number().int().nonnegative(),
  leaves: z.number().int().nonnegative(),
  platforms: z.number().int().nonnegative(),
}) satisfies z.ZodType<ThreadSnapshot>
export const threadDetailSchema = z.object({
  summary: threadSummarySchema,
  nodes: z.array(z.object({
    messageKey: threadMessageKeySchema,
    inputMessageKey: threadMessageKeySchema.nullable(),
    parentMessageKey: threadMessageKeySchema.nullable(),
    messages: z.array(storedThreadMessageSchema),
  })),
}) satisfies z.ZodType<ThreadDetail>
const activeThreadSchema = z.object({
  taskId: z.number().int().positive(),
  runId: z.string(),
  prompt: z.string(),
  parentMessageKey: threadMessageKeySchema.nullable(),
  parentThreadId: z.number().int().positive().nullable(),
  messageKeys: z.array(threadMessageKeySchema),
  pendingSteers: z.number().int().nonnegative(),
  messages: z.array(storedThreadMessageSchema),
}) satisfies z.ZodType<ActiveThread>
export const activeThreadListSchema = z.object({ threads: z.array(activeThreadSchema) })
export const threadRunTargetSchema = z.object({
  threadId: z.number().int().positive().nullable(),
  taskId: z.number().int().positive().nullable(),
}) satisfies z.ZodType<ThreadRunTarget>
export const haltThreadSchema = z.object({ taskId: z.number().int().positive(), halted: z.boolean() })

export const auditRecordSchema = z.object({
  id: z.number().int(),
  occurredAt: z.string(),
  event: z.discriminatedUnion('tag', [
    z.object({ tag: z.literal('AgentRunStarted'), runId: z.string(), messageId: z.string().nullable(), maxTurns: z.number().int(), exposedTools: z.array(z.string()), contextStrategy: z.string().nullable() }),
    z.object({ tag: z.literal('ModelTurnStarted'), runId: z.string(), turn: z.number().int(), messageCount: z.number().int(), exposedTools: z.array(z.string()), toolGroups: z.array(z.tuple([z.string(), z.number().int()])).nullable() }),
    z.object({ tag: z.literal('ModelTurnFinished'), runId: z.string(), turn: z.number().int(), answerKind: z.string(), contentLength: z.number().int(), toolCalls: z.array(toolCallTraceSchema), tokenUsage: tokenUsageSchema.nullable() }),
    z.object({ tag: z.literal('ContextCompacted'), runId: z.string(), turn: z.number().int(), messageCount: z.number().int(), tokenUsage: tokenUsageSchema.nullable() }),
    z.object({ tag: z.literal('RecursiveTranscriptFlushed'), runId: z.string(), turn: z.number().int() }),
    z.object({ tag: z.literal('SubAgentRunStarted'), runId: z.string(), childRunId: z.string(), subagentId: z.string() }),
    z.object({ tag: z.literal('ToolCallStarted'), runId: z.string(), turn: z.number().int(), toolCall: toolCallTraceSchema }),
    z.object({ tag: z.literal('ToolCallFinished'), runId: z.string(), turn: z.number().int(), toolCallId: z.string(), toolName: z.string(), status: z.string(), result: z.string(), resultLength: z.number().int(), messageIds: z.array(z.string().nullable()) }),
    z.object({ tag: z.literal('AgentRunFinished'), runId: z.string(), status: z.string(), finalLength: z.number().int(), turnsUsed: z.number().int() }),
    z.object({ tag: z.literal('AgentRunInterrupted'), runId: z.string(), reason: z.string() }),
    z.object({ tag: z.literal('AgentThreadLinked'), runId: z.string(), linkedMessageId: z.string(), linkedMessageKey: threadMessageKeySchema.nullable(), parentMessageId: z.string().nullable() }),
  ]),
}) satisfies z.ZodType<AuditRecord>

export const recentAuditSchema = z.array(auditRecordSchema)
export const auditCountSchema = z.number().int().nonnegative()
export const auditDetailSchema = auditRecordSchema.nullable()
export const auditThreadSchema = z.array(auditRecordSchema)
export const chatSessionSchema = z.object({
  sessionId: z.string().min(1),
  label: z.string().nullable(),
  parentSessionId: z.string().nullable(),
  parentMessageId: z.string().nullable(),
}) satisfies z.ZodType<ChatSession>
export const chatAttachmentSchema = z.object({
  attachmentId: z.string().min(1),
  name: z.string().min(1),
  mediaType: z.string().min(1),
  kind: z.enum(['image', 'audio', 'file']),
  size: z.number().int().nonnegative(),
  url: z.string().min(1),
}) satisfies z.ZodType<ChatAttachment>
export const chatMessageSchema = z.object({
  sessionId: z.string().min(1),
  messageId: z.string().min(1),
  sender: z.enum(['user', 'assistant']),
  text: z.string(),
  imageUrls: z.array(z.string()),
  attachments: z.array(chatAttachmentSchema),
  replyToMessageId: z.string().nullable(),
  parentMessageId: z.string().nullable(),
}) satisfies z.ZodType<ChatMessage>
export const chatSessionsSchema = z.object({ sessions: z.array(chatSessionSchema) })
export const chatHistorySchema = z.object({ sessionId: z.string(), messages: z.array(chatMessageSchema), hasOlder: z.boolean() })
export const chatOpenSchema = z.object({ sessionId: z.string(), session: chatSessionSchema })
export const chatRenameSchema = z.object({ session: chatSessionSchema })
export const chatDeleteSchema = z.object({ sessionId: z.string(), deleted: z.boolean() })
export const chatSendSchema = z.object({ sessionId: z.string(), messageId: z.string() })
export const chatMessageDoneSchema = z.object({ sessionId: z.string(), messageId: z.string() })
export const chatUploadSchema = chatAttachmentSchema.extend({ mediaRef: z.string(), fileId: z.string() })
const chatLogScopeSchema = z.object({
  platform: z.enum(['PlatformQQ', 'PlatformTelegram', 'PlatformMatrix', 'PlatformDiscord', 'PlatformRPC', 'PlatformACP']),
  kind: z.union([z.enum(['ChatPrivate', 'ChatGroup', 'ChatChannel']), z.templateLiteral(['ChatUnknown:', z.string()])]),
  chatId: z.string().regex(/^-?\d+$/).nullable(),
})
const chatLogEntrySchema = chatLogScopeSchema.extend({
  recordedAt: z.string().nullable(), senderId: z.string().nullable(), senderUsername: z.string().nullable(), senderDisplayName: z.string().nullable(),
  messageId: z.string().nullable(), replyToMessageId: z.string().nullable(), isBot: z.boolean(),
  mentions: z.array(z.string()), mentionUsernames: z.array(z.string()), imageUrls: z.array(z.string()),
  files: z.array(z.object({ name: z.string(), ref: z.string() })), text: z.string(),
})
export const chatLogListSchema = z.object({
  chats: z.array(z.object({
    scope: chatLogScopeSchema,
    chatDisplayName: z.string().nullable(),
    messageCount: z.number().int().nonnegative(),
    latestAt: z.string().nullable(),
  }) satisfies z.ZodType<ChatLogSummary>),
})
export const chatLogWindowSchema = z.object({
  scope: chatLogScopeSchema,
  chatDisplayName: z.string().nullable(),
  entries: z.array(z.object({ rowId: z.number().int().positive(), entry: chatLogEntrySchema, threadId: z.number().int().positive().nullable() })),
  hasOlder: z.boolean(), hasNewer: z.boolean(), anchorFound: z.boolean(), anchorMessageId: z.string().nullable(),
}) satisfies z.ZodType<ChatLogWindow>
export const mediaDeleteSchema = z.object({ fileId: z.string(), mediaId: z.string(), deleted: z.boolean() })
const mediaPlatformRefSchema = z.object({ platform: z.string(), scope: z.string(), platformRef: z.string() })
const mediaSourceKindSchema = z.enum(['chat', 'generated-image', 'tool-result', 'sandbox'])
export const mediaItemSchema = z.object({
  mediaId: z.string(),
  fileId: z.string(),
  digest: z.string(),
  mimeType: z.string(),
  sourceName: z.string().nullable(),
  size: z.number().int().nonnegative(),
  createdAtUnix: z.number().int(),
  lastUsedAtUnix: z.number().int(),
  exists: z.boolean(),
  sourceRefs: z.array(z.string()),
  platformRefs: z.array(mediaPlatformRefSchema),
  platforms: z.array(z.string()),
  sourceKinds: z.array(mediaSourceKindSchema),
}) satisfies z.ZodType<MediaItem>
const mediaStatsSchema = z.object({
  files: z.number().int().nonnegative(),
  existingFiles: z.number().int().nonnegative(),
  missingFiles: z.number().int().nonnegative(),
  totalBytes: z.number().int().nonnegative(),
  sources: z.number().int().nonnegative(),
  platformRefs: z.number().int().nonnegative(),
  platformAssociations: z.number().int().nonnegative(),
  mimeTypes: z.array(z.string()),
  platforms: z.array(z.string()),
})
const mediaGcSettingsSchema = z.object({
  enabled: z.boolean(),
  maxAgeSeconds: z.number().int().nonnegative(),
  intervalHours: z.number().int().nonnegative(),
})
export const mediaSnapshotSchema = z.object({
  stats: mediaStatsSchema,
  files: z.array(mediaItemSchema),
  gcSettings: mediaGcSettingsSchema,
}) satisfies z.ZodType<MediaSnapshot>
export const mediaSearchSchema = z.object({ files: z.array(mediaItemSchema) })
const mediaFileDetailSchema = z.object({
  fileId: z.string(), ref: z.string(), digest: z.string(), mimeType: z.string(), sourceName: z.string().nullable(),
  size: z.number().int().nonnegative(), createdAtUnix: z.number().int(), lastUsedAtUnix: z.number().int(), exists: z.boolean(),
})
export const mediaDetailSchema = z.object({
  mediaId: z.string(),
  fileId: z.string(),
  file: mediaFileDetailSchema,
  sourceRefs: z.array(z.string()),
  platformRefs: z.array(mediaPlatformRefSchema),
  platforms: z.array(z.string()),
  sourceKinds: z.array(mediaSourceKindSchema),
  publicUrl: z.string(),
}).transform(({ mediaId, fileId, file, sourceRefs, platformRefs, platforms, sourceKinds, publicUrl }): MediaDetail => ({
  mediaId, fileId, digest: file.digest, mimeType: file.mimeType, sourceName: file.sourceName,
  size: file.size, createdAtUnix: file.createdAtUnix, lastUsedAtUnix: file.lastUsedAtUnix,
  exists: file.exists, sourceRefs, platformRefs, platforms, sourceKinds, publicUrl,
}))
export const mediaGcSchema = z.object({
  deleted: z.number().int().nonnegative(),
  retainedReferencedFiles: z.number().int().nonnegative(),
  maxAgeSeconds: z.number().int().nonnegative(),
}) satisfies z.ZodType<MediaGcResult>
const resourceProbeSchema = z.discriminatedUnion('ok', [
  z.object({ ok: z.literal(true), result: z.string() }),
  z.object({ ok: z.literal(false), error: z.string() }),
])
export const resourceSchema = z.object({
  id: z.string().min(1),
  type: z.string(),
  sessionId: z.string().nullable(),
  description: z.string(),
  probe: resourceProbeSchema,
  remainingLifeMinutes: z.number().int().nullable(),
}) satisfies z.ZodType<Resource>
export const resourceListSchema = z.object({ resources: z.array(resourceSchema) })
export const resourceDetailSchema = z.object({ id: z.string(), detail: z.string() })
export const resourceDestroySchema = z.object({ id: z.string(), destroyed: z.literal(true) })
export const resourceRenameSchema = z.object({ id: z.string() })
export const resourceKeepAliveSchema = z.object({ id: z.string(), refreshed: z.literal(true) })
export const resourceMakePermanentSchema = z.object({ id: z.string(), permanent: z.literal(true) })
export const resourceListAssociatedSchema = z.object({
  id: z.number().int().positive(),
  resources: z.array(z.object({ id: z.string().min(1), type: z.string() })),
})
export const resourceDestroyAssociatedSchema = z.object({
  id: z.number().int().positive(),
  results: z.array(z.object({ ok: z.boolean(), code: z.string().optional(), error: z.string().optional() })),
})
export const pluginSchema = z.object({
  pluginId: z.string().min(1),
  version: z.string(),
  generation: z.number().int().positive(),
  required: z.boolean(),
  sandboxed: z.boolean(),
  routeCount: z.number().int().nonnegative(),
  toolCount: z.number().int().nonnegative(),
}).transform(({ pluginId: id, routeCount: routes, toolCount: tools, ...plugin }): Plugin => ({ id, routes, tools, ...plugin }))
export const pluginListSchema = z.object({ plugins: z.array(pluginSchema) })
export const pluginLifecycleSchema = pluginSchema
export const pluginUnloadSchema = z.object({ pluginId: z.string(), unloaded: z.literal(true) })

const memoryKeySchema = z.object({
  platform: z.enum(['qq', 'telegram', 'matrix', 'discord', 'rpc', 'acp']),
  scope: z.enum(['sender', 'chat']),
  scopeId: z.string(),
})
export const memorySummarySchema = memoryKeySchema.extend({ displayName: z.string().nullable(), username: z.string().nullable(), characters: z.number().int().nonnegative() })
export const memoryDetailSchema = memorySummarySchema.extend({ content: z.string() })
export const memoryListSchema = z.object({ memories: z.array(memorySummarySchema) })
export const memoryHistoryEntrySchema = z.object({ revision: z.string(), committedAt: z.string(), subject: z.string() })
export const memoryHistorySchema = z.object({ history: z.array(memoryHistoryEntrySchema) })
export const memoryRevertSchema = z.object({ reverted: z.literal(true), memory: memoryDetailSchema.nullable() })
export const skillSummarySchema = z.object({ name: z.string(), description: z.string().nullable() })
export const skillListSchema = z.object({ skills: z.array(skillSummarySchema) })
export const skillDetailSchema = z.object({ name: z.string(), content: z.string() })
export const skillRemoveSchema = z.object({ name: z.string(), removed: z.boolean() })
