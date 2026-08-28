import { z } from 'zod'
import type { AuditRecord, ChatAttachment, ChatMessage, ChatSession, Plugin, Resource, Task, ThreadMessageKey, TokenUsage, ToolCallTrace } from '@/types/domain'

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
export const concurrencyAwaitSchema = z.object({ id: z.number().int().positive(), awaited: z.literal(true) })

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
export const chatHistorySchema = z.object({ sessionId: z.string(), messages: z.array(chatMessageSchema) })
export const chatOpenSchema = z.object({ sessionId: z.string(), session: chatSessionSchema })
export const chatRenameSchema = z.object({ session: chatSessionSchema })
export const chatDeleteSchema = z.object({ sessionId: z.string(), deleted: z.boolean() })
export const chatSendSchema = z.object({ sessionId: z.string(), messageId: z.string() })
export const chatMessageDoneSchema = z.object({ sessionId: z.string(), messageId: z.string() })
export const chatUploadSchema = chatAttachmentSchema.extend({ mediaRef: z.string(), fileId: z.string() })
export const mediaDeleteSchema = z.object({ fileId: z.string(), mediaId: z.string(), deleted: z.boolean() })
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
