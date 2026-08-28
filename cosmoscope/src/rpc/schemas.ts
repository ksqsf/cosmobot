import { z } from 'zod'
import type { AuditRecord, ChatAttachment, ChatMessage, ChatSession } from '@/types/domain'

export const concurrencyListSchema = z.object({
  entries: z.array(z.object({
    id: z.number(),
    label: z.string(),
    status: z.enum(['running', 'completed', 'failed', 'cancelled']),
    error: z.string().nullable(),
    startedAt: z.string(),
    finishedAt: z.string().nullable(),
  })),
})

export const auditRecordSchema = z.object({
  id: z.number().int(),
  occurredAt: z.string(),
  event: z.discriminatedUnion('tag', [
    z.object({ tag: z.literal('AgentRunStarted'), runId: z.string() }),
    z.object({ tag: z.literal('ModelTurnStarted'), runId: z.string(), turn: z.number().int() }),
    z.object({ tag: z.literal('ModelTurnFinished'), runId: z.string(), turn: z.number().int(), answerKind: z.string() }),
    z.object({ tag: z.literal('ContextCompacted'), runId: z.string(), turn: z.number().int() }),
    z.object({ tag: z.literal('RecursiveTranscriptFlushed'), runId: z.string(), turn: z.number().int() }),
    z.object({ tag: z.literal('SubAgentRunStarted'), runId: z.string(), childRunId: z.string(), subagentId: z.string() }),
    z.object({ tag: z.literal('ToolCallStarted'), runId: z.string(), toolCall: z.object({ name: z.string() }) }),
    z.object({ tag: z.literal('ToolCallFinished'), runId: z.string(), toolName: z.string(), status: z.string() }),
    z.object({ tag: z.literal('AgentRunFinished'), runId: z.string(), status: z.string() }),
    z.object({ tag: z.literal('AgentRunInterrupted'), runId: z.string(), reason: z.string() }),
    z.object({ tag: z.literal('AgentThreadLinked'), runId: z.string() }),
  ]),
}) satisfies z.ZodType<AuditRecord>

export const recentAuditSchema = z.array(auditRecordSchema)
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
export const resourceListSchema = z.object({ resources: z.array(z.object({ id: z.string() }).loose()) })
