import { z } from 'zod'
import type { AuditRecord } from '@/types/domain'

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
export const chatSessionsSchema = z.object({ sessions: z.array(z.object({ sessionId: z.string() }).loose()) })
export const resourceListSchema = z.object({ resources: z.array(z.object({ id: z.string() }).loose()) })
