import { describe, expect, it } from 'vitest'
import { auditDetailFields, auditPresentation, boundedStructuredText, linkedThread, mergeAuditRecords, parseAuditSearch } from '@/backend/audit'
import { auditRecordSchema, threadRunTargetSchema } from '@/rpc/schemas'
import type { AuditRecord } from '@/types/domain'

const record = (id: number): AuditRecord => ({
  id,
  occurredAt: '2026-08-28T12:00:00Z',
  event: { tag: 'AgentRunInterrupted', runId: 'run-1', reason: 'cancelled' },
})

describe('audit timeline', () => {
  it('deduplicates durable IDs and bounds the loaded window', () => {
    const merged = mergeAuditRecords([record(1), record(2)], [record(2), record(3)], 2)
    expect(merged.map(({ id }) => id)).toEqual([2, 3])
  })

  it('retains structured tool payloads at the RPC boundary', () => {
    const parsed = auditRecordSchema.parse({
      id: 7,
      occurredAt: '2026-08-28T12:00:00Z',
      event: {
        tag: 'ToolCallFinished', runId: 'run-1', turn: 2, toolCallId: 'call-1', toolName: 'read_file',
        status: 'ok', result: '{"large":true}', resultLength: 14, messageIds: [null, 'message-2'],
      },
    })
    expect(parsed.event).toMatchObject({ result: '{"large":true}', messageIds: [null, 'message-2'] })
    expect(auditPresentation(parsed.event)).toMatchObject({ category: 'tool', tone: 'success' })
  })

  it('finds the latest durable thread correlation and caps rendered payloads', () => {
    const linkedMessageKey = { platform: 'PlatformMatrix', chatId: '!room:example.org', messageId: 'message-2' } as const
    const linked: AuditRecord = {
      id: 8,
      occurredAt: '2026-08-28T12:00:01Z',
      event: {
        tag: 'AgentThreadLinked', runId: 'run-1', linkedMessageId: 'message-2', parentMessageId: null,
        linkedMessageKey,
      },
    }
    expect(auditRecordSchema.parse(linked)).toEqual(linked)
    expect(linkedThread([record(7), linked], 'run-1')).toEqual(linkedMessageKey)
    expect(boundedStructuredText('x'.repeat(20), 8)).toEqual({ text: 'xxxxxxxx\n…', truncated: true })
  })

  it('marks run identifiers as links and validates run-to-thread targets', () => {
    expect(auditDetailFields(record(1).event)[0]).toEqual({ label: 'Run', value: 'run-1', kind: 'run' })
    expect(threadRunTargetSchema.parse({ threadId: 42, taskId: null })).toEqual({ threadId: 42, taskId: null })
  })

  it('parses typed thread and run qualifiers without consuming text search terms', () => {
    expect(parseAuditSearch('thread:42 failed tool')).toEqual({ text: 'failed tool', scope: { kind: 'thread', value: 42 } })
    expect(parseAuditSearch('model run:agent-abc')).toEqual({ text: 'model', scope: { kind: 'run', value: 'agent-abc' } })
    expect(parseAuditSearch('thread:not-a-number')).toEqual({ text: 'thread:not-a-number' })
  })
})
