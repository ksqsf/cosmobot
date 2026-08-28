import { describe, expect, it } from 'vitest'
import { auditPresentation, boundedStructuredText, linkedThread, mergeAuditRecords } from '@/backend/audit'
import { auditRecordSchema } from '@/rpc/schemas'
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
    const linkedMessageKey = { platform: 'PlatformDiscord', chatId: '1152921504606846976', messageId: 'message-2' } as const
    const linked: AuditRecord = {
      id: 8,
      occurredAt: '2026-08-28T12:00:01Z',
      event: {
        tag: 'AgentThreadLinked', runId: 'run-1', linkedMessageId: 'message-2', parentMessageId: null,
        linkedMessageKey,
      },
    }
    expect(linkedThread([record(7), linked], 'run-1')).toEqual(linkedMessageKey)
    expect(boundedStructuredText('x'.repeat(20), 8)).toEqual({ text: 'xxxxxxxx\n…', truncated: true })
  })
})
