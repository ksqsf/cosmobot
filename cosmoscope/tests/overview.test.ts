import { describe, expect, it } from 'vitest'
import { auditActivity, auditFailureCount, mergeAuditRecords, taskCounts } from '@/backend/overview'
import type { AuditEvent, AuditRecord, Task } from '@/types/domain'

const task = (status: Task['status']): Task => ({ id: status, label: status, detail: '', owner: '', platform: 'runtime', status, started: '', elapsed: '' })
const audit = (id: number, event: AuditEvent): AuditRecord => ({
  id,
  occurredAt: '2026-08-28T12:00:00Z',
  event,
})
const toolFinished = (status: string): Extract<AuditEvent, { tag: 'ToolCallFinished' }> => ({
  tag: 'ToolCallFinished', runId: 'run-1', turn: 1, toolCallId: 'call-1', toolName: 'read', status,
  result: '', resultLength: 0, messageIds: [],
})

describe('overview derivations', () => {
  it('counts task states without treating stopped work as active', () => {
    expect(taskCounts((['running', 'waiting', 'completed', 'failed', 'stopped'] satisfies Task['status'][]).map(task))).toEqual({ active: 2, completed: 1, failed: 1 })
  })

  it('counts audit failures from failed tools and interrupted runs', () => {
    expect(auditFailureCount([
      audit(1, toolFinished('ok')),
      audit(2, { ...toolFinished('timeout'), toolName: 'write' }),
      audit(3, { tag: 'AgentRunInterrupted', runId: 'run-1', reason: 'cancelled' }),
    ])).toBe(2)
  })

  it('merges live records once, keeps the bounded order, and renders newest activity first', () => {
    const records = mergeAuditRecords([
      audit(1, { tag: 'AgentRunStarted', runId: 'run-1', messageId: null, maxTurns: 10, exposedTools: [], contextStrategy: null }),
      audit(2, { tag: 'ModelTurnStarted', runId: 'run-1', turn: 1, messageCount: 2, exposedTools: [], toolGroups: null }),
    ], audit(2, { tag: 'AgentRunFinished', runId: 'run-1', status: 'answered', finalLength: 8, turnsUsed: 1 }), 2)
    expect(records.map(({ id }) => id)).toEqual([1, 2])
    expect(auditActivity(records).map(({ id, kind }) => [id, kind])).toEqual([['2', 'Agent finished'], ['1', 'Agent started']])
  })
})
