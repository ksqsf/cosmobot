import { describe, expect, it } from 'vitest'
import { threadStats } from '@/backend/threadStats'
import type { AuditRecord } from '@/types/domain'

describe('threadStats', () => {
  it('pairs model and tool events and sums reported usage', () => {
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '2026-01-01T00:00:00Z', event: { tag: 'AgentRunStarted', runId: 'run-1', messageId: null, maxTurns: 8, exposedTools: [], contextStrategy: null } },
      { id: 2, occurredAt: '2026-01-01T00:00:01Z', event: { tag: 'ModelTurnStarted', runId: 'run-1', turn: 1, messageCount: 3, exposedTools: [], toolGroups: null } },
      { id: 3, occurredAt: '2026-01-01T00:00:03Z', event: { tag: 'ModelTurnFinished', runId: 'run-1', turn: 1, answerKind: 'tool_request', contentLength: 0, toolCalls: [], tokenUsage: { prompt_tokens: 10, completion_tokens: 4, total_tokens: 14 } } },
      { id: 4, occurredAt: '2026-01-01T00:00:04Z', event: { tag: 'ToolCallStarted', runId: 'run-1', turn: 1, toolCall: { id: 'call-1', name: 'test', arguments: '{}' } } },
      { id: 5, occurredAt: '2026-01-01T00:00:05.500Z', event: { tag: 'ToolCallFinished', runId: 'run-1', turn: 1, toolCallId: 'call-1', toolName: 'test', status: 'success', result: 'ok', resultLength: 2, messageIds: [] } },
      { id: 6, occurredAt: '2026-01-01T00:00:07Z', event: { tag: 'AgentRunFinished', runId: 'run-1', status: 'success', finalLength: 2, turnsUsed: 1 } },
    ]

    expect(threadStats(records)).toMatchObject({
      runs: 1,
      modelTurns: 1,
      toolCalls: 1,
      contextMessages: 3,
      modelMilliseconds: 2000,
      toolMilliseconds: 1500,
      wallMilliseconds: 7000,
      tokens: { prompt_tokens: 10, completion_tokens: 4, total_tokens: 14 },
    })
  })

  it('does not report missing model usage as zero tokens', () => {
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '2026-01-01T00:00:00Z', event: { tag: 'ModelTurnStarted', runId: 'run-1', turn: 1, messageCount: 1, exposedTools: [], toolGroups: null } },
      { id: 2, occurredAt: '2026-01-01T00:00:01Z', event: { tag: 'ModelTurnFinished', runId: 'run-1', turn: 1, answerKind: 'final', contentLength: 2, toolCalls: [], tokenUsage: null } },
    ]

    expect(threadStats(records).tokens).toBeNull()
  })
})
