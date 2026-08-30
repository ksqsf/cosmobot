import { describe, expect, it } from 'vitest'
import { auditRecordsLinkedTo, threadStats } from '@/backend/threadStats'
import type { AuditRecord, ThreadMessageKey } from '@/types/domain'

describe('threadStats', () => {
  it('pairs model and tool events and sums reported usage', () => {
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '2026-01-01T00:00:00Z', event: { tag: 'AgentRunStarted', runId: 'run-1', messageId: null, maxTurns: 8, exposedTools: [], contextStrategy: null } },
      { id: 2, occurredAt: '2026-01-01T00:00:01Z', event: { tag: 'ModelTurnStarted', runId: 'run-1', turn: 1, messageCount: 3, exposedTools: [], toolGroups: null } },
      { id: 3, occurredAt: '2026-01-01T00:00:03Z', event: { tag: 'ModelTurnFinished', runId: 'run-1', turn: 1, answerKind: 'tool_request', contentLength: 0, toolCalls: [], tokenUsage: { prompt_tokens: 10, completion_tokens: 4, total_tokens: 14, prompt_tokens_details: { cached_tokens: 8 } } } },
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
      cachedPromptTokens: 8,
      promptCacheHitRate: 0.8,
    })
  })

  it('does not report missing model usage as zero tokens', () => {
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '2026-01-01T00:00:00Z', event: { tag: 'ModelTurnStarted', runId: 'run-1', turn: 1, messageCount: 1, exposedTools: [], toolGroups: null } },
      { id: 2, occurredAt: '2026-01-01T00:00:01Z', event: { tag: 'ModelTurnFinished', runId: 'run-1', turn: 1, answerKind: 'final', contentLength: 2, toolCalls: [], tokenUsage: null } },
    ]

    expect(threadStats(records).tokens).toBeNull()
  })

  it('keeps cumulative usage on the selected reply branch', () => {
    const key = (messageId: string): ThreadMessageKey => ({ platform: 'PlatformRPC', chatId: null, messageId })
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '2026-01-01T00:00:00Z', event: { tag: 'ModelTurnStarted', runId: 'root', turn: 1, messageCount: 1, exposedTools: [], toolGroups: null } },
      { id: 2, occurredAt: '2026-01-01T00:00:01Z', event: { tag: 'ModelTurnFinished', runId: 'root', turn: 1, answerKind: 'final', contentLength: 1, toolCalls: [], tokenUsage: { prompt_tokens: 10, completion_tokens: 1, total_tokens: 11 } } },
      { id: 3, occurredAt: '2026-01-01T00:00:02Z', event: { tag: 'AgentThreadLinked', runId: 'root', linkedMessageId: 'root', linkedMessageKey: key('root'), parentMessageId: null } },
      { id: 4, occurredAt: '2026-01-01T00:00:03Z', event: { tag: 'ModelTurnStarted', runId: 'left', turn: 1, messageCount: 2, exposedTools: [], toolGroups: null } },
      { id: 5, occurredAt: '2026-01-01T00:00:04Z', event: { tag: 'ModelTurnFinished', runId: 'left', turn: 1, answerKind: 'final', contentLength: 1, toolCalls: [], tokenUsage: { prompt_tokens: 20, completion_tokens: 2, total_tokens: 22 } } },
      { id: 6, occurredAt: '2026-01-01T00:00:05Z', event: { tag: 'AgentThreadLinked', runId: 'left', linkedMessageId: 'left', linkedMessageKey: key('left'), parentMessageId: 'root' } },
      { id: 7, occurredAt: '2026-01-01T00:00:06Z', event: { tag: 'ModelTurnStarted', runId: 'right', turn: 1, messageCount: 2, exposedTools: [], toolGroups: null } },
      { id: 8, occurredAt: '2026-01-01T00:00:07Z', event: { tag: 'ModelTurnFinished', runId: 'right', turn: 1, answerKind: 'final', contentLength: 1, toolCalls: [], tokenUsage: { prompt_tokens: 30, completion_tokens: 3, total_tokens: 33 } } },
      { id: 9, occurredAt: '2026-01-01T00:00:08Z', event: { tag: 'AgentThreadLinked', runId: 'right', linkedMessageId: 'right', linkedMessageKey: key('right'), parentMessageId: 'root' } },
    ]

    expect(threadStats(auditRecordsLinkedTo(records, [key('root'), key('left')])).tokens?.total_tokens).toBe(33)
  })

  it('keeps backend run occurrences intact when audit ids interleave', () => {
    const key = (messageId: string): ThreadMessageKey => ({ platform: 'PlatformRPC', chatId: null, messageId })
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '2026-01-01T00:00:00Z', event: { tag: 'ModelTurnStarted', runId: 'root', turn: 1, messageCount: 1, exposedTools: [], toolGroups: null } },
      { id: 4, occurredAt: '2026-01-01T00:00:03Z', event: { tag: 'AgentThreadLinked', runId: 'root', linkedMessageId: 'root', linkedMessageKey: key('root'), parentMessageId: null } },
      { id: 2, occurredAt: '2026-01-01T00:00:01Z', event: { tag: 'ModelTurnStarted', runId: 'sibling', turn: 1, messageCount: 1, exposedTools: [], toolGroups: null } },
      { id: 3, occurredAt: '2026-01-01T00:00:02Z', event: { tag: 'ModelTurnFinished', runId: 'sibling', turn: 1, answerKind: 'final', contentLength: 1, toolCalls: [], tokenUsage: { prompt_tokens: 30, completion_tokens: 3, total_tokens: 33 } } },
      { id: 5, occurredAt: '2026-01-01T00:00:04Z', event: { tag: 'AgentThreadLinked', runId: 'sibling', linkedMessageId: 'sibling', linkedMessageKey: key('sibling'), parentMessageId: null } },
    ]

    expect(auditRecordsLinkedTo(records, [key('root')]).map(({ event }) => event.runId)).toEqual(['root', 'root'])
  })
})
