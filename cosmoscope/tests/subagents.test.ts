import { effectScope } from 'vue'
import { flushPromises } from '@vue/test-utils'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { subagentRuns, subagentStatus } from '@/domain/subagents'
import { auditRecordsLinkedTo } from '@/backend/threadStats'
import { useSubagentInspector } from '@/composables/useSubagentInspector'
import type { AuditEvent, AuditRecord } from '@/types/domain'

const mocks = vi.hoisted(() => ({ run: vi.fn() }))
vi.mock('@/backend/runBackend', () => ({ runBackend: mocks.run }))
vi.mock('@/backend/AdminBackend', () => ({
  getRunTranscript: (runId: string) => ({ tag: 'transcript', runId }),
  getRunAudit: (runId: string) => ({ tag: 'audit', runId }),
  resolveThreadRun: (runId: string) => ({ tag: 'resolve', runId }),
  subscribeAudit: (refresh: () => Promise<void>, handler: (record: AuditRecord) => void) => ({ tag: 'subscribe', refresh, handler }),
}))

const record = (id: number, event: AuditEvent): AuditRecord => ({ id, occurredAt: '2026-01-01T00:00:00Z', event })
const child = (childRunId: string, runId = 'parent'): { runId: string, childRunId: string, subagentId: string } => ({ runId, childRunId, subagentId: 'worker' })
const start = (id: number, childRunId: string, runId = 'parent'): AuditRecord => record(id, { tag: 'SubAgentRunStarted', ...child(childRunId, runId) })
const finish = (id: number, runId: string): AuditRecord => record(id, { tag: 'AgentRunFinished', runId, status: 'success', finalLength: 0, turnsUsed: 1 })
const success = (value: unknown): unknown => ({ _tag: 'Success', value })

describe('subagent projection', () => {
  it('deduplicates by child run, retaining repeated names and branch scope', () => {
    const key = (messageId: string): { platform: 'PlatformRPC', chatId: null, messageId: string } => ({ platform: 'PlatformRPC' as const, chatId: null, messageId })
    const link = (id: number, runId: string): AuditRecord => record(id, {
      tag: 'AgentThreadLinked', runId, linkedMessageId: runId, linkedMessageKey: key(runId), parentMessageId: null,
    })
    const records = [start(1, 'a'), start(1, 'a'), start(2, 'b'), link(5, 'parent'), start(3, 'c', 'sibling'), link(6, 'sibling')]
    expect(subagentRuns(auditRecordsLinkedTo(records, [key('parent')])).map(({ childRunId }) => childRunId)).toEqual(['a', 'b'])
    expect(subagentRuns(auditRecordsLinkedTo(records, [key('sibling')])).map(({ childRunId }) => childRunId)).toEqual(['c'])
  })

  it('uses terminal evidence and audit order, never absence as success', () => {
    expect(subagentStatus([])).toBe('Unknown')
    expect(subagentStatus([start(1, 'a')])).toBe('Unknown')
    expect(subagentStatus([finish(3, 'a'), record(2, { tag: 'AgentRunInterrupted', runId: 'a', reason: 'cancelled' })])).toBe('success')
    expect(subagentStatus([record(4, { tag: 'AgentRunInterrupted', runId: 'a', reason: 'cancelled' }), finish(3, 'a')])).toBe('Interrupted: cancelled')
  })

  it('shows waiting until a join or a later model turn', () => {
    const waiting = record(1, { tag: 'AgentChildrenWait', runId: 'parent', waiting: true, subagentIds: ['worker'] })
    const joined = record(2, { tag: 'AgentChildrenWait', runId: 'parent', waiting: false, subagentIds: ['worker'] })
    expect(subagentStatus([waiting])).toBe('Waiting for subagents')
    expect(subagentStatus([joined, waiting])).toBe('Running')
    expect(subagentStatus([waiting, finish(3, 'parent')])).toBe('success')
  })

  it('keeps distinct child runs under shuffled duplicate delivery', () => {
    let seed = 73
    const random = (): number => { seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed }
    for (let trial = 0; trial < 40; trial += 1) {
      const count = 1 + random() % 20
      const events = Array.from({ length: count }, (_, index) => start(index + 1, `child-${String(index)}`))
      const delivered = events.flatMap((event) => Array.from({ length: 1 + random() % 4 }, () => event))
      for (let index = delivered.length - 1; index > 0; index -= 1) {
        const other = random() % (index + 1)
        const a = delivered[index]
        const b = delivered[other]
        if (a && b) { delivered[index] = b; delivered[other] = a }
      }
      expect(subagentRuns(delivered).map(({ childRunId }) => childRunId)).toEqual(
        Array.from({ length: count }, (_, index) => `child-${String(index)}`),
      )
    }
  })
})

describe('subagent selection lifecycle', () => {
  interface Subscription { tag: string, refresh: () => Promise<void>, handler: (record: AuditRecord) => void }
  let subscriptions: Subscription[]
  let stops: ReturnType<typeof vi.fn>[]
  beforeEach(() => {
    subscriptions = []
    stops = []
    mocks.run.mockReset().mockImplementation((operation: Subscription) => {
      if (operation.tag === 'subscribe') {
        subscriptions.push(operation)
        const stop = vi.fn()
        stops.push(stop)
        return Promise.resolve(success(stop))
      }
      return Promise.resolve(success(operation.tag === 'transcript' ? null : operation.tag === 'audit' ? [] : { threadId: null, taskId: null }))
    })
  })

  it('ignores stale snapshots, merges live events, and releases on back and disposal', async () => {
    const pending: ((value: unknown) => void)[] = []
    const defaultRun = mocks.run.getMockImplementation()
    mocks.run.mockImplementation((operation: Subscription) => operation.tag === 'audit'
      ? new Promise((resolve) => pending.push(resolve)) : defaultRun?.(operation))
    const scope = effectScope()
    const view = scope.run(useSubagentInspector)
    if (!view) throw new Error('Missing scope')
    view.select(child('a'))
    await flushPromises()
    view.select(child('b', 'a'))
    await flushPromises()
    expect(stops[0]).toHaveBeenCalledOnce()
    subscriptions[0]?.handler(finish(9, 'a'))
    subscriptions[1]?.handler(finish(10, 'b'))
    pending[1]?.(success([start(2, 'nested', 'b'), finish(3, 'unrelated')]))
    pending[0]?.(success([finish(4, 'a')]))
    await flushPromises()
    expect(view.records.value.map(({ id }) => id)).toEqual([2, 10])
    expect(view.selected.value?.childRunId).toBe('b')
    view.select(child('a', 'b'))
    expect(view.trail.value).toHaveLength(2)
    view.back(1)
    await flushPromises()
    expect(view.selected.value?.childRunId).toBe('a')
    expect(stops[1]).toHaveBeenCalledOnce()
    scope.stop()
    expect(stops[2]).toHaveBeenCalledOnce()
    pending[2]?.(success([finish(11, 'a')]))
    await flushPromises()
    expect(view.records.value).toEqual([])
  })

  it('ignores a previous child conversation response', async () => {
    const pending: ((value: unknown) => void)[] = []
    const defaultRun = mocks.run.getMockImplementation()
    mocks.run.mockImplementation((operation: Subscription) => operation.tag === 'transcript'
      ? new Promise((resolve) => pending.push(resolve)) : defaultRun?.(operation))
    const scope = effectScope()
    const view = scope.run(useSubagentInspector)
    if (!view) throw new Error('Missing scope')
    view.select(child('a'))
    view.select(child('b', 'a'))
    const messages = [{ role: 'user', content: 'Question' }, { role: 'assistant', content: 'Answer' }]
    pending[1]?.(success(messages))
    pending[0]?.(success([{ role: 'assistant', content: 'Stale' }]))
    await flushPromises()
    expect(view.messages.value).toEqual(messages)
    view.back(0)
    expect(view.messages.value).toBeNull()
    scope.stop()
  })

  it('refreshes after reconnect, exposes failures, and allows retry', async () => {
    const scope = effectScope()
    const view = scope.run(useSubagentInspector)
    if (!view) throw new Error('Missing scope')
    view.select(child('a'))
    await flushPromises()
    expect(view.records.value).toEqual([])
    const defaultRun = mocks.run.getMockImplementation()
    mocks.run.mockImplementation((operation: Subscription) => operation.tag === 'audit'
      ? Promise.resolve({ _tag: 'Failure', error: { message: 'Unavailable' } }) : defaultRun?.(operation))
    await subscriptions[0]?.refresh()
    expect(view.error.value).toBe('Unavailable')
    mocks.run.mockImplementation((operation: Subscription) => operation.tag === 'audit'
      ? Promise.resolve(success([finish(2, 'a')])) : defaultRun?.(operation))
    await view.reload()
    expect(view.error.value).toBe('')
    expect(view.records.value).toEqual([finish(2, 'a')])
    view.back(0)
    expect(view.selected.value).toBeUndefined()
    expect(stops[1]).toHaveBeenCalledOnce()
    scope.stop()
  })

  it('disposes a late subscription and prevents older link lookups replacing newer ones', async () => {
    const lookups: ((value: unknown) => void)[] = []
    let subscribeDone: ((value: unknown) => void) | undefined
    let refresh: (() => Promise<void>) | undefined
    const stop = vi.fn()
    mocks.run.mockImplementation((operation: Subscription) => {
      if (operation.tag === 'resolve') return new Promise((resolve) => lookups.push(resolve))
      if (operation.tag === 'subscribe') {
        refresh = operation.refresh
        return new Promise((resolve) => { subscribeDone = resolve })
      }
      return Promise.resolve(success([]))
    })
    const scope = effectScope()
    const view = scope.run(useSubagentInspector)
    if (!view) throw new Error('Missing scope')
    view.select(child('a'))
    const reconnect = refresh?.()
    lookups[1]?.(success({ threadId: 1, taskId: null }))
    await reconnect
    lookups[0]?.(success({ threadId: null, taskId: null }))
    await flushPromises()
    expect(view.hasThread.value).toBe(true)
    scope.stop()
    subscribeDone?.(success(stop))
    await flushPromises()
    expect(stop).toHaveBeenCalledOnce()
  })
})
