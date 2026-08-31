import { flushPromises, shallowMount } from '@vue/test-utils'
import { describe, expect, it, vi } from 'vitest'
import { auditActivity, auditFailureCount, mergeAuditRecords, taskCounts } from '@/backend/overview'
import type { AuditEvent, AuditRecord, Task } from '@/types/domain'

const pageMocks = vi.hoisted(() => ({
  connection: { state: 'authenticated', methods: new Set(['audit.recent', 'audit.subscribe']), error: '' },
  runBackend: vi.fn(),
}))

vi.mock('@/backend/AdminBackend', () => ({
  countAudit: 'audit.count',
  countResources: 'resource.count',
  countSessions: 'chat.count',
  listChatLogs: 'chat-logs.list',
  listMedia: () => 'media.list',
  listTasks: 'tasks.list',
  listThreads: () => 'threads.list',
  recentAudit: () => 'audit.recent',
  subscribeAudit: (...args: unknown[]) => ({ operation: 'subscribe', args }),
}))
vi.mock('@/backend/runBackend', () => ({ runBackend: pageMocks.runBackend }))
vi.mock('@/stores/connection', () => ({ useConnectionStore: () => pageMocks.connection }))
vi.mock('vue-router', () => ({
  RouterLink: { template: '<a><slot /></a>' },
  useRouter: () => ({ push: vi.fn() }),
}))
vi.mock('@/overlay', () => ({ useOverlayLayer: () => ({ isTop: { value: true } }) }))

const task = (status: Task['status'], id: number): Task => ({ id, label: status, status, error: null, startedAt: '2026-08-28T12:00:00Z', finishedAt: status === 'running' ? null : '2026-08-28T12:01:00Z' })
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
  it('counts only running manager tasks as active', () => {
    expect(taskCounts((['running', 'completed', 'failed', 'cancelled'] satisfies Task['status'][]).map(task))).toEqual({ active: 1, completed: 1, failed: 1 })
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

  it('cleans up an audit subscription that finishes installing after unmount', async () => {
    let finishInstall: ((result: { _tag: 'Success'; value: () => void }) => void) | undefined
    const cleanup = vi.fn()
    pageMocks.runBackend.mockImplementation((operation) => operation.operation === 'subscribe'
      ? new Promise((resolve) => { finishInstall = resolve })
      : Promise.reject(new Error('unexpected overview request')))
    const { default: OverviewPage } = await import('@/pages/OverviewPage.vue')
    const wrapper = shallowMount(OverviewPage)
    await flushPromises()
    wrapper.unmount()
    finishInstall?.({ _tag: 'Success', value: cleanup })
    await flushPromises()
    expect(cleanup).toHaveBeenCalledOnce()
  })
})
