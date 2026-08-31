import { flushPromises, mount } from '@vue/test-utils'
import { reactive } from 'vue'
import { describe, expect, it, vi } from 'vitest'
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

  it('ignores snapshots superseded by a newer request or unmount', async () => {
    const route = reactive({ params: {}, query: {} })
    const snapshots: Promise<unknown>[] = []
    let refresh: (() => Promise<void>) | undefined
    vi.doMock('vue-router', () => ({
      useRoute: () => route,
      useRouter: () => ({ replace: vi.fn(), push: vi.fn() }),
    }))
    vi.doMock('@/backend/AdminBackend', () => {
      class RpcBackendError extends Error {}
      return {
        RpcBackendError,
        getAudit: (id: number) => ({ tag: 'get', id }),
        getRunAudit: (runId: string) => ({ tag: 'snapshot', runId }),
        getThreadAudit: (threadId: number) => ({ tag: 'thread', threadId }),
        recentAudit: () => ({ tag: 'snapshot' }),
        resolveThreadRun: (runId: string) => ({ tag: 'resolve', runId }),
        searchAudit: (text: string) => ({ tag: 'snapshot', text }),
        subscribeAudit: (reload: () => Promise<void>) => { refresh = reload; return { tag: 'subscribe' } },
      }
    })
    vi.doMock('@/backend/runBackend', () => ({
      runBackend: (operation: { tag: string }) => {
        if (operation.tag === 'subscribe') return Promise.resolve({ _tag: 'Success', value: vi.fn() })
        if (operation.tag === 'get') return Promise.resolve({ _tag: 'Success', value: null })
        if (operation.tag === 'resolve') return Promise.resolve({ _tag: 'Success', value: { threadId: null, taskId: null } })
        return snapshots.shift()
      },
    }))
    vi.doMock('@/stores/connection', () => ({
      useConnectionStore: () => reactive({
        state: 'authenticated',
        error: '',
        methods: new Set(['audit.recent', 'audit.search', 'audit.get', 'audit.thread', 'audit.subscribe']),
      }),
    }))

    const Page = (await import('@/pages/AuditPage.vue')).default
    const wrapper = mount(Page, { shallow: true })
    await flushPromises()
    const page = wrapper.vm as unknown as { events: AuditRecord[]; installSubscription: () => Promise<void> }

    let resolveFirst!: (value: unknown) => void
    snapshots.push(new Promise((resolve) => { resolveFirst = resolve }))
    const first = refresh?.()
    snapshots.push(Promise.resolve({ _tag: 'Success', value: [record(2)] }))
    await refresh?.()
    expect(page.events.map(({ id }) => id)).toEqual([2])
    resolveFirst({ _tag: 'Success', value: [record(1)] })
    await first
    expect(page.events.map(({ id }) => id)).toEqual([2])

    let resolveBeforeReconnect!: (value: unknown) => void
    snapshots.push(new Promise((resolve) => { resolveBeforeReconnect = resolve }))
    const beforeReconnect = refresh?.()
    await page.installSubscription()
    resolveBeforeReconnect({ _tag: 'Success', value: [record(3)] })
    await beforeReconnect
    expect(page.events.map(({ id }) => id)).toEqual([2])

    let resolveAfterUnmount!: (value: unknown) => void
    snapshots.push(new Promise((resolve) => { resolveAfterUnmount = resolve }))
    const afterUnmount = refresh?.()
    wrapper.unmount()
    resolveAfterUnmount({ _tag: 'Success', value: [record(4)] })
    await afterUnmount
    expect(page.events.map(({ id }) => id)).toEqual([2])
    vi.resetModules()
    vi.doUnmock('vue-router')
    vi.doUnmock('@/backend/AdminBackend')
    vi.doUnmock('@/backend/runBackend')
    vi.doUnmock('@/stores/connection')
  })
})
