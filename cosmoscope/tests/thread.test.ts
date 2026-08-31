import { flushPromises, mount } from '@vue/test-utils'
import { reactive } from 'vue'
import { describe, expect, it, vi } from 'vitest'
import { threadMessageChatKey, threadMessageKeyId, threadPathTo, threadToolActivity } from '@/backend/thread'
import type { ActiveThread, AuditRecord, ThreadDetail, ThreadMessageKey, ThreadNode } from '@/types/domain'

const key = (messageId: string): ThreadMessageKey => ({ platform: 'PlatformRPC', chatId: null, messageId })
const node = (messageId: string, parentMessageId: string | null): ThreadNode => ({
  messageKey: key(messageId),
  inputMessageKey: null,
  parentMessageKey: parentMessageId === null ? null : key(parentMessageId),
  messages: [],
})

describe('threadPathTo', () => {
  it('returns only the selected branch from the root', () => {
    const leaf = node('leaf', 'left')
    const nodes = [node('root', null), node('left', 'root'), node('right', 'root'), leaf]
    const lookup = new Map(nodes.map((entry) => [threadMessageKeyId(entry.messageKey), entry]))

    expect(threadPathTo(leaf, lookup).map(({ messageKey }) => messageKey.messageId)).toEqual(['root', 'left', 'leaf'])
  })

  it('links only real user and final assistant chat messages', () => {
    const inputMessageKey = key('input')
    const outputMessageKey = key('output')
    const thread: ThreadNode = {
      messageKey: outputMessageKey,
      inputMessageKey,
      parentMessageKey: null,
      messages: [
        { role: 'user', content: 'prompt' },
        { role: 'assistant', tool_calls: [{ id: 'call-1', type: 'function', function: { name: 'image_generate', arguments: '{}' } }] },
        { role: 'tool', content: '{"result":"ok"}' },
        { role: 'synthetic', content: [{ type: 'image_url', image_url: 'media:mf_image' }] },
        { role: 'assistant', content: 'done' },
      ],
    }

    expect(thread.messages.map((_, index) => threadMessageChatKey(thread, index))).toEqual([
      inputMessageKey, undefined, undefined, undefined, outputMessageKey,
    ])
  })

  it('projects running and completed tool calls from live audit events', () => {
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '', event: { tag: 'ToolCallStarted', runId: 'run', turn: 1, toolCall: { id: 'call-1', name: 'search', arguments: '{"q":"test"}' } } },
      { id: 2, occurredAt: '', event: { tag: 'ToolCallStarted', runId: 'run', turn: 1, toolCall: { id: 'call-2', name: 'fetch', arguments: '{}' } } },
      { id: 3, occurredAt: '', event: { tag: 'ToolCallFinished', runId: 'run', turn: 1, toolCallId: 'call-1', toolName: 'search', status: 'ok', result: 'found', resultLength: 5, messageIds: [] } },
    ]

    expect(threadToolActivity(records)).toEqual([
      { id: 'call-1', name: 'search', arguments: '{"q":"test"}', turn: 1, status: 'ok', result: 'found' },
      { id: 'call-2', name: 'fetch', arguments: '{}', turn: 1, status: 'running' },
    ])
  })

  it('keeps only the latest active subscription and invalidates persisted detail', async () => {
    const route = reactive({ params: {}, query: {} })
    const subscriptions: Promise<unknown>[] = []
    const threadRequests: Promise<unknown>[] = []
    vi.doMock('vue-router', () => ({
      useRoute: () => route,
      useRouter: () => ({ replace: vi.fn(), push: vi.fn(), resolve: vi.fn(() => ({ href: '/' })) }),
    }))
    vi.doMock('primevue/usetoast', () => ({ useToast: () => ({ add: vi.fn() }) }))
    vi.doMock('@/overlay', () => ({
      useLayeredConfirm: () => ({ require: vi.fn(), close: vi.fn() }),
      useOverlayLayer: () => ({ isTop: { value: true }, show: vi.fn(), hide: vi.fn() }),
    }))
    vi.doMock('@/stores/connection', () => ({
      useConnectionStore: () => reactive({ state: 'authenticated', error: '', methods: new Set<string>() }),
    }))
    vi.doMock('@/backend/AdminBackend', () => ({
      getMedia: (mediaId: string) => ({ tag: 'media', mediaId }),
      getRunAudit: (runId: string) => ({ tag: 'audit', runId }),
      getThread: (threadId: number) => ({ tag: 'thread', threadId }),
      getThreadAudit: (threadId: number) => ({ tag: 'threadAudit', threadId }),
      haltActiveThread: (taskId: number) => ({ tag: 'halt', taskId }),
      listActiveThreads: { tag: 'active' },
      listThreads: () => ({ tag: 'list' }),
      resolveThreadRun: (runId: string) => ({ tag: 'resolve', runId }),
      subscribeAudit: () => ({ tag: 'subscribe' }),
    }))
    vi.doMock('@/backend/runBackend', () => ({
      runBackend: (operation: { tag: string }) => {
        if (operation.tag === 'subscribe') return subscriptions.shift()
        if (operation.tag === 'thread') return threadRequests.shift()
        if (operation.tag === 'list') return Promise.resolve({ _tag: 'Success', value: { threads: [], total: 0, nodes: 0, leaves: 0, platforms: 0 } })
        if (operation.tag === 'active') return Promise.resolve({ _tag: 'Success', value: [] })
        if (operation.tag === 'audit' || operation.tag === 'threadAudit') return Promise.resolve({ _tag: 'Success', value: [] })
        return Promise.resolve({ _tag: 'Success', value: null })
      },
    }))

    const Page = (await import('@/pages/ThreadsPage.vue')).default
    const wrapper = mount(Page, { shallow: true })
    await flushPromises()
    const page = wrapper.vm as unknown as {
      monitor: (active: ActiveThread) => Promise<void>
      inspectThread: (threadId: number) => Promise<void>
      detail?: ThreadDetail
    }
    const active = (taskId: number): ActiveThread => ({
      taskId, runId: `run-${String(taskId)}`, prompt: '', parentMessageKey: null,
      parentThreadId: null, messageKeys: [], pendingSteers: 0, messages: [],
    })

    let resolveFirstSubscription!: (value: unknown) => void
    let resolveSecondSubscription!: (value: unknown) => void
    const stopFirst = vi.fn()
    const stopSecond = vi.fn()
    subscriptions.push(new Promise((resolve) => { resolveFirstSubscription = resolve }))
    await page.monitor(active(1))
    subscriptions.push(new Promise((resolve) => { resolveSecondSubscription = resolve }))
    await page.monitor(active(2))
    resolveSecondSubscription({ _tag: 'Success', value: stopSecond })
    await flushPromises()
    resolveFirstSubscription({ _tag: 'Success', value: stopFirst })
    await flushPromises()
    expect(stopFirst).toHaveBeenCalledOnce()
    expect(stopSecond).not.toHaveBeenCalled()

    let resolveThread!: (value: unknown) => void
    threadRequests.push(new Promise((resolve) => { resolveThread = resolve }))
    const persisted = page.inspectThread(42)
    subscriptions.push(Promise.resolve({ _tag: 'Success', value: vi.fn() }))
    await page.monitor(active(3))
    const detail: ThreadDetail = {
      summary: {
        threadId: 42, latestPreview: '', rootKey: key('root'), latestKey: key('root'),
        chatDisplayName: null, nodeCount: 1, leafCount: 1,
      },
      nodes: [node('root', null)],
    }
    resolveThread({ _tag: 'Success', value: detail })
    await persisted
    expect(page.detail).toBeUndefined()
    wrapper.unmount()

    vi.resetModules()
    vi.doUnmock('vue-router')
    vi.doUnmock('primevue/usetoast')
    vi.doUnmock('@/overlay')
    vi.doUnmock('@/stores/connection')
    vi.doUnmock('@/backend/AdminBackend')
    vi.doUnmock('@/backend/runBackend')
  })
})
