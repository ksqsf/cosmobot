import { Effect } from 'effect'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { flushPromises, mount, shallowMount } from '@vue/test-utils'
import type { DOMWrapper, VueWrapper } from '@vue/test-utils'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { mergeChatLogItems, mergeChatMessage, safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { highlightCode, mediaRefsInText, renderMarkdown } from '@/markdown'
import { RpcClient } from '@/rpc/client'
import { liveAdminMethods } from '@/rpc/protocol'
import { chatLogListSchema } from '@/rpc/schemas'
import type { ChatMessage } from '@/types/domain'

const pageMocks = vi.hoisted(() => ({
  route: { params: {}, query: {} },
  router: { push: vi.fn(() => Promise.resolve()), replace: vi.fn(() => Promise.resolve()) },
  connection: { state: 'authenticated', methods: new Set<string>(), error: '' },
  respond: vi.fn<(operation: { kind: string, [key: string]: unknown }) => unknown>(),
  listOperation: { kind: 'list' },
  discard: vi.fn((attachmentId: string) => ({ kind: 'discard', attachmentId })),
}))

vi.mock('vue-router', () => ({ useRoute: () => pageMocks.route, useRouter: () => pageMocks.router }))
vi.mock('primevue/usetoast', () => ({ useToast: () => ({ add: vi.fn() }) }))
vi.mock('@/stores/connection', () => ({ useConnectionStore: () => pageMocks.connection }))
vi.mock('@/overlay', () => ({
  useLayeredConfirm: () => ({ require: vi.fn() }),
  useOverlayLayer: () => ({ isTop: true, show: vi.fn(), hide: vi.fn() }),
}))
vi.mock('@/backend/AdminBackend', () => ({
  listChatSessions: pageMocks.listOperation,
  loadChatHistory: (sessionId: string) => ({ kind: 'history', sessionId }),
  subscribeChat: (sessionId: string, refresh: () => Promise<void>) => ({ kind: 'subscribe', sessionId, refresh }),
  uploadChatAttachment: (file: File) => ({ kind: 'upload', file }),
  discardChatAttachment: pageMocks.discard,
  sendChatMessage: (message: unknown) => ({ kind: 'send', message }),
  openChatSession: () => ({ kind: 'create' }),
  renameChatSession: () => ({ kind: 'unused' }),
  forkChatSession: () => ({ kind: 'unused' }),
  deleteChatSession: () => ({ kind: 'unused' }),
}))
vi.mock('@/backend/runBackend', () => ({ runBackend: (operation: { kind: string, [key: string]: unknown }) => Promise.resolve(pageMocks.respond(operation)) }))

import ChatPage from '@/pages/ChatPage.vue'
import MessageContent from '@/components/MessageContent.vue'
import ChatTranscript from '@/components/chat/ChatTranscript.vue'
import { chatMethods } from '@/rpc/protocol'

const message: ChatMessage = {
  sessionId: 'session-1',
  messageId: 'message-1',
  sender: 'assistant',
  text: 'draft',
  imageUrls: [],
  attachments: [],
  replyToMessageId: null,
  parentMessageId: null,
}

describe('chat projection', () => {
  it('accepts native platform chat identifiers', () => {
    const scope = { platform: 'PlatformMatrix', kind: 'ChatPrivate', chatId: '!room:example.org' }
    expect(chatLogListSchema.parse({ chats: [{ scope, chatDisplayName: null, messageCount: 1, latestAt: null }] }).chats[0]?.scope).toEqual(scope)
  })

  it('requests bounded history pages with a stable cursor', async () => {
    const client = new RpcClient()
    const request = vi.spyOn(client, 'request').mockResolvedValue({ sessionId: 'session-1', messages: [message], hasOlder: true })
    const history = await Effect.runPromise(makeRpcBackend(client, new Set(liveAdminMethods)).chat.history('session-1', 'message-20', 50))

    expect(history).toEqual({ sessionId: 'session-1', messages: [message], hasOlder: true })
    expect(request).toHaveBeenCalledWith('chat.history', { sessionId: 'session-1', beforeMessageId: 'message-20', limit: 50 })
  })

  it('keeps platform chat logs as a bounded sliding window', () => {
    const item = (rowId: number) => ({ rowId, threadId: null, entry: { platform: 'PlatformRPC', kind: 'ChatPrivate', chatId: '1', recordedAt: null, senderId: null, senderUsername: null, senderDisplayName: null, messageId: String(rowId), replyToMessageId: null, isBot: false, mentions: [], mentionUsernames: [], imageUrls: [], files: [], text: '' } } as const)
    expect(mergeChatLogItems([item(3), item(4)], [item(1), item(2), item(3)], 'older', 3)).toEqual({ entries: [item(1), item(2), item(3)], pruned: true })
    expect(mergeChatLogItems([item(1), item(2)], [item(2), item(3), item(4)], 'newer', 3)).toEqual({ entries: [item(2), item(3), item(4)], pruned: true })
  })

  it('deduplicates complete streaming snapshots in place', () => {
    expect(mergeChatMessage([message], { ...message, text: 'new draft' })).toEqual([{ ...message, text: 'new draft' }])
  })

  it('allows browser-safe image and download URLs only', () => {
    const base = 'https://cosmoscope.example/chat'
    expect(safeImageUrl('/media/image.webp', base)).toBe('https://cosmoscope.example/media/image.webp')
    expect(safeImageUrl('data:image/png;base64,AA==', base)).toBe('data:image/png;base64,AA==')
    expect(safeImageUrl('data:image/svg+xml,<svg/>', base)).toBeUndefined()
    expect(safeDownloadUrl('javascript:alert(1)', base)).toBeUndefined()
    expect(safeDownloadUrl('https://files.example/report.pdf', base)).toBe('https://files.example/report.pdf')
  })

  it('renders CommonMark and KaTeX without enabling raw HTML', () => {
    const rendered = renderMarkdown('## Result\n\n$E = mc^2$\n\n\\[a^2+b^2=c^2\\]\n\n<script>alert(1)</script>')
    expect(rendered).toContain('<h2>Result</h2>')
    expect(rendered).toContain('class="katex"')
    expect(rendered).toContain('class="katex-display"')
    expect(rendered).not.toContain('[a^2+b^2=c^2]')
    expect(rendered).toContain('&lt;script&gt;')
    expect(rendered).not.toContain('<script>')
  })

  it('syntax-highlights code without allowing HTML through', () => {
    expect(renderMarkdown('```ts\nconst answer: number = 42\n```')).toContain('hljs-keyword')
    expect(highlightCode('{"value":"<script>"}', 'json')).toContain('&lt;script&gt;')
  })

  it('renders tables and treats frontmatter as metadata', () => {
    const rendered = renderMarkdown('---\nname: example\ndescription: Test\n---\n\n| A | B |\n| - | - |\n| 1 | 2 |')
    expect(rendered).toContain('<table>')
    expect(rendered).toContain('<td>1</td>')
    expect(rendered).not.toContain('description: Test')
  })

  it('links media references embedded in tool result text', () => {
    const ref = 'media:mf_5StthYV0IIB0-yoo1w5DRw'
    const rendered = renderMarkdown(`Generated image. Media ids: ${ref}`)
    expect(rendered).toContain(`href="/media/${encodeURIComponent(ref)}"`)
    expect(rendered).toContain(`data-media-ref="${ref}"`)
  })

  it('links compacted tool-result media ids using their canonical reference', () => {
    const id = 'mf_Q2JHMRvDjVlW_MceWO3S8g'
    const ref = `media:${id}`
    const source = `[tool result omitted; media_id=${id}, mime=image/png]`

    expect(renderMarkdown(source)).toContain(`data-media-ref="${ref}"`)
    expect(mediaRefsInText(source)).toEqual([ref])
  })

  it('opens markdown images through the shared message preview', async () => {
    const wrapper = mount(MessageContent, { props: { text: '![result](/inline.png)', images: [], attachments: [] } })
    await wrapper.find('.markdown-body img').trigger('click')

    expect(wrapper.emitted('previewImage')?.[0]?.[0]).toMatch(/\/inline\.png$/u)
  })
})

const success = <T>(value: T): { _tag: 'Success', value: T } => ({ _tag: 'Success', value })
const sessions = [
  { sessionId: 'session-1', label: 'First', createdAt: null, updatedAt: null },
  { sessionId: 'session-2', label: 'Second', createdAt: null, updatedAt: null },
]

function deferred<T>(): { promise: Promise<T>, resolve: (value: T) => void } {
  let resolve!: (value: T) => void
  const promise = new Promise<T>((done) => { resolve = done })
  return { promise, resolve }
}

async function mountChat(): Promise<VueWrapper> {
  const wrapper = shallowMount(ChatPage, { global: { stubs: {
    PageHeading: { template: '<div><slot /></div>' },
    ChatSessionList: false,
    ChatTranscript: false,
    ChatMessage: false,
    ChatMessageItem: false,
    ChatComposer: false,
    AttachmentTray: false,
    MessageContent: false,
  } } })
  await flushPromises()
  return wrapper
}

function secondSessionButton(wrapper: VueWrapper): DOMWrapper<Element> {
  const button = wrapper.findAll('.conversation').at(1)
  if (button === undefined) throw new Error('Expected a second chat session')
  return button
}

describe('chat page session lifecycles', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    pageMocks.route.params = {}
    pageMocks.route.query = {}
    pageMocks.connection.state = 'authenticated'
    pageMocks.connection.methods = new Set(chatMethods)
  })

  it('does not apply a completed send to a newly selected session', async () => {
    const firstSend = deferred<ReturnType<typeof success<string>>>()
    const secondSend = deferred<ReturnType<typeof success<string>>>()
    let sends = 0
    const attachment = { attachmentId: 'sent', name: 'sent.txt', kind: 'file', mediaType: 'text/plain', size: 1, url: '/sent' }
    pageMocks.respond.mockImplementation((operation) => {
      if (operation.kind === 'send') { sends += 1; return sends === 1 ? firstSend.promise : secondSend.promise }
      return operation.kind === 'upload' ? success(attachment) : operation.kind === 'list' ? success(sessions) : success(() => undefined)
    })
    const wrapper = await mountChat()
    const composer = wrapper.findComponent({ name: 'Textarea' })
    const input = wrapper.find('input[type="file"]')
    Object.defineProperty(input.element, 'files', { configurable: true, value: [new File(['x'], 'sent.txt')] })
    await input.trigger('change')
    await flushPromises()
    composer.vm.$emit('update:modelValue', 'message for first')
    await wrapper.find('form').trigger('submit')

    await secondSessionButton(wrapper).trigger('click')
    await flushPromises()
    expect(pageMocks.discard).not.toHaveBeenCalled()
    composer.vm.$emit('update:modelValue', 'draft for second')
    await wrapper.find('form').trigger('submit')
    firstSend.resolve(success('message-1'))
    await flushPromises()

    expect(wrapper.findComponent({ name: 'Textarea' }).attributes('modelvalue')).toBe('draft for second')
    expect(wrapper.findAll('.message')).toHaveLength(0)
    secondSend.resolve(success('message-2'))
    await flushPromises()
    expect(wrapper.findAll('.message')).toHaveLength(1)
  })

  it('discards an upload that completes after changing sessions', async () => {
    const pendingUpload = deferred<ReturnType<typeof success<unknown>>>()
    pageMocks.respond.mockImplementation((operation) => operation.kind === 'upload' ? pendingUpload.promise : operation.kind === 'list' ? success(sessions) : success(() => undefined))
    const wrapper = await mountChat()
    const input = wrapper.find('input[type="file"]')
    Object.defineProperty(input.element, 'files', { configurable: true, value: [new File(['x'], 'late.txt')] })
    await input.trigger('change')
    await secondSessionButton(wrapper).trigger('click')
    pendingUpload.resolve(success({ attachmentId: 'late', name: 'late.txt', kind: 'file', mediaType: 'text/plain', size: 1, url: '/late' }))
    await flushPromises()

    expect(pageMocks.discard).toHaveBeenCalledWith('late')
    expect(wrapper.text()).not.toContain('late.txt')
  })

  it('ignores an older session-list response', async () => {
    const oldList = deferred<ReturnType<typeof success<typeof sessions>>>()
    let lists = 0
    pageMocks.respond.mockImplementation((operation) => {
      if (operation.kind !== 'list') return success(() => undefined)
      lists += 1
      return lists === 1 ? oldList.promise : success(sessions.slice(1))
    })
    const wrapper = await mountChat()
    const rpcSessions = wrapper.find('button-stub[label="RPC sessions"]')
    await rpcSessions.trigger('click')
    await flushPromises()
    oldList.resolve(success(sessions))
    await flushPromises()

    expect(wrapper.text()).toContain('Second')
    expect(wrapper.text()).not.toContain('First')
  })

  it('ignores an older history refresh in the same session', async () => {
    Element.prototype.scrollIntoView = vi.fn()
    const oldHistory = deferred<ReturnType<typeof success<unknown>>>()
    const newHistory = deferred<ReturnType<typeof success<unknown>>>()
    let histories = 0
    pageMocks.respond.mockImplementation((operation) => {
      if (operation.kind === 'list') return success(sessions)
      if (operation.kind === 'history') { histories += 1; return histories === 1 ? oldHistory.promise : newHistory.promise }
      return success(() => undefined)
    })
    const wrapper = await mountChat()
    const subscribe = pageMocks.respond.mock.calls.map(([operation]) => operation).find(({ kind }) => kind === 'subscribe')
    const refresh = subscribe?.['refresh'] as (() => Promise<void>) | undefined
    if (refresh === undefined) throw new Error('Expected a chat refresh callback')

    const oldRequest = refresh()
    const newRequest = refresh()
    newHistory.resolve(success({ messages: [{ ...message, text: 'new history' }], hasOlder: false }))
    await newRequest
    oldHistory.resolve(success({ messages: [{ ...message, text: 'old history' }], hasOlder: false }))
    await oldRequest
    await flushPromises()

    expect(wrapper.text()).toContain('new history')
    expect(wrapper.text()).not.toContain('old history')
  })

  it('does not restore an attachment removed while sending or changing sessions', async () => {
    const pendingDiscard = deferred<{ _tag: 'Failure', error: { message: string } }>()
    const attachment = { attachmentId: 'late', name: 'late.txt', kind: 'file', mediaType: 'text/plain', size: 1, url: '/late' }
    pageMocks.respond.mockImplementation((operation) => {
      if (operation.kind === 'list') return success(sessions)
      if (operation.kind === 'upload') return success(attachment)
      if (operation.kind === 'discard') return pendingDiscard.promise
      if (operation.kind === 'send') return success('message-1')
      return success(() => undefined)
    })
    const wrapper = await mountChat()
    const input = wrapper.find('input[type="file"]')
    Object.defineProperty(input.element, 'files', { configurable: true, value: [new File(['x'], 'late.txt')] })
    await input.trigger('change')
    await flushPromises()
    await wrapper.find('button-stub[aria-label="Remove late.txt"]').trigger('click')
    wrapper.findComponent({ name: 'Textarea' }).vm.$emit('update:modelValue', 'text only')
    await wrapper.find('form').trigger('submit')
    await secondSessionButton(wrapper).trigger('click')
    pendingDiscard.resolve({ _tag: 'Failure', error: { message: 'too late' } })
    await flushPromises()

    const send = pageMocks.respond.mock.calls.map(([operation]) => operation).find(({ kind }) => kind === 'send')
    expect(send?.['message']).toMatchObject({ attachments: [] })
    expect(wrapper.text()).not.toContain('late.txt')
    expect(pageMocks.discard).toHaveBeenCalledOnce()
  })

  it('does not continue creating a session after unmount', async () => {
    const pendingCreate = deferred<ReturnType<typeof success<{ sessionId: string }>>>()
    pageMocks.respond.mockImplementation((operation) => operation.kind === 'create' ? pendingCreate.promise : operation.kind === 'list' ? success(sessions) : success(() => undefined))
    const wrapper = await mountChat()
    const listsBefore = pageMocks.respond.mock.calls.filter(([operation]) => operation.kind === 'list').length
    await wrapper.find('button-stub[label="New session"]').trigger('click')
    wrapper.unmount()
    pendingCreate.resolve(success({ sessionId: 'late-session' }))
    await flushPromises()

    expect(pageMocks.respond.mock.calls.filter(([operation]) => operation.kind === 'list')).toHaveLength(listsBefore)
  })

  it('does not apply an older-page scroll adjustment to a new session', async () => {
    const pendingOlder = deferred<undefined>()
    const loadOlder = vi.fn(() => pendingOlder.promise)
    const first = { sessionId: 'session-1', label: 'First', parentSessionId: null, parentMessageId: null }
    const second = { sessionId: 'session-2', label: 'Second', parentSessionId: null, parentMessageId: null }
    const wrapper = shallowMount(ChatTranscript, {
      props: {
        session: first,
        messages: [message],
        streamingMessageIds: new Set<string>(),
        loading: false,
        loadingOlder: false,
        hasOlder: true,
        loadOlder,
      } as never,
    })
    const pane = wrapper.find('.messages').element
    Object.defineProperty(pane, 'scrollHeight', { configurable: true, value: 100 })
    pane.scrollTop = 7
    await wrapper.find('button-stub[label="Load older messages"]').trigger('click')
    await wrapper.setProps({ session: second, messages: [] } as never)
    Object.defineProperty(pane, 'scrollHeight', { configurable: true, value: 300 })
    pendingOlder.resolve(undefined)
    await flushPromises()

    expect(loadOlder).toHaveBeenCalledOnce()
    expect(pane.scrollTop).toBe(7)
  })
})
