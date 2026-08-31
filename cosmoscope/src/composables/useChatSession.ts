import { computed, onMounted, ref, watch, type ComputedRef, type Ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useLatest, useLatestSubscription, type LatestToken } from '@/async'
import { deleteChatSession, forkChatSession, listChatSessions, loadChatHistory, openChatSession, renameChatSession, subscribeChat } from '@/backend/AdminBackend'
import { mergeChatMessage } from '@/backend/chat'
import { runBackend } from '@/backend/runBackend'
import { chatMethods } from '@/rpc/protocol'
import { useConnectionStore } from '@/stores/connection'
import type { ChatMessage, ChatSession } from '@/types/domain'

export interface ChatSelection {
  readonly sessionId: string
  readonly token: LatestToken
}

interface ChatSessionOptions {
  readonly error: Ref<string>
  readonly beforeSelectionChange: () => Promise<boolean>
  readonly selectionChanged: () => void
  readonly selectionInvalidated: () => void
  readonly sessionDeleted: (sessionId: string) => void
}

export interface ChatSessionState {
  readonly sessions: Ref<readonly ChatSession[]>
  readonly messages: Ref<readonly ChatMessage[]>
  readonly selectedId: Ref<string | undefined>
  readonly selectedSession: ComputedRef<ChatSession | undefined>
  readonly sessionsLoading: Ref<boolean>
  readonly sessionsLoaded: Ref<boolean>
  readonly loading: Ref<boolean>
  readonly loadingOlder: Ref<boolean>
  readonly hasOlder: Ref<boolean>
  readonly streamingMessageIds: Ref<ReadonlySet<string>>
  readonly live: ComputedRef<boolean>
  readonly showingPlatformLogs: ComputedRef<boolean>
  readonly showChatView: (view: 'sessions' | 'logs') => Promise<void>
  readonly loadSessions: (preferredId?: string) => Promise<void>
  readonly selectSession: (sessionId: string) => Promise<void>
  readonly loadOlder: () => Promise<void>
  readonly createSession: () => Promise<void>
  readonly renameSession: (sessionId: string, label: string) => Promise<void>
  readonly forkSession: (message: ChatMessage, label?: string) => Promise<void>
  readonly removeSession: (sessionId: string) => Promise<void>
  readonly mergeMessage: (message: ChatMessage) => void
  readonly captureSelection: () => ChatSelection | undefined
  readonly isCurrentSelection: (selection: ChatSelection) => boolean
}

export function useChatSession(options: ChatSessionOptions): ChatSessionState {
  const route = useRoute()
  const router = useRouter()
  const connection = useConnectionStore()
  const pageLatest = useLatest()
  const pageToken = pageLatest.begin()
  const sessionsLatest = useLatest()
  const historyLatest = useLatest()
  const subscription = useLatestSubscription()
  const sessions = ref<readonly ChatSession[]>([])
  const messages = ref<readonly ChatMessage[]>([])
  const selectedId = ref<string>()
  const sessionsLoading = ref(true)
  const sessionsLoaded = ref(false)
  const loading = ref(false)
  const loadingOlder = ref(false)
  const hasOlder = ref(false)
  const streamingMessageIds = ref<ReadonlySet<string>>(new Set())
  let selectedToken: LatestToken | undefined

  const live = computed(() => connection.state === 'authenticated' && chatMethods.every((method) => connection.methods.has(method)))
  const showingPlatformLogs = computed(() => route.query['view'] === 'logs')
  const selectedSession = computed(() => sessions.value.find(({ sessionId }) => sessionId === selectedId.value))

  function mergeMessage(message: ChatMessage): void {
    messages.value = mergeChatMessage(messages.value, message)
  }

  function finishMessage(messageId: string): void {
    const next = new Set(streamingMessageIds.value)
    next.delete(messageId)
    streamingMessageIds.value = next
  }

  function captureSelection(): ChatSelection | undefined {
    return selectedId.value === undefined || selectedToken === undefined
      ? undefined
      : { sessionId: selectedId.value, token: selectedToken }
  }

  function isCurrentSelection(selection: ChatSelection): boolean {
    return subscription.current(selection.token) && selectedId.value === selection.sessionId
  }

  async function refreshHistory(sessionId: string, token: LatestToken): Promise<void> {
    const historyToken = historyLatest.begin()
    const result = await runBackend(loadChatHistory(sessionId))
    if (!historyLatest.current(historyToken) || !subscription.current(token) || selectedId.value !== sessionId) return
    loading.value = false
    if (result._tag === 'Failure') { options.error.value = result.error.message; return }
    options.error.value = ''
    streamingMessageIds.value = new Set()
    messages.value = [...result.value.messages]
    hasOlder.value = result.value.hasOlder
  }

  async function loadOlder(): Promise<void> {
    const sessionId = selectedId.value
    const token = selectedToken
    const oldest = messages.value[0]
    if (sessionId === undefined || token === undefined || oldest === undefined || loadingOlder.value || !hasOlder.value) return
    const historyToken = historyLatest.begin()
    loadingOlder.value = true
    const result = await runBackend(loadChatHistory(sessionId, oldest.messageId))
    if (!subscription.current(token) || selectedId.value !== sessionId) return
    loadingOlder.value = false
    if (!historyLatest.current(historyToken)) return
    if (result._tag === 'Failure') { options.error.value = result.error.message; return }
    const known = new Set(messages.value.map(({ messageId }) => messageId))
    messages.value = [...result.value.messages.filter(({ messageId }) => !known.has(messageId)), ...messages.value]
    hasOlder.value = result.value.hasOlder
  }

  async function selectSession(sessionId: string): Promise<void> {
    if (!pageLatest.current(pageToken) || selectedId.value === sessionId && subscription.owned()) return
    if (!await options.beforeSelectionChange() || !pageLatest.current(pageToken)) return
    options.selectionChanged()
    const token = subscription.begin()
    if (!subscription.current(token)) return
    selectedToken = token
    historyLatest.invalidate()
    selectedId.value = sessionId
    messages.value = []
    hasOlder.value = false
    loadingOlder.value = false
    streamingMessageIds.value = new Set()
    loading.value = true
    await router.replace({ name: 'chat', params: { sessionId } })
    if (!subscription.current(token)) return
    const result = await runBackend(subscribeChat(
      sessionId,
      () => subscription.current(token) ? refreshHistory(sessionId, token) : Promise.resolve(),
      (message) => {
        if (!subscription.current(token) || selectedId.value !== sessionId) return
        mergeMessage(message)
        if (message.sender === 'assistant') streamingMessageIds.value = new Set(streamingMessageIds.value).add(message.messageId)
      },
      (messageId) => { if (subscription.current(token) && selectedId.value === sessionId) finishMessage(messageId) },
    ))
    if (result._tag === 'Success') subscription.own(token, result.value)
    else if (subscription.current(token)) { loading.value = false; options.error.value = result.error.message }
  }

  async function clearSelection(): Promise<void> {
    if (!await options.beforeSelectionChange() || !pageLatest.current(pageToken)) return
    options.selectionChanged()
    subscription.invalidate()
    historyLatest.invalidate()
    selectedToken = undefined
    selectedId.value = undefined
    messages.value = []
    hasOlder.value = false
    loadingOlder.value = false
    await router.replace({ name: 'chat' })
  }

  async function loadSessions(preferredId?: string): Promise<void> {
    const token = sessionsLatest.begin()
    if (!sessionsLatest.current(token)) return
    if (showingPlatformLogs.value) { sessionsLoading.value = false; return }
    if (connection.state === 'opening' || connection.state === 'reconnecting') { sessionsLoading.value = true; return }
    if (!live.value) {
      sessionsLoading.value = false
      options.error.value = connection.state === 'authenticated'
        ? 'The server does not provide every Chat RPC method required by this page.'
        : connection.error || 'Connect to cosmobot to load chat sessions.'
      return
    }
    sessionsLoading.value = true
    const result = await runBackend(listChatSessions)
    if (!sessionsLatest.current(token)) return
    sessionsLoading.value = false
    if (result._tag === 'Failure') { options.error.value = result.error.message; return }
    options.error.value = ''
    sessions.value = [...result.value]
    sessionsLoaded.value = true
    const routeId = typeof route.params['sessionId'] === 'string' ? route.params['sessionId'] : undefined
    const requestedId = preferredId ?? routeId
    const nextId = requestedId !== undefined && result.value.some(({ sessionId }) => sessionId === requestedId)
      ? requestedId
      : result.value[0]?.sessionId
    if (nextId !== undefined) await selectSession(nextId)
    else await clearSelection()
  }

  async function showChatView(view: 'sessions' | 'logs'): Promise<void> {
    if (view === 'logs') await router.push({ name: 'chat', query: { view: 'logs' } })
    else {
      await router.push({ name: 'chat', params: selectedId.value === undefined ? {} : { sessionId: selectedId.value } })
      await loadSessions(selectedId.value)
    }
  }

  async function createSession(): Promise<void> {
    const result = await runBackend(openChatSession('New session'))
    if (!pageLatest.current(pageToken)) return
    if (result._tag === 'Failure') { options.error.value = result.error.message; return }
    await loadSessions(result.value.sessionId)
  }

  async function renameSession(sessionId: string, label: string): Promise<void> {
    const result = await runBackend(renameChatSession(sessionId, label))
    if (!pageLatest.current(pageToken)) return
    if (result._tag === 'Failure') { options.error.value = result.error.message; return }
    sessions.value = sessions.value.map((item) => item.sessionId === result.value.sessionId ? result.value : item)
  }

  async function forkSession(message: ChatMessage, label?: string): Promise<void> {
    const result = await runBackend(forkChatSession(message.sessionId, message.messageId, label))
    if (!pageLatest.current(pageToken)) return
    if (result._tag === 'Failure') { options.error.value = result.error.message; return }
    await loadSessions(result.value.sessionId)
  }

  async function removeSession(sessionId: string): Promise<void> {
    const result = await runBackend(deleteChatSession(sessionId))
    if (!pageLatest.current(pageToken)) return
    if (result._tag === 'Failure') { options.error.value = result.error.message; return }
    if (!result.value) { options.error.value = 'The session no longer exists.'; return }
    options.sessionDeleted(sessionId)
    await loadSessions()
  }

  watch([() => connection.state, () => connection.methods], ([state]) => {
    void loadSessions()
    if (state === 'offline' || state === 'failed') {
      options.selectionInvalidated()
      subscription.invalidate()
      historyLatest.invalidate()
      selectedToken = undefined
      selectedId.value = undefined
      sessions.value = []
      messages.value = []
      hasOlder.value = false
      loadingOlder.value = false
      sessionsLoaded.value = false
    }
  })
  watch(() => route.params['sessionId'], (sessionId) => {
    if (typeof sessionId === 'string' && sessionId !== selectedId.value && sessions.value.some((session) => session.sessionId === sessionId)) void selectSession(sessionId)
  })
  onMounted(() => { void loadSessions() })

  return {
    sessions, messages, selectedId, selectedSession, sessionsLoading, sessionsLoaded, loading, loadingOlder, hasOlder,
    streamingMessageIds, live, showingPlatformLogs, showChatView, loadSessions, selectSession, loadOlder, createSession,
    renameSession, forkSession, removeSession, mergeMessage, captureSelection, isCurrentSelection,
  }
}
