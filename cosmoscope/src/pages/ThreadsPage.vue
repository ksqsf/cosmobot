<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, ref, useTemplateRef, watch } from 'vue'
import { refDebounced } from '@vueuse/core'
import { useRoute, useRouter, type RouteLocationRaw } from 'vue-router'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
import ContextMenu, { type ContextMenuMethods } from 'primevue/contextmenu'
import DataTable from 'primevue/datatable'
import type { DataTablePageEvent } from 'primevue/datatable'
import Dialog from 'primevue/dialog'
import Drawer from 'primevue/drawer'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import Select from 'primevue/select'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import Tree from 'primevue/tree'
import type { TreeNode as PrimeTreeNode } from 'primevue/treenode'
import type { MenuItem } from 'primevue/menuitem'
import PageHeading from '@/components/PageHeading.vue'
import ChatLogMessageLink from '@/components/ChatLogMessageLink.vue'
import DisplayIdentity from '@/components/DisplayIdentity.vue'
import MessageContent from '@/components/MessageContent.vue'
import type { MessageContentAttachment } from '@/components/messageContent'
import PlatformIcon from '@/components/PlatformIcon.vue'
import RunIdLink from '@/components/RunIdLink.vue'
import { getMedia, getRunAudit, getThread, getThreadAudit, haltActiveThread, listActiveThreads, listThreads, resolveThreadRun, subscribeAudit } from '@/backend/AdminBackend'
import { mergeAuditRecords } from '@/backend/audit'
import { runBackend } from '@/backend/runBackend'
import { safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { threadMessageChatKey, threadMessageKeyId, threadPathTo, threadToolActivity, type ThreadToolActivity } from '@/backend/thread'
import { auditRecordsLinkedTo, threadStats } from '@/backend/threadStats'
import { formatBytes } from '@/format'
import { highlightCode, mediaRefsInText } from '@/markdown'
import { useConnectionStore } from '@/stores/connection'
import { useLayeredConfirm, useOverlayLayer } from '@/overlay'
import type { ActiveThread, AuditPlatform, AuditRecord, MediaDetail, StoredThreadMessage, ThreadDetail, ThreadMessageKey, ThreadNode, ThreadSummary } from '@/types/domain'

interface ThreadTranscriptEntry {
  readonly node: ThreadNode
  readonly message: StoredThreadMessage
  readonly messageIndex: number
}

const threads = ref<ThreadSummary[]>([])
const activeThreads = ref<ActiveThread[]>([])
const detail = ref<ThreadDetail>()
const auditRecords = ref<AuditRecord[]>([])
const selectedNode = ref<ThreadNode>()
const selectedKeys = ref<Record<string, boolean>>({})
const expandedKeys = ref<Record<string, boolean>>({})
const query = ref('')
const debouncedQuery = refDebounced(query, 250)
const platform = ref<AuditPlatform | 'all'>('all')
const first = ref(0)
const rows = ref(25)
const total = ref(0)
const nodeTotal = ref(0)
const leafTotal = ref(0)
const platformTotal = ref(0)
const error = ref('')
const detailError = ref('')
const statsError = ref('')
const loading = ref(true)
const tableLoading = ref(false)
const loaded = ref(false)
const detailLoading = ref(false)
const activeError = ref('')
const activeAuditError = ref('')
const activeTaskId = ref<number>()
const activeSnapshot = ref<ActiveThread>()
const activeAuditRecords = ref<AuditRecord[]>([])
const haltingTaskId = ref<number>()
const visible = ref(false)
const treeFocused = ref(false)
const treeZoom = ref(100)
const mediaByRef = ref<ReadonlyMap<string, MediaDetail>>(new Map())
const previewImage = ref<string>()
const transcriptList = ref<HTMLOListElement>()
const transcriptMessageMenu = useTemplateRef<ContextMenuMethods>('transcriptMessageMenu')
const contextTranscriptEntry = ref<ThreadTranscriptEntry>()
const route = useRoute()
const router = useRouter()
const confirm = useLayeredConfirm()
const toast = useToast()
const connection = useConnectionStore()
let detailGeneration = 0
let activeGeneration = 0
let stopActiveAuditSubscription: (() => void) | undefined
let listGeneration = 0
let monitorTimer: number | undefined
let activePolling = false
let finalizingRunId: string | undefined
const { isTop: detailIsTop } = useOverlayLayer(visible)
const { isTop: previewIsTop } = useOverlayLayer(computed(() => previewImage.value !== undefined))
const transcriptMenuLayer = useOverlayLayer()

const platformNames = {
  PlatformQQ: 'QQ',
  PlatformTelegram: 'Telegram',
  PlatformMatrix: 'Matrix',
  PlatformDiscord: 'Discord',
  PlatformRPC: 'RPC',
  PlatformACP: 'ACP',
} satisfies Record<AuditPlatform, string>
const allPlatforms: readonly AuditPlatform[] = ['PlatformQQ', 'PlatformTelegram', 'PlatformMatrix', 'PlatformDiscord', 'PlatformRPC', 'PlatformACP']
const platformOptions = computed(() => [
  { label: 'All platforms', value: 'all' as const },
  ...allPlatforms.map((value) => ({ label: platformNames[value], value })),
])
const summary = computed(() => ({
  threads: total.value,
  nodes: nodeTotal.value,
  leaves: leafTotal.value,
  platforms: platformTotal.value,
}))
const activeSelected = computed(() => activeThreads.value.find(({ taskId }) => taskId === activeTaskId.value) ?? activeSnapshot.value)
const nodeLookup = computed(() => new Map((detail.value?.nodes ?? []).map((node) => [threadMessageKeyId(node.messageKey), node])))
const inspectedActiveThreads = computed(() => detail.value === undefined
  ? activeSelected.value === undefined ? [] : [activeSelected.value]
  : activeThreads.value.filter(({ parentThreadId }) => parentThreadId === detail.value?.summary.threadId))
const treeNodes = computed<PrimeTreeNode[]>(() => buildActiveTree(detail.value?.nodes ?? [], inspectedActiveThreads.value))
const selectedPath = computed(() => selectedNode.value === undefined ? [] : threadPathTo(selectedNode.value, nodeLookup.value))
const transcript = computed<ThreadTranscriptEntry[]>(() => selectedPath.value.flatMap((node) =>
  node.messages.map((message, messageIndex) => ({ node, message, messageIndex })),
))
const activeStats = computed(() => threadStats(activeAuditRecords.value))
const activeTools = computed(() => threadToolActivity(activeAuditRecords.value))
const activeTranscriptMessages = computed<StoredThreadMessage[]>(() => [...(activeSelected.value?.messages ?? [])])
const stats = computed(() => threadStats(auditRecordsLinkedTo(auditRecords.value, selectedPath.value.map(({ messageKey }) => messageKey))))
const inspectorStats = computed(() => activeSelected.value === undefined ? stats.value : activeStats.value)
const transcriptMenuItems = computed<MenuItem[]>(() => {
  const entry = contextTranscriptEntry.value
  if (entry === undefined) return []
  const messageKey = transcriptMessageKey(entry)
  return [
    { label: 'Copy text', icon: 'pi pi-copy', disabled: readableMessageText(entry.message) === '', command: () => { void copyTranscriptText(entry.message) } },
    ...(messageKey === undefined ? [] : [
      { separator: true },
      { label: 'Open in chat logs', icon: 'pi pi-comments', command: () => { void openChatLogMessage(messageKey) } },
      { label: 'Copy message link', icon: 'pi pi-link', command: () => { void copyChatLogLink(messageKey) } },
    ]),
  ]
})

async function refresh(): Promise<void> {
  if (connection.state === 'opening' || connection.state === 'reconnecting') {
    if (loaded.value) tableLoading.value = true
    else loading.value = true
    return
  }
  if (connection.state !== 'authenticated') {
    loading.value = false
    tableLoading.value = false
    error.value = connection.error || 'Connect to cosmobot to load threads.'
    return
  }
  const generation = ++listGeneration
  if (loaded.value) tableLoading.value = true
  else loading.value = true
  const result = await runBackend(listThreads({
    offset: first.value,
    limit: rows.value,
    ...(debouncedQuery.value.trim() === '' ? {} : { query: debouncedQuery.value.trim() }),
    ...(platform.value === 'all' ? {} : { platform: platform.value }),
  }))
  if (generation !== listGeneration) return
  loading.value = false
  tableLoading.value = false
  if (result._tag === 'Failure') {
    error.value = result.error.message
    return
  }
  threads.value = [...result.value.threads]
  total.value = result.value.total
  nodeTotal.value = result.value.nodes
  leafTotal.value = result.value.leaves
  platformTotal.value = result.value.platforms
  loaded.value = true
  error.value = ''
  await selectFromRoute()
}

function changePage(event: DataTablePageEvent): void {
  first.value = event.first
  rows.value = event.rows
  void refresh()
}

async function refreshActive(): Promise<void> {
  if (connection.state !== 'authenticated') { activeThreads.value = []; return }
  const generation = ++activeGeneration
  const monitored = activeSelected.value
  const keepTranscriptPinned = activeTranscriptIsPinned()
  const result = await runBackend(listActiveThreads)
  if (generation !== activeGeneration) return
  if (result._tag === 'Failure') { activeError.value = result.error.message; return }
  activeError.value = ''
  activeThreads.value = [...result.value]
  const selected = result.value.find(({ taskId }) => taskId === activeTaskId.value)
  if (selected !== undefined) {
    activeSnapshot.value = selected
    await Promise.all([
      stopActiveAuditSubscription === undefined ? refreshActiveAudit(selected.runId, generation) : Promise.resolve(),
      loadMediaForMessages(selected.messages),
    ])
    if (generation === activeGeneration && keepTranscriptPinned) await scrollTranscriptToEnd()
  } else if (monitored !== undefined && visible.value) {
    await openFinalizedThread(monitored)
  }
}

async function refreshActiveAudit(runId: string, generation: number): Promise<void> {
  const result = await runBackend(getRunAudit(runId))
  if (generation !== activeGeneration || activeSelected.value?.runId !== runId) return
  if (result._tag === 'Failure') { activeAuditError.value = result.error.message; return }
  activeAuditError.value = ''
  activeAuditRecords.value = [...result.value]
  await loadMediaForMessages(result.value.flatMap(({ event }) => event.tag === 'ToolCallFinished'
    ? [{ role: 'tool', content: event.result } satisfies StoredThreadMessage]
    : []))
}

async function openFinalizedThread(active: ActiveThread): Promise<void> {
  if (finalizingRunId === active.runId) return
  finalizingRunId = active.runId
  try {
    const result = await runBackend(resolveThreadRun(active.runId))
    if (result._tag === 'Failure') { activeAuditError.value = result.error.message; return }
    if (result.value.threadId === null) return
    activeTaskId.value = undefined
    activeSnapshot.value = undefined
    activeAuditRecords.value = []
    await router.replace({ name: 'threads', params: { threadId: String(result.value.threadId) } })
  } finally {
    finalizingRunId = undefined
  }
}

function stopActivePolling(): void {
  if (monitorTimer !== undefined) window.clearTimeout(monitorTimer)
  monitorTimer = undefined
}

function scheduleActivePolling(): void {
  stopActivePolling()
  if (!activePolling || document.hidden || connection.state !== 'authenticated') return
  monitorTimer = window.setTimeout(() => { void pollActiveThreads() }, 1000)
}

async function pollActiveThreads(): Promise<void> {
  await refreshActive()
  scheduleActivePolling()
}

function activeVisibilityChanged(): void {
  if (document.hidden) stopActivePolling()
  else void pollActiveThreads()
}

async function monitor(active: ActiveThread): Promise<void> {
  if (active.parentThreadId === null) {
    clearActiveSelection()
    detail.value = undefined
    detailError.value = ''
    detailLoading.value = false
    visible.value = true
  } else if (detail.value?.summary.threadId !== active.parentThreadId) {
    await inspectThread(active.parentThreadId)
  }
  activeTaskId.value = active.taskId
  activeSnapshot.value = active
  activeAuditRecords.value = []
  activeAuditError.value = ''
  selectedNode.value = undefined
  selectedKeys.value = { [activeTreeKey(active.taskId)]: true }
  void installActiveAuditSubscription()
  void loadMediaForMessages(active.messages)
  void scrollTranscriptToEnd()
}

async function installActiveAuditSubscription(): Promise<void> {
  stopActiveAuditSubscription?.()
  stopActiveAuditSubscription = undefined
  const result = await runBackend(subscribeAudit(
    async () => {
      const active = activeSelected.value
      if (active !== undefined) await refreshActiveAudit(active.runId, activeGeneration)
    },
    (record) => {
      if (record.event.runId !== activeSelected.value?.runId) return
      activeAuditRecords.value = mergeAuditRecords(activeAuditRecords.value, [record], 2_000)
    },
  ))
  if (result._tag === 'Success' && visible.value) stopActiveAuditSubscription = result.value
  else if (result._tag === 'Success') result.value()
}

function requestHalt(active: ActiveThread): void {
  confirm.require({
    header: `Halt task #${String(active.taskId)}?`,
    message: 'Cancel this active agent thread and persist the transcript produced so far.',
    rejectLabel: 'Keep running',
    acceptLabel: 'Halt thread',
    acceptClass: 'p-button-danger',
    accept: () => { void halt(active) },
  })
}

async function halt(active: ActiveThread): Promise<void> {
  haltingTaskId.value = active.taskId
  const result = await runBackend(haltActiveThread(active.taskId))
  haltingTaskId.value = undefined
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  toast.add({ severity: result.value ? 'success' : 'warn', summary: result.value ? 'Thread halted' : 'Thread was no longer active', life: 2500 })
  await Promise.all([refreshActive(), refresh()])
}

async function selectFromRoute(): Promise<void> {
  const raw = route.params['threadId']
  if (typeof raw === 'string') {
    const threadId = Number(raw)
    if (!Number.isSafeInteger(threadId) || threadId < 1) { error.value = 'The thread ID is invalid.'; return }
    await inspectThread(threadId)
    return
  }
  const runId = route.query['run']
  if (typeof runId === 'string' && runId !== '') await inspectRun(runId)
}

async function inspectRun(runId: string): Promise<void> {
  const result = await runBackend(resolveThreadRun(runId))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  if (result.value.taskId !== null) {
    await refreshActive()
    const active = activeThreads.value.find(({ taskId }) => taskId === result.value.taskId)
    if (active === undefined) { error.value = `Agent run ${runId} is no longer active.`; return }
    error.value = ''
    await monitor(active)
    return
  }
  if (result.value.threadId !== null) {
    await router.replace({ name: 'threads', params: { threadId: String(result.value.threadId) } })
    return
  }
  error.value = `No agent thread is linked to run ${runId}.`
}

function inspect(thread: ThreadSummary): void {
  void router.replace({ name: 'threads', params: { threadId: String(thread.threadId) } })
}

function viewThreadAudit(threadId: number): void {
  void router.push({ name: 'audit', query: { thread: String(threadId) } })
}

function viewRunAudit(runId: string): void {
  void router.push({ name: 'audit', query: { run: runId } })
}

async function inspectThread(threadId: number): Promise<void> {
  clearActiveSelection()
  const generation = ++detailGeneration
  visible.value = true
  detailLoading.value = true
  detailError.value = ''
  statsError.value = ''
  detail.value = undefined
  auditRecords.value = []
  selectedNode.value = undefined
  treeFocused.value = false
  treeZoom.value = 100
  mediaByRef.value = new Map()
  const result = await runBackend(getThread(threadId))
  if (generation !== detailGeneration) return
  detailLoading.value = false
  if (result._tag === 'Failure') { detailError.value = result.error.message; return }
  if (result.value === null) { detailError.value = `Thread #${String(threadId)} was not found.`; return }
  detail.value = result.value
  expandedKeys.value = Object.fromEntries(result.value.nodes.map((node) => [threadMessageKeyId(node.messageKey), true]))
  const latest = nodeLookup.value.get(threadMessageKeyId(result.value.summary.latestKey)) ?? result.value.nodes.at(-1)
  if (latest !== undefined) selectNode(latest)
  await Promise.all([loadThreadMedia(result.value, generation), loadThreadStats(result.value, generation)])
}

async function loadThreadStats(thread: ThreadDetail, generation: number): Promise<void> {
  const result = await runBackend(getThreadAudit(thread.summary.threadId))
  if (generation !== detailGeneration) return
  if (result._tag === 'Failure') { statsError.value = result.error.message; return }
  if (result.value === null) { statsError.value = `Thread #${String(thread.summary.threadId)} was not found.`; return }
  auditRecords.value = [...result.value]
}

async function loadThreadMedia(thread: ThreadDetail, generation: number): Promise<void> {
  await loadMediaForMessages(thread.nodes.flatMap(({ messages }) => messages), generation)
}

async function loadMediaForMessages(messages: readonly StoredThreadMessage[], generation?: number): Promise<void> {
  const refs = [...new Set(messages.flatMap(mediaRefs))].filter((ref) => !mediaByRef.value.has(ref))
  if (refs.length === 0) return
  const results = await Promise.all(refs.map(async (ref) => [ref, await runBackend(getMedia(ref))] as const))
  if (generation !== undefined && generation !== detailGeneration) return
  mediaByRef.value = new Map([...mediaByRef.value, ...results.flatMap(([ref, result]) => result._tag === 'Success' ? [[ref, result.value] as const] : [])])
}

function closeDrawer(): void {
  detailGeneration += 1
  clearActiveSelection()
  visible.value = false
  detail.value = undefined
  selectedNode.value = undefined
  treeFocused.value = false
  if (route.params['threadId'] !== undefined) void router.replace({ name: 'threads' })
}

function clearActiveSelection(): void {
  stopActiveAuditSubscription?.()
  stopActiveAuditSubscription = undefined
  activeTaskId.value = undefined
  activeSnapshot.value = undefined
  activeAuditRecords.value = []
}

function activeTranscriptIsPinned(): boolean {
  const list = transcriptList.value
  return list === undefined || list.scrollHeight - list.scrollTop - list.clientHeight < 80
}

async function scrollTranscriptToEnd(): Promise<void> {
  await nextTick()
  const list = transcriptList.value
  if (list !== undefined) list.scrollTop = list.scrollHeight
}

function selectTreeNode(node: PrimeTreeNode): void {
  const active = activeThreads.value.find(({ taskId }) => activeTreeKey(taskId) === node.key)
  if (active !== undefined) { void monitor(active); return }
  clearActiveSelection()
  const selected = nodeLookup.value.get(node.key)
  if (selected !== undefined) selectNode(selected)
}

function selectNode(node: ThreadNode): void {
  selectedNode.value = node
  selectedKeys.value = { [threadMessageKeyId(node.messageKey)]: true }
  void nextTick(() => {
    const list = transcriptList.value
    if (list !== undefined) list.scrollTop = list.scrollHeight
  })
}

function transcriptMessageKey(entry: ThreadTranscriptEntry): ThreadMessageKey | undefined {
  return threadMessageChatKey(entry.node, entry.messageIndex)
}

function chatLogLocation(messageKey: ThreadMessageKey): RouteLocationRaw {
  return {
    name: 'chat' as const,
    query: {
      view: 'logs',
      platform: messageKey.platform,
      ...(messageKey.chatId === null ? {} : { chat: messageKey.chatId }),
      message: messageKey.messageId,
    },
  }
}

function showTranscriptMenu(event: Event, entry: ThreadTranscriptEntry): void {
  contextTranscriptEntry.value = entry
  transcriptMessageMenu.value?.show(event)
}

async function openChatLogMessage(messageKey: ThreadMessageKey): Promise<void> {
  await router.push(chatLogLocation(messageKey))
}

function openTranscriptChat(entry: ThreadTranscriptEntry): void {
  const messageKey = transcriptMessageKey(entry)
  if (messageKey !== undefined) void openChatLogMessage(messageKey)
}

async function copyTranscriptText(message: StoredThreadMessage): Promise<void> {
  try { await navigator.clipboard.writeText(readableMessageText(message)) } catch {
    toast.add({ severity: 'error', summary: 'Could not copy the message text.', life: 3000 })
  }
}

async function copyChatLogLink(messageKey: ThreadMessageKey): Promise<void> {
  const href = router.resolve(chatLogLocation(messageKey)).href
  try { await navigator.clipboard.writeText(new URL(href, globalThis.location.href).href) } catch {
    toast.add({ severity: 'error', summary: 'Could not copy the message link.', life: 3000 })
  }
}

function buildTree(nodes: readonly ThreadNode[]): PrimeTreeNode[] {
  const byKey = new Map(nodes.map((node) => [threadMessageKeyId(node.messageKey), {
    key: threadMessageKeyId(node.messageKey),
    label: nodeLabel(node),
    icon: node.parentMessageKey === null ? 'pi pi-comments' : 'pi pi-reply',
    children: [] as PrimeTreeNode[],
  } satisfies PrimeTreeNode]))
  const roots: PrimeTreeNode[] = []
  for (const node of nodes) {
    const item = byKey.get(threadMessageKeyId(node.messageKey))
    if (item === undefined) continue
    const parent = node.parentMessageKey === null ? undefined : byKey.get(threadMessageKeyId(node.parentMessageKey))
    if (parent === undefined) roots.push(item)
    else parent.children.push(item)
  }
  return roots.length === 0 ? [...byKey.values()] : roots
}

function activeTreeKey(taskId: number): string {
  return `active:${String(taskId)}`
}

function buildActiveTree(nodes: readonly ThreadNode[], activeThreads: readonly ActiveThread[]): PrimeTreeNode[] {
  const roots = buildTree(nodes)
  for (const active of activeThreads) {
    const running: PrimeTreeNode = {
      key: activeTreeKey(active.taskId),
      label: active.prompt || 'Active thread',
      icon: 'pi pi-spinner pi-spin',
      styleClass: 'thread-tree-active',
      children: [],
    }
    const parentKey = active.parentMessageKey === null ? undefined : threadMessageKeyId(active.parentMessageKey)
    const parent = parentKey === undefined ? undefined : findTreeNode(roots, parentKey)
    if (parent === undefined) roots.push(running)
    else (parent.children ??= []).push(running)
  }
  return roots
}

function findTreeNode(nodes: readonly PrimeTreeNode[], key: string): PrimeTreeNode | undefined {
  for (const node of nodes) {
    if (node.key === key) return node
    const found = findTreeNode(node.children ?? [], key)
    if (found !== undefined) return found
  }
  return undefined
}

function toolStatusSeverity(status: string): 'success' | 'info' | 'danger' {
  if (status === 'running') return 'info'
  return /^(?:ok|success|succeeded)$/i.test(status) ? 'success' : 'danger'
}

function toolResultMessage(tool: ThreadToolActivity): StoredThreadMessage {
  return { role: 'tool', content: tool.result ?? '' }
}

function nodeLabel(node: ThreadNode): string {
  const visibleMessages = node.messages.filter(({ role }) => role !== 'synthetic')
  const preferred = [...visibleMessages].reverse().find((message) => message.role === 'user' && readableMessageText(message) !== '')
    ?? [...visibleMessages].reverse().find((message) => readableMessageText(message) !== '')
  if (preferred === undefined) return node.messageKey.messageId
  const text = readableMessageText(preferred)
  return text.length > 64 ? `${text.slice(0, 61)}…` : text
}

function readableMessageText(message: StoredThreadMessage): string {
  return messageText(message).replace(/\s+/g, ' ').trim()
}

function messageText(message: StoredThreadMessage): string {
  const { content } = message
  if (typeof content === 'string') return content
  if (content === undefined || content === null) return message.tool_calls?.map((call) => `Calls ${call.function.name}`).join('\n') ?? '(No text content)'
  return content.map((part) => {
    if (part.text !== undefined) return part.text
    const image = typeof part.image_url === 'string' ? part.image_url : part.image_url?.url
    return image === undefined ? `[${part.type}]` : `[Image] ${image}`
  }).join('\n')
}

function mediaRefs(message: StoredThreadMessage): string[] {
  const text = [messageText(message), ...(message.tool_calls?.map((call) => call.function.arguments) ?? [])].join('\n')
  const contentRefs = typeof message.content === 'object' && message.content !== null
    ? message.content.flatMap((part) => {
        const image = typeof part.image_url === 'string' ? part.image_url : part.image_url?.url
        return image?.startsWith('media:mf_') === true ? [image] : []
      })
    : []
  return [...new Set([...contentRefs, ...mediaRefsInText(text)])]
}

function mediaDetails(message: StoredThreadMessage): MediaDetail[] {
  return mediaRefs(message).flatMap((ref) => {
    const media = mediaByRef.value.get(ref)
    return media === undefined ? [] : [media]
  })
}

function mediaUrl(media: MediaDetail): string | undefined {
  return safeDownloadUrl(media.publicUrl, window.location.href)
}

function imageUrls(message: StoredThreadMessage): string[] {
  const contentUrls = Array.isArray(message.content) ? message.content.flatMap((part) => {
    const ref = typeof part.image_url === 'string' ? part.image_url : part.image_url?.url
    return ref === undefined ? [] : [mediaByRef.value.get(ref)?.publicUrl ?? ref]
  }) : []
  const mediaUrls = mediaDetails(message)
    .filter(({ exists, mimeType }) => exists && mimeType.startsWith('image/'))
    .map(({ publicUrl }) => publicUrl)
  return [...new Set([...contentUrls, ...mediaUrls].flatMap((resolved) => {
    const safe = safeImageUrl(resolved, window.location.href)
    return safe === undefined ? [] : [safe]
  }))]
}

function messageAttachments(message: StoredThreadMessage): MessageContentAttachment[] {
  return mediaDetails(message)
    .filter(({ mimeType }) => !mimeType.startsWith('image/'))
    .map((media) => {
      const url = media.exists ? mediaUrl(media) : undefined
      return {
        key: media.mediaId,
        name: media.sourceName ?? media.mediaId,
        detail: `${media.mimeType} · ${formatBytes(media.size)}`,
        mimeType: media.mimeType,
        mediaId: media.mediaId,
        ...(url === undefined ? {} : { url }),
      }
    })
}

function roleLabel(role: string): string {
  return ({ user: 'User', assistant: 'Assistant', system: 'System', tool: 'Tool', synthetic: 'Synthetic' } as Readonly<Record<string, string>>)[role] ?? role
}

function formatDuration(milliseconds: number, unreported = 0): string {
  const value = milliseconds < 1000 ? `${String(milliseconds)} ms` : `${(milliseconds / 1000).toFixed(1)} s`
  return unreported === 0 ? value : `${value} · ${String(unreported)} unreported`
}

function threadPlatformName(thread: ThreadSummary): string {
  return platformNames[thread.rootKey.platform]
}

onMounted(() => {
  activePolling = true
  void refresh()
  void pollActiveThreads()
  document.addEventListener('visibilitychange', activeVisibilityChanged)
})
onUnmounted(() => { activePolling = false; stopActivePolling(); stopActiveAuditSubscription?.(); document.removeEventListener('visibilitychange', activeVisibilityChanged); activeGeneration += 1; detailGeneration += 1; listGeneration += 1 })
watch([() => connection.state, () => connection.methods], () => { void refresh(); void pollActiveThreads() })
watch([() => route.params['threadId'], () => route.query['run']], () => { void selectFromRoute() })
watch([debouncedQuery, platform], () => { first.value = 0; void refresh() })
</script>

<template>
  <section class="page">
    <ContextMenu
      ref="transcriptMessageMenu"
      :model="transcriptMenuItems"
      @show="transcriptMenuLayer.show"
      @hide="transcriptMenuLayer.hide(); contextTranscriptEntry = undefined"
    />
    <PageHeading
      eyebrow="Conversations"
      title="Threads"
      description="Explore persisted conversation branches and the model context behind each reply."
    >
      <Button
        label="Refresh snapshot"
        icon="pi pi-refresh"
        severity="secondary"
        :loading="loading"
        @click="refresh"
      />
    </PageHeading>
    <Message
      v-if="error"
      severity="error"
      :closable="false"
    >
      {{ error }}
    </Message>
    <article
      v-if="loading"
      class="panel manager-loading"
      aria-label="Loading threads"
    >
      <Skeleton
        v-for="index in 6"
        :key="index"
        height="3rem"
      />
    </article>
    <template v-else-if="loaded">
      <div
        class="manager-summary"
        aria-label="Thread summary"
      >
        <div><span class="summary-mark violet"><i class="pi pi-sitemap" /></span><span><strong>{{ summary.threads }}</strong><small>Threads</small></span></div>
        <div><span class="summary-mark info"><i class="pi pi-comments" /></span><span><strong>{{ summary.nodes }}</strong><small>Reply nodes</small></span></div>
        <div><span class="summary-mark success"><i class="pi pi-share-alt" /></span><span><strong>{{ summary.leaves }}</strong><small>Branch tips</small></span></div>
        <div><span class="summary-mark neutral"><i class="pi pi-globe" /></span><span><strong>{{ summary.platforms }}</strong><small>Platforms</small></span></div>
      </div>
      <article class="panel active-thread-panel">
        <header class="stream-heading">
          <span><i class="pi pi-circle-fill active-thread-pulse" />Active threads</span><small>Live · refreshes every second</small>
        </header>
        <Message
          v-if="activeError"
          severity="error"
          :closable="false"
        >
          {{ activeError }}
        </Message>
        <p
          v-else-if="activeThreads.length === 0"
          class="active-thread-empty"
        >
          No agent threads are currently running.
        </p>
        <ul
          v-else
          class="active-thread-list"
        >
          <li
            v-for="active in activeThreads"
            :key="active.taskId"
          >
            <span class="manager-type-icon"><i class="pi pi-sparkles" /></span>
            <span><strong>{{ active.prompt || 'Untitled prompt' }}</strong><small>Task #{{ active.taskId }} · Run <RunIdLink :run-id="active.runId" /> · {{ active.messages.length }} messages<span v-if="active.pendingSteers"> · {{ active.pendingSteers }} pending steer</span></small></span>
            <Button
              label="Open thread"
              icon="pi pi-eye"
              severity="secondary"
              size="small"
              @click="monitor(active)"
            />
            <Button
              label="Halt"
              icon="pi pi-stop-circle"
              severity="danger"
              size="small"
              :loading="haltingTaskId === active.taskId"
              @click="requestHalt(active)"
            />
          </li>
        </ul>
      </article>
      <article class="panel manager-table">
        <div class="table-toolbar thread-toolbar">
          <InputText
            v-model="query"
            placeholder="Filter by thread, chat, or message"
            aria-label="Filter threads"
          />
          <Select
            v-model="platform"
            :options="platformOptions"
            option-label="label"
            option-value="value"
            aria-label="Filter by platform"
          />
        </div>
        <DataTable
          :value="threads"
          data-key="threadId"
          selection-mode="single"
          lazy
          paginator
          :first="first"
          :rows="rows"
          :total-records="total"
          :loading="tableLoading"
          :rows-per-page-options="[25, 50, 100, 200]"
          @page="changePage"
          @row-select="inspect($event.data)"
        >
          <Column
            field="threadId"
            header="Thread"
          >
            <template #body="{ data }">
              <span class="manager-identity"><span class="manager-type-icon"><PlatformIcon :platform="data.rootKey.platform" /></span><span><strong>#{{ data.threadId }}</strong><small>{{ threadPlatformName(data) }}</small></span></span>
            </template>
          </Column>
          <Column header="Chat">
            <template #body="{ data }">
              <DisplayIdentity
                :id="data.rootKey.chatId"
                :name="data.chatDisplayName"
                unknown="Direct / unscoped"
              />
            </template>
          </Column>
          <Column header="Latest message">
            <template #body="{ data }">
              <span class="thread-message-id">{{ data.latestPreview || 'No text content' }}</span>
            </template>
          </Column>
          <Column
            field="nodeCount"
            header="Nodes"
          />
          <Column header="Shape">
            <template #body="{ data }">
              <Tag
                :value="data.leafCount === 1 ? 'Linear' : `${data.leafCount} branches`"
                :severity="data.leafCount === 1 ? 'secondary' : 'info'"
              />
            </template>
          </Column>
        </DataTable>
      </article>
    </template>
    <Drawer
      v-model:visible="visible"
      header="Thread inspector"
      position="right"
      :style="{ width: 'min(1100px, 100vw)' }"
      :close-on-escape="detailIsTop"
      @hide="closeDrawer"
    >
      <div
        v-if="detailLoading"
        class="manager-loading"
      >
        <Skeleton height="5rem" /><Skeleton height="18rem" />
      </div>
      <Message
        v-else-if="detailError"
        severity="error"
        :closable="false"
      >
        {{ detailError }}
      </Message>
      <div
        v-else-if="detail || activeSelected"
        class="thread-inspector"
      >
        <header
          v-if="detail"
          class="drawer-hero"
        >
          <span class="platform-icon"><PlatformIcon :platform="detail.summary.rootKey.platform" /></span><div>
            <small>{{ platformNames[detail.summary.rootKey.platform] }} thread</small><h2>Thread #{{ detail.summary.threadId }}</h2>
            <div class="tag-list">
              <Tag
                :value="`${detail.summary.nodeCount} nodes`"
                severity="secondary"
              /><Tag
                :value="`${detail.summary.leafCount} branch tips`"
                severity="info"
              /><Tag
                v-if="activeSelected"
                value="Generating"
                severity="success"
              />
            </div>
          </div><Button
            label="View audit"
            icon="pi pi-wave-pulse"
            severity="secondary"
            @click="viewThreadAudit(detail.summary.threadId)"
          />
        </header>
        <header
          v-else-if="activeSelected"
          class="drawer-hero"
        >
          <span class="platform-icon"><i class="pi pi-sparkles" /></span><div>
            <small>Task #{{ activeSelected.taskId }} · live</small><h2>{{ activeSelected.prompt || 'Active thread' }}</h2><Tag
              value="Generating"
              severity="success"
            />
          </div><Button
            label="View audit"
            icon="pi pi-wave-pulse"
            severity="secondary"
            @click="viewRunAudit(activeSelected.runId)"
          />
        </header>
        <dl
          v-if="detail"
          class="detail-list"
        >
          <div>
            <dt>Chat</dt>
            <dd>
              <DisplayIdentity
                :id="detail.summary.rootKey.chatId"
                :name="detail.summary.chatDisplayName"
                unknown="Direct / unscoped"
              />
            </dd>
          </div>
          <div><dt>Root message</dt><dd><ChatLogMessageLink :message-key="detail.summary.rootKey" /></dd></div>
          <div><dt>Latest message</dt><dd><ChatLogMessageLink :message-key="detail.summary.latestKey" /></dd></div>
        </dl>
        <dl
          v-else-if="activeSelected"
          class="detail-list"
        >
          <div><dt>Agent run</dt><dd><RunIdLink :run-id="activeSelected.runId" /></dd></div><div><dt>Linked messages</dt><dd>{{ activeSelected.messageKeys.length }}</dd></div><div><dt>Pending steers</dt><dd>{{ activeSelected.pendingSteers }}</dd></div>
        </dl>
        <Message
          v-if="activeSelected ? activeAuditError : statsError"
          severity="warn"
          :closable="false"
        >
          {{ activeSelected ? activeAuditError : statsError }}
        </Message>
        <section
          v-else
          class="thread-stats"
          aria-label="Thread execution statistics"
        >
          <div><span class="summary-mark violet"><i class="pi pi-chart-bar" /></span><span><strong>{{ inspectorStats.tokens?.total_tokens.toLocaleString() ?? 'Unreported' }}</strong><small v-if="inspectorStats.tokens">Tokens · {{ inspectorStats.tokens.prompt_tokens.toLocaleString() }} prompt / {{ inspectorStats.tokens.completion_tokens.toLocaleString() }} completion</small><small v-else>Token usage was not returned by the model provider</small></span></div>
          <div><span class="summary-mark info"><i class="pi pi-sparkles" /></span><span><strong>{{ formatDuration(inspectorStats.modelMilliseconds, inspectorStats.unreportedModelTurns) }}</strong><small>Model time · {{ inspectorStats.modelTurns }} turns</small></span></div>
          <div><span class="summary-mark warning"><i class="pi pi-wrench" /></span><span><strong>{{ formatDuration(inspectorStats.toolMilliseconds, inspectorStats.unreportedToolCalls) }}</strong><small>Tool time · {{ inspectorStats.toolCalls }} calls / {{ inspectorStats.failedTools }} failed</small></span></div>
          <div><span class="summary-mark success"><i class="pi pi-clock" /></span><span><strong>{{ activeSelected ? activeTools.filter(({ status }) => status === 'running').length : formatDuration(inspectorStats.wallMilliseconds) }}</strong><small>{{ activeSelected ? 'Tools currently running' : `Run wall time · ${String(inspectorStats.runs)} runs` }}</small></span></div>
          <footer>
            <Tag
              :value="`${inspectorStats.contextMessages} peak context messages`"
              severity="secondary"
            /><Tag
              :value="`${inspectorStats.compactions} compactions`"
              severity="secondary"
            /><Tag
              :value="`${inspectorStats.subagents} subagents`"
              severity="secondary"
            /><Tag
              :value="`${activeSelected ? activeAuditRecords.length : auditRecords.length} audit events`"
              severity="secondary"
            />
          </footer>
        </section>
        <div
          class="thread-detail-grid"
          :class="{ 'tree-focused': treeFocused }"
        >
          <section class="thread-tree-panel">
            <header>
              <span>Reply tree</span>
              <span class="thread-tree-tools">
                <Button
                  icon="pi pi-search-minus"
                  text
                  rounded
                  size="small"
                  aria-label="Zoom reply tree out"
                  title="Zoom out"
                  :disabled="treeZoom <= 60"
                  @click="treeZoom -= 10"
                />
                <small>{{ treeZoom }}%</small>
                <Button
                  icon="pi pi-search-plus"
                  text
                  rounded
                  size="small"
                  aria-label="Zoom reply tree in"
                  title="Zoom in"
                  :disabled="treeZoom >= 150"
                  @click="treeZoom += 10"
                />
                <Button
                  :icon="treeFocused ? 'pi pi-window-minimize' : 'pi pi-window-maximize'"
                  text
                  rounded
                  size="small"
                  :aria-label="treeFocused ? 'Exit focused reply tree' : 'Focus reply tree'"
                  :title="treeFocused ? 'Show context' : 'Focus tree'"
                  @click="treeFocused = !treeFocused"
                />
              </span>
            </header>
            <div class="thread-tree-viewport">
              <Tree
                v-model:selection-keys="selectedKeys"
                v-model:expanded-keys="expandedKeys"
                :value="treeNodes"
                :style="{ zoom: treeZoom / 100 }"
                selection-mode="single"
                @node-select="selectTreeNode"
              />
            </div>
          </section>
          <section class="thread-transcript-panel">
            <header>
              <span>{{ activeSelected ? 'Live context' : 'Context at node' }}</span><small><template v-if="activeSelected">{{ activeTranscriptMessages.length }} messages</template><ChatLogMessageLink
                v-else-if="selectedNode"
                :message-key="selectedNode.messageKey"
              /><template v-else>No node selected</template></small>
            </header>
            <div
              v-if="activeSelected && activeTools.length"
              class="active-tool-stream"
            >
              <details
                v-for="tool in activeTools"
                :key="tool.id"
                class="thread-tool-call"
                :open="tool.status === 'running'"
              >
                <summary>
                  <Tag
                    :value="tool.name"
                    severity="secondary"
                  /><span>Turn {{ tool.turn }}</span><Tag
                    :value="tool.status"
                    :severity="toolStatusSeverity(tool.status)"
                  />
                </summary>
                <div class="active-tool-detail">
                  <small>Arguments</small><pre><code
                    class="hljs language-json"
                    :innerHTML="highlightCode(tool.arguments || '{}', 'json')"
                  /></pre>
                  <template v-if="tool.result !== undefined">
                    <small>Result</small><MessageContent
                      :text="tool.result"
                      :images="imageUrls(toolResultMessage(tool))"
                      :attachments="messageAttachments(toolResultMessage(tool))"
                      @preview-image="previewImage = $event"
                    />
                  </template>
                </div>
              </details>
            </div>
            <div
              v-if="activeSelected === undefined && transcript.length === 0"
              class="thread-empty"
            >
              No stored messages for this node.
            </div>
            <ol
              v-if="activeSelected"
              ref="transcriptList"
              class="thread-transcript"
            >
              <li
                v-for="(message, index) in activeTranscriptMessages"
                :key="index"
                :class="`role-${message.role}`"
              >
                <div><strong>{{ roleLabel(message.role) }}</strong><code v-if="message.tool_call_id">{{ message.tool_call_id }}</code></div>
                <MessageContent
                  :text="messageText(message)"
                  :images="imageUrls(message)"
                  :attachments="messageAttachments(message)"
                  @preview-image="previewImage = $event"
                />
                <div
                  v-if="message.tool_calls?.length"
                  class="thread-tool-calls"
                >
                  <details
                    v-for="call in message.tool_calls"
                    :key="call.id"
                    class="thread-tool-call"
                  >
                    <summary>
                      <Tag
                        :value="call.function.name"
                        severity="secondary"
                      /><span>Arguments</span>
                    </summary>
                    <pre><code
                      class="hljs language-json"
                      :innerHTML="highlightCode(call.function.arguments || '{}', 'json')"
                    /></pre>
                  </details>
                </div>
              </li>
            </ol>
            <ol
              v-else-if="transcript.length"
              ref="transcriptList"
              class="thread-transcript"
            >
              <li
                v-for="(entry, index) in transcript"
                :key="index"
                :class="[`role-${entry.message.role}`, { 'context-selected': contextTranscriptEntry === entry }]"
                @contextmenu.prevent="showTranscriptMenu($event, entry)"
              >
                <div>
                  <strong>{{ roleLabel(entry.message.role) }}</strong><span class="thread-message-actions"><code v-if="entry.message.tool_call_id">{{ entry.message.tool_call_id }}</code><Button
                    v-if="transcriptMessageKey(entry)"
                    icon="pi pi-comments"
                    severity="secondary"
                    text
                    rounded
                    size="small"
                    aria-label="Open in chat logs"
                    title="Open in chat logs"
                    @click="openTranscriptChat(entry)"
                  /></span>
                </div>
                <MessageContent
                  :text="messageText(entry.message)"
                  :images="imageUrls(entry.message)"
                  :attachments="messageAttachments(entry.message)"
                  @preview-image="previewImage = $event"
                />
                <div
                  v-if="entry.message.tool_calls?.length"
                  class="thread-tool-calls"
                >
                  <details
                    v-for="call in entry.message.tool_calls"
                    :key="call.id"
                    class="thread-tool-call"
                  >
                    <summary>
                      <Tag
                        :value="call.function.name"
                        severity="secondary"
                      /><span>Arguments</span>
                    </summary>
                    <pre><code
                      class="hljs language-json"
                      :innerHTML="highlightCode(call.function.arguments || '{}', 'json')"
                    /></pre>
                  </details>
                </div>
              </li>
            </ol>
          </section>
        </div>
        <Button
          v-if="activeSelected"
          label="Halt thread"
          icon="pi pi-stop-circle"
          severity="danger"
          :loading="haltingTaskId === activeSelected.taskId"
          @click="requestHalt(activeSelected)"
        />
      </div>
    </Drawer>
    <Dialog
      :visible="previewImage !== undefined"
      modal
      dismissable-mask
      header="Image preview"
      class="image-preview-dialog"
      :close-on-escape="previewIsTop"
      @update:visible="previewImage = undefined"
    >
      <img
        v-if="previewImage"
        :src="previewImage"
        alt="Full-size thread image"
        class="object-preview"
      />
    </Dialog>
  </section>
</template>
