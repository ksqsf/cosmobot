<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { refDebounced } from '@vueuse/core'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import { useConfirm } from 'primevue/useconfirm'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
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
import PageHeading from '@/components/PageHeading.vue'
import ChatLogMessageLink from '@/components/ChatLogMessageLink.vue'
import RunIdLink from '@/components/RunIdLink.vue'
import { getAuditThreadMessages, getMedia, getThread, haltActiveThread, listActiveThreads, listThreads, resolveThreadRun } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { threadStats } from '@/backend/threadStats'
import { formatBytes } from '@/format'
import { highlightCode, mediaRefFromClick, renderMarkdown } from '@/markdown'
import { useConnectionStore } from '@/stores/connection'
import type { ActiveThread, AuditPlatform, AuditRecord, MediaDetail, StoredThreadMessage, ThreadDetail, ThreadMessageKey, ThreadNode, ThreadSummary } from '@/types/domain'

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
const activeVisible = ref(false)
const activeTaskId = ref<number>()
const haltingTaskId = ref<number>()
const visible = ref(false)
const treeFocused = ref(false)
const treeZoom = ref(100)
const mediaByRef = ref<ReadonlyMap<string, MediaDetail>>(new Map())
const previewImage = ref<string>()
const route = useRoute()
const router = useRouter()
const confirm = useConfirm()
const toast = useToast()
const connection = useConnectionStore()
let detailGeneration = 0
let activeGeneration = 0
let listGeneration = 0
let monitorTimer: number | undefined
let activePolling = false

const platformNames = {
  PlatformQQ: 'QQ',
  PlatformTelegram: 'Telegram',
  PlatformMatrix: 'Matrix',
  PlatformDiscord: 'Discord',
  PlatformRPC: 'RPC',
  PlatformACP: 'ACP',
} satisfies Record<AuditPlatform, string>
const platformIcons = {
  PlatformQQ: 'pi pi-comment',
  PlatformTelegram: 'pi pi-send',
  PlatformMatrix: 'pi pi-th-large',
  PlatformDiscord: 'pi pi-discord',
  PlatformRPC: 'pi pi-desktop',
  PlatformACP: 'pi pi-code',
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
const nodeLookup = computed(() => new Map((detail.value?.nodes ?? []).map((node) => [messageKeyId(node.messageKey), node])))
const treeNodes = computed<PrimeTreeNode[]>(() => buildTree(detail.value?.nodes ?? []))
const transcript = computed(() => selectedNode.value === undefined ? [] : transcriptTo(selectedNode.value, nodeLookup.value))
const activeSelected = computed(() => activeThreads.value.find(({ taskId }) => taskId === activeTaskId.value))
const stats = computed(() => threadStats(auditRecords.value))

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
  const result = await runBackend(listActiveThreads)
  if (generation !== activeGeneration) return
  if (result._tag === 'Failure') { activeError.value = result.error.message; return }
  activeError.value = ''
  activeThreads.value = [...result.value]
  if (activeTaskId.value !== undefined && activeSelected.value === undefined) activeVisible.value = false
  await loadMediaForMessages(result.value.flatMap(({ messages }) => messages))
}

function stopActivePolling(): void {
  if (monitorTimer !== undefined) window.clearTimeout(monitorTimer)
  monitorTimer = undefined
}

function scheduleActivePolling(): void {
  stopActivePolling()
  if (!activePolling || document.hidden || connection.state !== 'authenticated') return
  monitorTimer = window.setTimeout(() => { void pollActiveThreads() }, 2000)
}

async function pollActiveThreads(): Promise<void> {
  await refreshActive()
  scheduleActivePolling()
}

function activeVisibilityChanged(): void {
  if (document.hidden) stopActivePolling()
  else void pollActiveThreads()
}

function monitor(active: ActiveThread): void {
  activeTaskId.value = active.taskId
  activeVisible.value = true
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
    monitor(active)
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
  expandedKeys.value = Object.fromEntries(result.value.nodes.map((node) => [messageKeyId(node.messageKey), true]))
  const latest = nodeLookup.value.get(messageKeyId(result.value.summary.latestKey)) ?? result.value.nodes.at(-1)
  if (latest !== undefined) selectNode(latest)
  await Promise.all([loadThreadMedia(result.value, generation), loadThreadStats(result.value, generation)])
}

async function loadThreadStats(thread: ThreadDetail, generation: number): Promise<void> {
  const result = await runBackend(getAuditThreadMessages(thread.nodes.map(({ messageKey }) => messageKey)))
  if (generation !== detailGeneration) return
  if (result._tag === 'Failure') { statsError.value = result.error.message; return }
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
  visible.value = false
  detail.value = undefined
  selectedNode.value = undefined
  treeFocused.value = false
  if (route.params['threadId'] !== undefined) void router.replace({ name: 'threads' })
}

function closeActiveDrawer(): void {
  activeTaskId.value = undefined
  if (route.query['run'] !== undefined) void router.replace({ name: 'threads' })
}

function selectTreeNode(node: PrimeTreeNode): void {
  const selected = nodeLookup.value.get(node.key)
  if (selected !== undefined) selectNode(selected)
}

function selectNode(node: ThreadNode): void {
  selectedNode.value = node
  selectedKeys.value = { [messageKeyId(node.messageKey)]: true }
}

function buildTree(nodes: readonly ThreadNode[]): PrimeTreeNode[] {
  const byKey = new Map(nodes.map((node) => [messageKeyId(node.messageKey), {
    key: messageKeyId(node.messageKey),
    label: nodeLabel(node),
    icon: node.parentMessageKey === null ? 'pi pi-comments' : 'pi pi-reply',
    children: [] as PrimeTreeNode[],
  } satisfies PrimeTreeNode]))
  const roots: PrimeTreeNode[] = []
  for (const node of nodes) {
    const item = byKey.get(messageKeyId(node.messageKey))
    if (item === undefined) continue
    const parent = node.parentMessageKey === null ? undefined : byKey.get(messageKeyId(node.parentMessageKey))
    if (parent === undefined) roots.push(item)
    else parent.children.push(item)
  }
  return roots.length === 0 ? [...byKey.values()] : roots
}

function nodeLabel(node: ThreadNode): string {
  const preferred = [...node.messages].reverse().find((message) => message.role === 'user' && readableMessageText(message) !== '')
    ?? [...node.messages].reverse().find((message) => readableMessageText(message) !== '')
  if (preferred === undefined) return node.messageKey.messageId
  const text = readableMessageText(preferred)
  return text.length > 64 ? `${text.slice(0, 61)}…` : text
}

function readableMessageText(message: StoredThreadMessage): string {
  return messageText(message).replace(/\s+/g, ' ').trim()
}

function transcriptTo(node: ThreadNode, nodes: ReadonlyMap<string, ThreadNode>): StoredThreadMessage[] {
  const segments: (readonly StoredThreadMessage[])[] = []
  const visited = new Set<string>()
  let current: ThreadNode | undefined = node
  while (current !== undefined && !visited.has(messageKeyId(current.messageKey))) {
    visited.add(messageKeyId(current.messageKey))
    segments.unshift(current.messages)
    current = current.parentMessageKey === null ? undefined : nodes.get(messageKeyId(current.parentMessageKey))
  }
  return segments.flat()
}

function messageKeyId(key: ThreadMessageKey): string {
  return `${key.platform}\u0000${key.chatId ?? ''}\u0000${key.messageId}`
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
  return [...new Set([...contentRefs, ...[...text.matchAll(/media:mf_[A-Za-z0-9_-]{7,}/g)].map(([ref]) => ref)])]
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

function openMediaRef(event: MouseEvent): void {
  const mediaRef = mediaRefFromClick(event)
  if (mediaRef === undefined) return
  event.preventDefault()
  void router.push({ name: 'media', params: { mediaId: mediaRef } })
}

function roleLabel(role: string): string {
  return ({ user: 'User', assistant: 'Assistant', system: 'System', tool: 'Tool' } as Readonly<Record<string, string>>)[role] ?? role
}

function formatDuration(milliseconds: number, unreported = 0): string {
  const value = milliseconds < 1000 ? `${String(milliseconds)} ms` : `${(milliseconds / 1000).toFixed(1)} s`
  return unreported === 0 ? value : `${value} · ${String(unreported)} unreported`
}

function threadPlatformName(thread: ThreadSummary): string {
  return platformNames[thread.rootKey.platform]
}

function threadPlatformIcon(thread: ThreadSummary): string {
  return platformIcons[thread.rootKey.platform]
}

onMounted(() => {
  activePolling = true
  void refresh()
  void pollActiveThreads()
  document.addEventListener('visibilitychange', activeVisibilityChanged)
})
onUnmounted(() => { activePolling = false; stopActivePolling(); document.removeEventListener('visibilitychange', activeVisibilityChanged); activeGeneration += 1; detailGeneration += 1; listGeneration += 1 })
watch([() => connection.state, () => connection.methods], () => { void refresh(); void pollActiveThreads() })
watch([() => route.params['threadId'], () => route.query['run']], () => { void selectFromRoute() })
watch([debouncedQuery, platform], () => { first.value = 0; void refresh() })
</script>

<template>
  <section class="page">
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
        <div><span class="summary-mark success"><i class="pi pi-code-branch" /></span><span><strong>{{ summary.leaves }}</strong><small>Branch tips</small></span></div>
        <div><span class="summary-mark neutral"><i class="pi pi-globe" /></span><span><strong>{{ summary.platforms }}</strong><small>Platforms</small></span></div>
      </div>
      <article class="panel active-thread-panel">
        <header class="stream-heading">
          <span><i class="pi pi-circle-fill active-thread-pulse" />Active threads</span><small>Live · refreshes every 2 seconds</small>
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
              label="Monitor"
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
              <span class="manager-identity"><span class="manager-type-icon"><i :class="threadPlatformIcon(data)" /></span><span><strong>#{{ data.threadId }}</strong><small>{{ threadPlatformName(data) }}</small></span></span>
            </template>
          </Column>
          <Column header="Chat">
            <template #body="{ data }">
              <code>{{ data.rootKey.chatId ?? 'Direct / unscoped' }}</code>
            </template>
          </Column>
          <Column header="Root message">
            <template #body="{ data }">
              <span class="thread-message-id">{{ data.rootPreview || 'No text content' }}</span>
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
        v-else-if="detail"
        class="thread-inspector"
      >
        <header class="drawer-hero">
          <span class="platform-icon"><i :class="platformIcons[detail.summary.rootKey.platform]" /></span><div>
            <small>{{ platformNames[detail.summary.rootKey.platform] }} thread</small><h2>Thread #{{ detail.summary.threadId }}</h2>
            <div class="tag-list">
              <Tag
                :value="`${detail.summary.nodeCount} nodes`"
                severity="secondary"
              /><Tag
                :value="`${detail.summary.leafCount} branch tips`"
                severity="info"
              />
            </div>
          </div><Button
            label="View audit"
            icon="pi pi-wave-pulse"
            severity="secondary"
            @click="viewThreadAudit(detail.summary.threadId)"
          />
        </header>
        <dl class="detail-list">
          <div><dt>Chat</dt><dd><code>{{ detail.summary.rootKey.chatId ?? 'Direct / unscoped' }}</code></dd></div>
          <div><dt>Root message</dt><dd><ChatLogMessageLink :message-key="detail.summary.rootKey" /></dd></div>
          <div><dt>Latest message</dt><dd><ChatLogMessageLink :message-key="detail.summary.latestKey" /></dd></div>
        </dl>
        <Message
          v-if="statsError"
          severity="warn"
          :closable="false"
        >
          {{ statsError }}
        </Message>
        <section
          v-else
          class="thread-stats"
          aria-label="Thread execution statistics"
        >
          <div><span class="summary-mark violet"><i class="pi pi-chart-bar" /></span><span><strong>{{ stats.tokens?.total_tokens.toLocaleString() ?? 'Unreported' }}</strong><small v-if="stats.tokens">Tokens · {{ stats.tokens.prompt_tokens.toLocaleString() }} prompt / {{ stats.tokens.completion_tokens.toLocaleString() }} completion</small><small v-else>Token usage was not returned by the model provider</small></span></div>
          <div><span class="summary-mark info"><i class="pi pi-sparkles" /></span><span><strong>{{ formatDuration(stats.modelMilliseconds, stats.unreportedModelTurns) }}</strong><small>Model time · {{ stats.modelTurns }} turns</small></span></div>
          <div><span class="summary-mark warning"><i class="pi pi-wrench" /></span><span><strong>{{ formatDuration(stats.toolMilliseconds, stats.unreportedToolCalls) }}</strong><small>Tool time · {{ stats.toolCalls }} calls / {{ stats.failedTools }} failed</small></span></div>
          <div><span class="summary-mark success"><i class="pi pi-clock" /></span><span><strong>{{ formatDuration(stats.wallMilliseconds) }}</strong><small>Run wall time · {{ stats.runs }} runs</small></span></div>
          <footer>
            <Tag
              :value="`${stats.contextMessages} peak context messages`"
              severity="secondary"
            /><Tag
              :value="`${stats.compactions} compactions`"
              severity="secondary"
            /><Tag
              :value="`${stats.subagents} subagents`"
              severity="secondary"
            /><Tag
              :value="`${auditRecords.length} audit events`"
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
              <span>Context at node</span><small><ChatLogMessageLink
                v-if="selectedNode"
                :message-key="selectedNode.messageKey"
              /><template v-else>No node selected</template></small>
            </header>
            <div
              v-if="transcript.length === 0"
              class="thread-empty"
            >
              No stored messages for this node.
            </div>
            <ol
              v-else
              class="thread-transcript"
            >
              <li
                v-for="(message, index) in transcript"
                :key="index"
                :class="`role-${message.role}`"
              >
                <div><strong>{{ roleLabel(message.role) }}</strong><code v-if="message.tool_call_id">{{ message.tool_call_id }}</code></div>
                <div
                  v-if="imageUrls(message).length > 0"
                  class="chat-images thread-media"
                >
                  <button
                    v-for="url in imageUrls(message)"
                    :key="url"
                    type="button"
                    class="chat-image-button"
                    aria-label="Zoom image"
                    @click="previewImage = url"
                  >
                    <img
                      :src="url"
                      alt="Thread image"
                      loading="lazy"
                    />
                  </button>
                </div>
                <div
                  v-if="mediaDetails(message).some(({ mimeType }) => mimeType.startsWith('video/') || mimeType.startsWith('audio/') || !mimeType.startsWith('image/'))"
                  class="thread-attachments"
                >
                  <template
                    v-for="media in mediaDetails(message)"
                    :key="media.mediaId"
                  >
                    <video
                      v-if="media.exists && mediaUrl(media) && media.mimeType.startsWith('video/')"
                      class="object-preview"
                      controls
                      preload="metadata"
                    ><source
                      :src="mediaUrl(media)"
                      :type="media.mimeType"
                    /></video>
                    <audio
                      v-else-if="media.exists && mediaUrl(media) && media.mimeType.startsWith('audio/')"
                      controls
                      preload="metadata"
                    ><source
                      :src="mediaUrl(media)"
                      :type="media.mimeType"
                    /></audio>
                    <RouterLink
                      v-else-if="!media.mimeType.startsWith('image/')"
                      class="chat-file-card"
                      :to="{ name: 'media', params: { mediaId: media.mediaId } }"
                    >
                      <i class="pi pi-file" /><span><strong>{{ media.sourceName || media.mediaId }}</strong><small>{{ media.mimeType }} · {{ formatBytes(media.size) }}</small></span><i class="pi pi-arrow-right" />
                    </RouterLink>
                  </template>
                </div>
                <div
                  class="markdown-body"
                  :innerHTML="renderMarkdown(messageText(message))"
                  @click="openMediaRef"
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
          </section>
        </div>
      </div>
    </Drawer>
    <Drawer
      v-model:visible="activeVisible"
      header="Active thread monitor"
      position="right"
      :style="{ width: 'min(720px, 100vw)' }"
      @hide="closeActiveDrawer"
    >
      <div
        v-if="activeSelected"
        class="thread-inspector"
      >
        <header class="drawer-hero">
          <span class="platform-icon"><i class="pi pi-sparkles" /></span><div>
            <small>Task #{{ activeSelected.taskId }} · live</small><h2>{{ activeSelected.prompt || 'Active thread' }}</h2><Tag
              value="Running"
              severity="success"
            />
          </div><Button
            label="View audit"
            icon="pi pi-wave-pulse"
            severity="secondary"
            @click="viewRunAudit(activeSelected.runId)"
          />
        </header>
        <dl class="detail-list">
          <div><dt>Agent run</dt><dd><RunIdLink :run-id="activeSelected.runId" /></dd></div><div><dt>Linked messages</dt><dd>{{ activeSelected.messageKeys.length }}</dd></div><div><dt>Pending steers</dt><dd>{{ activeSelected.pendingSteers }}</dd></div>
        </dl>
        <section class="thread-transcript-panel active-transcript">
          <header><span>Current model context</span><small>{{ activeSelected.messages.length }} messages</small></header>
          <ol class="thread-transcript">
            <li
              v-for="(message, index) in activeSelected.messages"
              :key="index"
              :class="`role-${message.role}`"
            >
              <div><strong>{{ roleLabel(message.role) }}</strong><code v-if="message.tool_call_id">{{ message.tool_call_id }}</code></div>
              <div
                v-if="imageUrls(message).length"
                class="chat-images thread-media"
              >
                <button
                  v-for="url in imageUrls(message)"
                  :key="url"
                  type="button"
                  class="chat-image-button"
                  aria-label="Zoom image"
                  @click="previewImage = url"
                >
                  <img
                    :src="url"
                    alt="Thread image"
                    loading="lazy"
                  />
                </button>
              </div>
              <div
                v-if="mediaDetails(message).some(({ mimeType }) => mimeType.startsWith('video/') || mimeType.startsWith('audio/') || !mimeType.startsWith('image/'))"
                class="thread-attachments"
              >
                <template
                  v-for="media in mediaDetails(message)"
                  :key="media.mediaId"
                >
                  <video
                    v-if="media.exists && mediaUrl(media) && media.mimeType.startsWith('video/')"
                    class="object-preview"
                    controls
                    preload="metadata"
                  ><source
                    :src="mediaUrl(media)"
                    :type="media.mimeType"
                  /></video>
                  <audio
                    v-else-if="media.exists && mediaUrl(media) && media.mimeType.startsWith('audio/')"
                    controls
                    preload="metadata"
                  ><source
                    :src="mediaUrl(media)"
                    :type="media.mimeType"
                  /></audio>
                  <RouterLink
                    v-else-if="!media.mimeType.startsWith('image/')"
                    class="chat-file-card"
                    :to="{ name: 'media', params: { mediaId: media.mediaId } }"
                  >
                    <i class="pi pi-file" /><span><strong>{{ media.sourceName || media.mediaId }}</strong><small>{{ media.mimeType }} · {{ formatBytes(media.size) }}</small></span><i class="pi pi-arrow-right" />
                  </RouterLink>
                </template>
              </div>
              <div
                class="markdown-body"
                :innerHTML="renderMarkdown(messageText(message))"
                @click="openMediaRef"
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
        </section>
        <Button
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
