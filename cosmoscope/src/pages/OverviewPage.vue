<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import Button from 'primevue/button'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import PageHeading from '@/components/PageHeading.vue'
import OverviewActivityPanel from '@/components/overview/OverviewActivityPanel.vue'
import OverviewMetrics from '@/components/overview/OverviewMetrics.vue'
import type { OverviewMetric } from '@/components/overview/OverviewMetrics.vue'
import OverviewTaskDrawer from '@/components/overview/OverviewTaskDrawer.vue'
import OverviewTaskPanel from '@/components/overview/OverviewTaskPanel.vue'
import { countAudit, countResources, countSessions, countThreads, getChatLogStats, listMedia, listTasks, recentAudit, subscribeAudit } from '@/backend/AdminBackend'
import { auditActivity, mergeAuditRecords } from '@/backend/overview'
import { runBackend } from '@/backend/runBackend'
import { useLatest, useLatestSubscription } from '@/async'
import { useVisibilityPolling } from '@/composables/useVisibilityPolling'
import { formatBytes } from '@/format'
import { useConnectionStore } from '@/stores/connection'
import type { Activity, AuditRecord, MediaStats, Task } from '@/types/domain'
import type { LiveAdminMethod } from '@/rpc/protocol'

const router = useRouter()
const connection = useConnectionStore()
const today = new Intl.DateTimeFormat(undefined, { weekday: 'long', day: 'numeric', month: 'long' }).format(new Date())
type PageState = 'loading' | 'ready' | 'error'
const state = ref<PageState>('loading')
const error = ref('')
const tasks = ref<Task[]>([])
const auditRecords = ref<AuditRecord[]>([])
const activities = ref<Activity[]>([])
const threadCount = ref(0)
const sessionCount = ref(0)
const chatMessageCount = ref(0)
const chatPlatformCount = ref(0)
const auditCount = ref(0)
const resourceCount = ref(0)
const mediaStats = ref<MediaStats>({ files: 0, totalBytes: 0, sources: 0, platformRefs: 0, platformAssociations: 0, mimeTypes: [], platforms: [] })
const taskError = ref('')
const auditError = ref('')
const auditCountError = ref('')
const threadError = ref('')
const sessionError = ref('')
const chatLogError = ref('')
const resourceError = ref('')
const mediaError = ref('')
const auditLoading = ref(true)
const threadLoading = ref(true)
const chatLogLoading = ref(true)
const mediaLoading = ref(true)
const resourceLoading = ref(true)
const selectedTask = ref<Task>()
const drawerOpen = ref(false)
const cycleLatest = useLatest()
const auditLatest = useLatest()
const auditCountLatest = useLatest()
const auditSubscription = useLatestSubscription()

const supports = (method: LiveAdminMethod): boolean => connection.state === 'authenticated' && connection.methods.has(method)
const metrics = computed<readonly OverviewMetric[]>(() => [
  { to: '/threads', icon: 'pi pi-sitemap', tone: 'violet', available: supports('thread.count'), loading: threadLoading.value, value: threadCount.value, label: 'Conversation threads', detail: 'Persisted reply trees', error: threadError.value },
  { to: '/chat', icon: 'pi pi-comments', tone: 'green', available: supports('chat.list_sessions'), loading: chatLogLoading.value, value: chatMessageCount.value, label: 'Chat messages', detail: `${String(chatPlatformCount.value)} platforms · ${String(sessionCount.value)} RPC sessions`, error: chatLogError.value || sessionError.value },
  { to: '/audit', icon: 'pi pi-wave-pulse', tone: 'blue', available: supports('audit.count'), value: auditCount.value, label: 'Audit events', detail: 'Complete event history', error: auditCountError.value },
  { to: '/media', icon: 'pi pi-images', tone: 'violet', available: supports('media.stats'), loading: mediaLoading.value, value: formatBytes(mediaStats.value.totalBytes), label: 'Media storage', detail: `${String(mediaStats.value.files)} objects`, error: mediaError.value },
  { to: '/resources', icon: 'pi pi-box', tone: 'blue', available: supports('resource.list'), loading: resourceLoading.value, value: resourceCount.value, label: 'Managed resources', detail: 'Current resource snapshot', error: resourceError.value },
  { to: '/tasks', icon: 'pi pi-bolt', tone: 'green', available: supports('concurrency.list'), value: tasks.value.length, label: 'Tasks', detail: `${String(tasks.value.filter(({ status }) => status === 'running').length)} active`, error: taskError.value },
])
async function loadTasks(token: symbol): Promise<void> {
  const result = await runBackend(listTasks)
  if (!cycleLatest.current(token)) return
  if (result._tag === 'Failure') { taskError.value = result.error.message; return }
  taskError.value = ''; tasks.value = [...result.value]
  if (selectedTask.value) selectedTask.value = tasks.value.find(({ id }) => id === selectedTask.value?.id) ?? selectedTask.value
}

async function loadAudit(): Promise<void> {
  const token = auditLatest.begin()
  if (!auditLatest.current(token)) return
  const result = await runBackend(recentAudit())
  if (!auditLatest.current(token)) return
  auditLoading.value = false
  if (result._tag === 'Failure') { auditError.value = result.error.message; return }
  auditError.value = ''; auditRecords.value = [...result.value]; activities.value = auditActivity(result.value)
}

async function loadAuditCount(): Promise<void> {
  const token = auditCountLatest.begin()
  if (!auditCountLatest.current(token)) return
  const result = await runBackend(countAudit)
  if (!auditCountLatest.current(token)) return
  if (result._tag === 'Failure') { auditCountError.value = result.error.message; return }
  auditCountError.value = ''; auditCount.value = result.value
}

async function loadSessions(token: symbol): Promise<void> {
  const result = await runBackend(countSessions)
  if (!cycleLatest.current(token)) return
  if (result._tag === 'Failure') { sessionError.value = result.error.message; return }
  sessionError.value = ''; sessionCount.value = result.value
}

async function loadChatLogs(token: symbol): Promise<void> {
  const result = await runBackend(getChatLogStats)
  if (!cycleLatest.current(token)) return
  chatLogLoading.value = false
  if (result._tag === 'Failure') { chatLogError.value = result.error.message; return }
  chatLogError.value = ''
  chatMessageCount.value = result.value.messages
  chatPlatformCount.value = result.value.platforms
}

async function loadThreads(token: symbol): Promise<void> {
  const result = await runBackend(countThreads)
  if (!cycleLatest.current(token)) return
  threadLoading.value = false
  if (result._tag === 'Failure') { threadError.value = result.error.message; return }
  threadError.value = ''; threadCount.value = result.value
}

async function loadResources(token: symbol): Promise<void> {
  const result = await runBackend(countResources)
  if (!cycleLatest.current(token)) return
  resourceLoading.value = false
  if (result._tag === 'Failure') { resourceError.value = result.error.message; return }
  resourceError.value = ''; resourceCount.value = result.value
}

async function loadMedia(token: symbol): Promise<void> {
  const result = await runBackend(listMedia(4))
  if (!cycleLatest.current(token)) return
  mediaLoading.value = false
  if (result._tag === 'Failure') { mediaError.value = result.error.message; return }
  mediaError.value = ''; mediaStats.value = result.value.stats
}

async function installAuditSubscription(): Promise<void> {
  if (!supports('audit.subscribe') || !supports('audit.recent')) return
  const token = auditSubscription.begin()
  if (!auditSubscription.current(token)) return
  const result = await runBackend(subscribeAudit(async () => {
    if (!auditSubscription.current(token)) return
    await Promise.all([loadAudit(), loadAuditCount()])
  }, (record) => {
    if (!auditSubscription.current(token)) return
    auditRecords.value = mergeAuditRecords(auditRecords.value, record)
    activities.value = auditActivity(auditRecords.value)
    auditCount.value += 1
  }))
  if (result._tag === 'Success') {
    if (auditSubscription.current(token) && (!supports('audit.subscribe') || !supports('audit.recent'))) auditSubscription.invalidate()
    auditSubscription.own(token, result.value)
  }
  else if (auditSubscription.current(token)) { auditLoading.value = false; auditError.value = result.error.message }
}

async function loadImmediateSnapshots(token: symbol): Promise<void> {
  await Promise.all([
    supports('concurrency.list') ? loadTasks(token) : Promise.resolve(),
    supports('audit.count') ? loadAuditCount() : Promise.resolve(),
  ])
}

async function loadDeferredSnapshots(token: symbol): Promise<void> {
  await Promise.all([
    supports('chat.list_sessions') ? loadSessions(token) : Promise.resolve(),
    supports('media.stats') ? loadMedia(token) : Promise.resolve(),
    supports('chat_log.stats') ? loadChatLogs(token) : Promise.resolve(),
    supports('thread.count') ? loadThreads(token) : Promise.resolve(),
    supports('resource.list') ? loadResources(token) : Promise.resolve(),
  ])
}

async function loadSlowSnapshots(): Promise<void> {
  const token = cycleLatest.begin()
  if (!cycleLatest.current(token)) return
  await loadImmediateSnapshots(token)
  if (!cycleLatest.current(token)) return
  await loadDeferredSnapshots(token)
}

const polling = useVisibilityPolling(loadSlowSnapshots, { interval: 30_000 })

async function refreshLive(): Promise<void> {
  const token = cycleLatest.begin()
  if (!cycleLatest.current(token)) return
  if (connection.state === 'opening' || connection.state === 'reconnecting') {
    if (state.value !== 'ready') state.value = 'loading'
    return
  }
  if (connection.state !== 'authenticated') {
    if (state.value !== 'ready') state.value = 'error'
    error.value = connection.error || 'Connect to cosmobot to load the overview.'
    return
  }
  if (state.value !== 'ready') state.value = 'loading'
  error.value = ''
  auditError.value = supports('audit.recent') ? '' : 'The server does not support audit.recent.'
  auditCountError.value = supports('audit.count') ? '' : 'The server does not support audit.count.'
  taskError.value = supports('concurrency.list') ? '' : 'The server does not support concurrency.list.'
  sessionError.value = supports('chat.list_sessions') ? '' : 'The server does not support chat.list_sessions.'
  chatLogError.value = supports('chat_log.stats') ? '' : 'The server does not support chat_log.stats.'
  threadError.value = supports('thread.count') ? '' : 'The server does not support thread.count.'
  resourceError.value = supports('resource.list') ? '' : 'The server does not support resource.list.'
  mediaError.value = supports('media.stats') ? '' : 'The server does not support media.stats.'
  if (!supports('audit.recent')) auditLoading.value = false
  if (!supports('thread.count')) threadLoading.value = false
  if (!supports('chat_log.stats')) chatLogLoading.value = false
  if (!supports('media.stats')) mediaLoading.value = false
  if (!supports('resource.list')) resourceLoading.value = false
  if (supports('audit.subscribe') && supports('audit.recent')) {
    if (!auditSubscription.owned()) await installAuditSubscription()
  }
  else {
    auditSubscription.invalidate()
  }
  if (!cycleLatest.current(token)) return
  const immediate = loadImmediateSnapshots(token)
  if (supports('audit.recent') && !supports('audit.subscribe')) void loadAudit()
  await immediate
  if (!cycleLatest.current(token)) return
  state.value = 'ready'
  await loadDeferredSnapshots(token)
  if (cycleLatest.current(token)) polling.start()
}

function stopLive(): void {
  cycleLatest.invalidate()
  auditLatest.invalidate()
  auditCountLatest.invalidate()
  auditSubscription.invalidate()
  polling.stop()
}
function inspect(task: Task): void { selectedTask.value = task; drawerOpen.value = true }

watch([() => connection.state, () => connection.methods], ([next]) => {
  if (next === 'authenticated' && connection.methods.size > 0) void refreshLive()
  else stopLive()
})
onMounted(() => {
  void refreshLive()
})
</script>

<template>
  <section class="page">
    <PageHeading
      :eyebrow="today"
      title="Cosmobot overview"
      description="Here is what cosmobot is doing right now."
    >
      <Button
        label="View audit"
        severity="secondary"
        @click="router.push('/audit')"
      />
      <Button
        label="Open chat"
        icon="pi pi-arrow-up-right"
        icon-pos="right"
        @click="router.push('/chat')"
      />
    </PageHeading>
    <div
      v-if="state === 'loading'"
      class="metric-grid overview-summary-grid"
      aria-label="Loading overview"
    >
      <article
        v-for="index in 6"
        :key="index"
        class="metric"
      >
        <Skeleton
          width="2rem"
          height="2rem"
        /><Skeleton
          width="4rem"
          height="2.2rem"
        /><Skeleton width="8rem" />
      </article>
    </div>
    <Message
      v-else-if="state === 'error'"
      severity="error"
      :closable="false"
    >
      {{ error }}
      <Button
        label="Retry"
        size="small"
        text
        @click="refreshLive"
      />
    </Message>
    <template v-else>
      <Message
        v-if="error"
        severity="error"
        :closable="false"
      >
        {{ error }}
      </Message>
      <OverviewMetrics :metrics="metrics" />
      <div class="overview-grid">
        <OverviewTaskPanel
          :tasks="tasks"
          :error="taskError"
          @inspect="inspect"
          @view-all="router.push('/tasks')"
        />
        <OverviewActivityPanel
          :activities="activities"
          :error="auditError"
          :loading="auditLoading"
          :live="supports('audit.subscribe')"
        />
      </div>
    </template>
    <OverviewTaskDrawer
      v-model:visible="drawerOpen"
      :task="selectedTask"
      @hide="selectedTask = undefined"
    />
  </section>
</template>
