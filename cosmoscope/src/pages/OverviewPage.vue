<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { RouterLink, useRouter } from 'vue-router'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Drawer from 'primevue/drawer'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import RunIdLink from '@/components/RunIdLink.vue'
import StatusBadge from '@/components/StatusBadge.vue'
import { countAudit, countResources, countSessions, listChatLogs, listMedia, listTasks, listThreads, recentAudit, subscribeAudit } from '@/backend/AdminBackend'
import { auditActivity, mergeAuditRecords } from '@/backend/overview'
import { runBackend } from '@/backend/runBackend'
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
const mediaStats = ref<MediaStats>({ files: 0, existingFiles: 0, missingFiles: 0, totalBytes: 0, sources: 0, platformRefs: 0, platformAssociations: 0, mimeTypes: [], platforms: [] })
const taskError = ref('')
const auditError = ref('')
const auditCountError = ref('')
const threadError = ref('')
const sessionError = ref('')
const chatLogError = ref('')
const resourceError = ref('')
const mediaError = ref('')
const selectedTask = ref<Task>()
const drawerOpen = ref(false)
let stopAuditSubscription: (() => void) | undefined
let pollTimer: ReturnType<typeof setTimeout> | undefined

const supports = (method: LiveAdminMethod): boolean => connection.state === 'authenticated' && connection.methods.has(method)
async function loadTasks(): Promise<void> {
  const result = await runBackend(listTasks)
  if (result._tag === 'Failure') { taskError.value = result.error.message; return }
  taskError.value = ''; tasks.value = [...result.value]
  if (selectedTask.value) selectedTask.value = tasks.value.find(({ id }) => id === selectedTask.value?.id) ?? selectedTask.value
}

async function loadAudit(): Promise<void> {
  const result = await runBackend(recentAudit())
  if (result._tag === 'Failure') { auditError.value = result.error.message; return }
  auditError.value = ''; auditRecords.value = [...result.value]; activities.value = auditActivity(result.value)
}

async function loadAuditCount(): Promise<void> {
  const result = await runBackend(countAudit)
  if (result._tag === 'Failure') { auditCountError.value = result.error.message; return }
  auditCountError.value = ''; auditCount.value = result.value
}

async function loadSessions(): Promise<void> {
  const result = await runBackend(countSessions)
  if (result._tag === 'Failure') { sessionError.value = result.error.message; return }
  sessionError.value = ''; sessionCount.value = result.value
}

async function loadChatLogs(): Promise<void> {
  const result = await runBackend(listChatLogs)
  if (result._tag === 'Failure') { chatLogError.value = result.error.message; return }
  chatLogError.value = ''
  chatMessageCount.value = result.value.reduce((total, chat) => total + chat.messageCount, 0)
  chatPlatformCount.value = new Set(result.value.map(({ scope }) => scope.platform)).size
}

async function loadThreads(): Promise<void> {
  const result = await runBackend(listThreads({ offset: 0, limit: 1 }))
  if (result._tag === 'Failure') { threadError.value = result.error.message; return }
  threadError.value = ''; threadCount.value = result.value.total
}

async function loadResources(): Promise<void> {
  const result = await runBackend(countResources)
  if (result._tag === 'Failure') { resourceError.value = result.error.message; return }
  resourceError.value = ''; resourceCount.value = result.value
}

async function loadMedia(): Promise<void> {
  const result = await runBackend(listMedia(4))
  if (result._tag === 'Failure') { mediaError.value = result.error.message; return }
  mediaError.value = ''; mediaStats.value = result.value.stats
}

async function installAuditSubscription(): Promise<void> {
  if (!supports('audit.subscribe') || !supports('audit.recent')) return
  const result = await runBackend(subscribeAudit(async () => { await Promise.all([loadAudit(), loadAuditCount()]) }, (record) => {
    auditRecords.value = mergeAuditRecords(auditRecords.value, record)
    activities.value = auditActivity(auditRecords.value)
    auditCount.value += 1
  }))
  if (result._tag === 'Success') stopAuditSubscription = result.value
  else auditError.value = result.error.message
}

function startPolling(): void {
  stopPolling()
  if (document.hidden || connection.state !== 'authenticated') return
  pollTimer = setTimeout(async () => {
    await loadSlowSnapshots()
    startPolling()
  }, 30_000)
}

function stopPolling(): void {
  if (pollTimer !== undefined) clearTimeout(pollTimer)
  pollTimer = undefined
}

async function loadSlowSnapshots(): Promise<void> {
  await Promise.all([
    supports('concurrency.list') ? loadTasks() : Promise.resolve(),
    supports('thread.list') ? loadThreads() : Promise.resolve(),
    supports('chat.list_sessions') ? loadSessions() : Promise.resolve(),
    supports('chat_log.list') ? loadChatLogs() : Promise.resolve(),
    supports('resource.list') ? loadResources() : Promise.resolve(),
    supports('media.stats') ? loadMedia() : Promise.resolve(),
    supports('audit.count') ? loadAuditCount() : Promise.resolve(),
  ])
}

async function refreshLive(): Promise<void> {
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
  if (supports('audit.subscribe') && supports('audit.recent')) {
    if (stopAuditSubscription === undefined) await installAuditSubscription()
  }
  else {
    stopAuditSubscription?.()
    stopAuditSubscription = undefined
  }
  await Promise.all([
    supports('concurrency.list') ? loadTasks() : Promise.resolve().then(() => { taskError.value = 'The server does not support concurrency.list.' }),
    supports('thread.list') ? loadThreads() : Promise.resolve().then(() => { threadError.value = 'The server does not support thread.list.' }),
    supports('chat.list_sessions') ? loadSessions() : Promise.resolve().then(() => { sessionError.value = 'The server does not support chat.list_sessions.' }),
    supports('chat_log.list') ? loadChatLogs() : Promise.resolve().then(() => { chatLogError.value = 'The server does not support chat_log.list.' }),
    supports('resource.list') ? loadResources() : Promise.resolve().then(() => { resourceError.value = 'The server does not support resource.list.' }),
    supports('media.stats') ? loadMedia() : Promise.resolve().then(() => { mediaError.value = 'The server does not support media.stats.' }),
    supports('audit.count') ? loadAuditCount() : Promise.resolve(),
    supports('audit.recent') && !supports('audit.subscribe') ? loadAudit() : Promise.resolve(),
  ])
  state.value = 'ready'
  startPolling()
}

function stopLive(): void {
  stopAuditSubscription?.()
  stopAuditSubscription = undefined
  stopPolling()
}

function onVisibilityChange(): void {
  if (document.hidden) stopPolling()
  else {
    void loadSlowSnapshots().then(startPolling)
  }
}
function inspect(task: Task): void { selectedTask.value = task; drawerOpen.value = true }
function formatTaskTime(value: string): string { return new Date(value).toLocaleTimeString() }
function taskElapsed(task: Task): string {
  const end = task.finishedAt === null ? Date.now() : Date.parse(task.finishedAt)
  return `${String(Math.max(0, Math.round((end - Date.parse(task.startedAt)) / 60_000)))}m`
}

watch(() => connection.methods, () => {
  if (connection.state === 'authenticated') void refreshLive()
})
watch(() => connection.state, (next) => {
  if (next === 'authenticated' && connection.methods.size > 0) void refreshLive()
  else if (next === 'offline' || next === 'failed') stopLive()
})
onMounted(async () => {
  await refreshLive()
  document.addEventListener('visibilitychange', onVisibilityChange)
})
onUnmounted(() => { stopLive(); document.removeEventListener('visibilitychange', onVisibilityChange) })
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
      <div class="metric-grid overview-summary-grid">
        <RouterLink
          class="metric"
          to="/threads"
        >
          <div class="metric-top">
            <span class="pi pi-sitemap metric-icon violet" /><Tag
              :value="supports('thread.list') ? 'Live' : 'Unavailable'"
              :severity="supports('thread.list') ? 'success' : 'warn'"
            />
          </div>
          <strong>{{ threadCount }}</strong><p>Conversation threads</p>
          <small
            v-if="threadError"
            class="metric-error"
          >{{ threadError }}</small><small v-else>Persisted reply trees</small>
        </RouterLink>
        <RouterLink
          class="metric"
          to="/chat"
        >
          <div class="metric-top">
            <span class="pi pi-comments metric-icon green" /><Tag
              :value="supports('chat.list_sessions') ? 'Live' : 'Unavailable'"
              :severity="supports('chat.list_sessions') ? 'success' : 'warn'"
            />
          </div>
          <strong>{{ chatMessageCount }}</strong><p>Chat messages</p>
          <small
            v-if="chatLogError || sessionError"
            class="metric-error"
          >{{ chatLogError || sessionError }}</small><small v-else>{{ chatPlatformCount }} platforms · {{ sessionCount }} RPC sessions</small>
        </RouterLink>
        <RouterLink
          class="metric"
          to="/audit"
        >
          <div class="metric-top">
            <span class="pi pi-wave-pulse metric-icon blue" /><Tag
              :value="supports('audit.count') ? 'Live' : 'Unavailable'"
              :severity="supports('audit.count') ? 'success' : 'warn'"
            />
          </div>
          <strong>{{ auditCount }}</strong><p>Audit events</p>
          <small
            v-if="auditCountError"
            class="metric-error"
          >{{ auditCountError }}</small><small v-else>Complete event history</small>
        </RouterLink>
        <RouterLink
          class="metric"
          to="/media"
        >
          <div class="metric-top">
            <span class="pi pi-images metric-icon violet" /><Tag
              :value="supports('media.stats') ? 'Live' : 'Unavailable'"
              :severity="supports('media.stats') ? 'success' : 'warn'"
            />
          </div>
          <strong>{{ formatBytes(mediaStats.totalBytes) }}</strong><p>Media storage</p>
          <small
            v-if="mediaError"
            class="metric-error"
          >{{ mediaError }}</small><small v-else>{{ mediaStats.files }} objects · {{ mediaStats.missingFiles }} missing</small>
        </RouterLink>
        <RouterLink
          class="metric"
          to="/resources"
        >
          <div class="metric-top">
            <span class="pi pi-box metric-icon blue" /><Tag
              :value="supports('resource.list') ? 'Live' : 'Unavailable'"
              :severity="supports('resource.list') ? 'success' : 'warn'"
            />
          </div>
          <strong>{{ resourceCount }}</strong><p>Managed resources</p>
          <small
            v-if="resourceError"
            class="metric-error"
          >{{ resourceError }}</small><small v-else>Current resource snapshot</small>
        </RouterLink>
        <RouterLink
          class="metric"
          to="/tasks"
        >
          <div class="metric-top">
            <span class="pi pi-bolt metric-icon green" /><Tag
              :value="supports('concurrency.list') ? 'Live' : 'Unavailable'"
              :severity="supports('concurrency.list') ? 'success' : 'warn'"
            />
          </div>
          <strong>{{ tasks.length }}</strong><p>Tasks</p>
          <small
            v-if="taskError"
            class="metric-error"
          >{{ taskError }}</small><small v-else>{{ tasks.filter(({ status }) => status === 'running').length }} active</small>
        </RouterLink>
      </div>
      <div class="overview-grid">
        <article class="panel">
          <div class="panel-heading">
            <div><h2>Active tasks</h2><p>Work managed by Concurrency</p></div><Button
              label="View all"
              text
              @click="router.push('/tasks')"
            />
          </div>
          <Message
            v-if="taskError"
            severity="error"
            :closable="false"
          >
            {{ taskError }}
          </Message>
          <DataTable
            v-else
            :value="tasks.filter(({ status }) => status === 'running').slice(0, 8)"
            data-key="id"
            selection-mode="single"
            @row-select="inspect($event.data)"
          >
            <Column
              field="label"
              header="Task"
            >
              <template #body="{ data }">
                <span class="task-name"><span class="platform-icon"><i class="pi pi-bolt" /></span><span><strong>{{ data.label }}</strong><small>Task #{{ data.id }}</small></span></span>
              </template>
            </Column>
            <Column
              field="status"
              header="Status"
            >
              <template #body="{ data }">
                <StatusBadge :status="data.status" />
              </template>
            </Column>
            <Column
              header="Started"
            >
              <template #body="{ data }">
                {{ formatTaskTime(data.startedAt) }}
              </template>
            </Column><Column
              header="Elapsed"
            >
              <template #body="{ data }">
                {{ taskElapsed(data) }}
              </template>
            </Column>
          </DataTable>
        </article>
        <article class="panel activity-panel">
          <div class="panel-heading">
            <div><h2>Recent activity</h2><p>Agent audit events</p></div><Tag
              :value="supports('audit.subscribe') ? 'Live' : 'Snapshot'"
              :severity="supports('audit.subscribe') ? 'success' : 'secondary'"
            />
          </div>
          <Message
            v-if="auditError"
            severity="error"
            :closable="false"
          >
            {{ auditError }}
          </Message>
          <ol
            v-else
            class="activity-list"
          >
            <li
              v-for="item in activities.slice(0, 8)"
              :key="item.id"
            >
              <i
                class="pi pi-circle-fill"
                :class="item.tone"
              /><div>
                <p>
                  <RouterLink :to="`/audit/${item.id}`">
                    <strong>{{ item.kind }}</strong> {{ item.summary }}
                  </RouterLink>
                </p><small><RunIdLink :run-id="item.source" /> · {{ item.time }}</small>
              </div>
            </li>
          </ol>
        </article>
      </div>
    </template>
    <Drawer
      v-model:visible="drawerOpen"
      position="right"
      header="Task detail"
      aria-label="Task detail"
      :style="{ width: 'min(420px, 100vw)' }"
      @hide="selectedTask = undefined"
    >
      <template v-if="selectedTask">
        <div class="stack stack-loose">
          <div class="drawer-hero">
            <span class="platform-icon"><i class="pi pi-bolt" /></span><div><h2>{{ selectedTask.label }}</h2><StatusBadge :status="selectedTask.status" /></div>
          </div>
          <dl class="detail-list">
            <div><dt>ID</dt><dd>#{{ selectedTask.id }}</dd></div><div><dt>Started</dt><dd>{{ formatTaskTime(selectedTask.startedAt) }}</dd></div><div><dt>Elapsed</dt><dd>{{ taskElapsed(selectedTask) }}</dd></div>
          </dl>
          <Button
            label="Open task page"
            @click="router.push(`/tasks/${selectedTask?.id}`)"
          />
        </div>
      </template>
    </Drawer>
  </section>
</template>
