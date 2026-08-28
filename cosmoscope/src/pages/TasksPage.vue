<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useConfirm } from 'primevue/useconfirm'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Drawer from 'primevue/drawer'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import Select from 'primevue/select'
import Skeleton from 'primevue/skeleton'
import PageHeading from '@/components/PageHeading.vue'
import StatusBadge from '@/components/StatusBadge.vue'
import { cancelTask, destroyTaskResources, listTaskResources, listTasks, lookupTask } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { AssociatedResource, Task, TaskStatus } from '@/types/domain'
import { useConnectionStore } from '@/stores/connection'

const taskMethods = ['concurrency.list', 'concurrency.lookup', 'concurrency.cancel', 'concurrency.await', 'resource.list_associated', 'resource.destroy_associated'] as const
type TaskFilter = 'all' | TaskStatus
type PendingAction = 'cancel' | 'destroy-associated'
const tasks = ref<Task[]>([])
const route = useRoute()
const router = useRouter()
const query = ref('')
const statusFilter = ref<TaskFilter>('all')
const selected = ref<Task>()
const associatedResources = ref<AssociatedResource[]>([])
const drawerOpen = ref(false)
const pending = ref<PendingAction>()
const error = ref('')
const loading = ref(true)
const loaded = ref(false)
const confirm = useConfirm()
const toast = useToast()
const connection = useConnectionStore()
const live = computed(() => connection.state === 'authenticated' && taskMethods.every((method) => connection.methods.has(method)))
const summary = computed(() => ({
  running: tasks.value.filter(({ status }) => status === 'running').length,
  completed: tasks.value.filter(({ status }) => status === 'completed').length,
  failed: tasks.value.filter(({ status }) => status === 'failed').length,
  cancelled: tasks.value.filter(({ status }) => status === 'cancelled').length,
}))
const filtered = computed(() => tasks.value.filter((task) =>
  `${String(task.id)} ${task.label}`.toLowerCase().includes(query.value.toLowerCase())
  && (statusFilter.value === 'all' || task.status === statusFilter.value),
))
const statusOptions = [
  { label: 'All statuses', value: 'all' },
  { label: 'Running', value: 'running' },
  { label: 'Completed', value: 'completed' },
  { label: 'Failed', value: 'failed' },
  { label: 'Cancelled', value: 'cancelled' },
] satisfies readonly { label: string; value: TaskFilter }[]
const taskIcons: Readonly<Record<string, string>> = {
  agent: 'pi pi-sparkles',
  subagent: 'pi pi-users',
  tool: 'pi pi-wrench',
  resource: 'pi pi-box',
  command: 'pi pi-desktop',
  python: 'pi pi-code',
  scheduler: 'pi pi-clock',
  media: 'pi pi-image',
  plugin: 'pi pi-objects-column',
  rpc: 'pi pi-server',
  acp: 'pi pi-code',
  qq: 'pi pi-comments',
  telegram: 'pi pi-send',
  matrix: 'pi pi-table',
  discord: 'pi pi-discord',
}
function taskIcon(label: string): string {
  return taskIcons[label.split('.', 1)[0] ?? ''] ?? 'pi pi-bolt'
}

function formatTime(value: string | null): string {
  return value === null ? '—' : new Date(value).toLocaleString()
}
function elapsed(task: Task): string {
  const end = task.finishedAt === null ? Date.now() : Date.parse(task.finishedAt)
  const seconds = Math.max(0, Math.round((end - Date.parse(task.startedAt)) / 1_000))
  return seconds < 60 ? `${String(seconds)}s` : `${String(Math.floor(seconds / 60))}m ${String(seconds % 60)}s`
}
async function refresh(): Promise<void> {
  if (connection.state === 'opening' || connection.state === 'reconnecting') { loading.value = true; return }
  if (connection.state !== 'authenticated') {
    loading.value = false
    error.value = connection.error || 'Connect to cosmobot to load tasks.'
    return
  }
  loading.value = true
  const result = await runBackend(listTasks)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  tasks.value = [...result.value]
  loaded.value = true
  await selectTaskFromRoute()
}
async function selectTaskFromRoute(): Promise<void> {
  const rawId = route.params['taskId']
  if (typeof rawId !== 'string') return
  const id = Number(rawId)
  if (!Number.isSafeInteger(id) || id < 1) { error.value = 'Task ID must be a positive integer.'; return }
  if (!live.value) {
    selected.value = tasks.value.find((task) => task.id === id)
    associatedResources.value = []
    drawerOpen.value = selected.value !== undefined
    return
  }
  const [taskResult, resourcesResult] = await Promise.all([runBackend(lookupTask(id)), runBackend(listTaskResources(id))])
  if (taskResult._tag === 'Failure') { error.value = taskResult.error.message; return }
  if (taskResult.value === null) { error.value = `Task #${String(id)} was not found.`; drawerOpen.value = false; return }
  if (resourcesResult._tag === 'Failure') { error.value = resourcesResult.error.message; return }
  error.value = ''
  selected.value = taskResult.value
  associatedResources.value = [...resourcesResult.value]
  drawerOpen.value = true
}
function inspect(task: Task): void {
  void router.replace(`/tasks/${String(task.id)}`)
}
function closeDrawer(): void {
  selected.value = undefined
  associatedResources.value = []
  if (route.params['taskId'] !== undefined) void router.replace('/tasks')
}
async function doCancel(): Promise<void> {
  if (!selected.value || pending.value !== undefined) return
  const id = selected.value.id
  pending.value = 'cancel'
  const result = await runBackend(cancelTask(id))
  pending.value = undefined
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  toast.add({ severity: result.value ? 'success' : 'warn', summary: result.value ? `Task #${String(id)} cancelled` : `Task #${String(id)} was already finished or missing`, life: 3500 })
  await refresh()
}
function requestCancel(): void {
  if (!selected.value) return
  const { id, label } = selected.value
  confirm.require({ header: `Cancel task #${String(id)}?`, message: `Cancel “${label}”. Its work stops and cleanup runs; this cannot be resumed.`, rejectLabel: 'Keep running', acceptLabel: 'Cancel task', acceptClass: 'p-button-danger', accept: () => { void doCancel() } })
}
async function doDestroyAssociated(): Promise<void> {
  if (!selected.value || pending.value !== undefined) return
  const id = selected.value.id
  pending.value = 'destroy-associated'
  const result = await runBackend(destroyTaskResources(id))
  pending.value = undefined
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  const failures = result.value.filter(({ ok }) => !ok)
  toast.add({ severity: failures.length === 0 ? 'success' : 'warn', summary: failures.length === 0 ? `${String(result.value.length)} associated resources destroyed` : `${String(failures.length)} of ${String(result.value.length)} resources could not be destroyed`, life: 4000 })
  await refresh()
}
function requestDestroyAssociated(): void {
  if (!selected.value || associatedResources.value.length === 0) return
  const { id, label } = selected.value
  const targets = associatedResources.value.map((resource) => `“${resource.id}” (${resource.type})`).join(', ')
  confirm.require({ header: `Destroy ${String(associatedResources.value.length)} resources for task #${String(id)}?`, message: `Task “${label}” created: ${targets}. These resources become unavailable, active users are cancelled, and cleanup runs.`, rejectLabel: 'Keep resources', acceptLabel: 'Destroy listed resources', acceptClass: 'p-button-danger', accept: () => { void doDestroyAssociated() } })
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
watch(() => route.params['taskId'], () => { void selectTaskFromRoute() })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Runtime"
      title="Tasks"
      description="Inspect work managed by Cosmobot's concurrency manager."
    >
      <Button
        label="Refresh snapshot"
        severity="secondary"
        :loading="pending !== undefined"
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
      v-if="loading && !loaded"
      class="panel manager-loading"
      aria-label="Loading tasks"
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
        aria-label="Task summary"
      >
        <div><span class="summary-mark info"><i class="pi pi-spin pi-spinner" /></span><span><strong>{{ summary.running }}</strong><small>Running</small></span></div>
        <div><span class="summary-mark success"><i class="pi pi-check" /></span><span><strong>{{ summary.completed }}</strong><small>Completed</small></span></div>
        <div><span class="summary-mark danger"><i class="pi pi-times" /></span><span><strong>{{ summary.failed }}</strong><small>Failed</small></span></div>
        <div><span class="summary-mark neutral"><i class="pi pi-ban" /></span><span><strong>{{ summary.cancelled }}</strong><small>Cancelled</small></span></div>
      </div>
      <article class="panel manager-table">
        <div class="table-toolbar">
          <InputText
            v-model="query"
            placeholder="Filter by ID or label"
            aria-label="Filter tasks"
          />
          <Select
            v-model="statusFilter"
            :options="statusOptions"
            option-label="label"
            option-value="value"
            aria-label="Filter by status"
          />
        </div>
        <DataTable
          :value="filtered"
          data-key="id"
          selection-mode="single"
          :paginator="filtered.length > 25"
          :rows="25"
          :rows-per-page-options="[25, 50, 100, 200]"
          @row-select="inspect($event.data)"
        >
          <Column
            field="label"
            header="Task"
          >
            <template #body="{ data }">
              <span class="task-identity"><span class="task-type-icon"><i :class="taskIcon(data.label)" /></span><span><strong>{{ data.label }}</strong><small>Task #{{ data.id }}</small><small
                v-if="data.error"
                class="danger-text"
              >{{ data.error }}</small></span></span>
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
            field="startedAt"
            header="Started"
          >
            <template #body="{ data }">
              <time class="task-time">{{ formatTime(data.startedAt) }}</time>
            </template>
          </Column>
          <Column header="Elapsed">
            <template #body="{ data }">
              <span class="task-duration">{{ elapsed(data) }}</span>
            </template>
          </Column>
        </DataTable>
      </article>
    </template>
    <Drawer
      v-model:visible="drawerOpen"
      position="right"
      header="Task detail"
      aria-label="Task detail"
      :style="{ width: 'min(440px, 100vw)' }"
      @hide="closeDrawer"
    >
      <template v-if="selected">
        <div class="stack stack-loose">
          <header class="drawer-hero">
            <span class="platform-icon"><i class="pi pi-bolt" /></span><div><small>Task #{{ selected.id }}</small><h2>{{ selected.label }}</h2><StatusBadge :status="selected.status" /></div>
          </header>
          <Message
            v-if="selected.error"
            severity="error"
            :closable="false"
          >
            {{ selected.error }}
          </Message>
          <dl class="detail-list">
            <div><dt>Started</dt><dd>{{ formatTime(selected.startedAt) }}</dd></div>
            <div><dt>Finished</dt><dd>{{ formatTime(selected.finishedAt) }}</dd></div>
            <div><dt>Elapsed</dt><dd>{{ elapsed(selected) }}</dd></div>
          </dl>
          <section class="drawer-section stack stack-tight">
            <h3>Resources created by this task</h3>
            <ul
              v-if="associatedResources.length > 0"
              class="associated-resource-list"
            >
              <li
                v-for="resource in associatedResources"
                :key="resource.id"
              >
                <code>{{ resource.id }}</code><small>{{ resource.type }}</small>
              </li>
            </ul>
            <p
              v-else
              class="bounded-note"
            >
              No associated resources.
            </p>
          </section>
          <Message
            v-if="!live"
            severity="secondary"
            :closable="false"
          >
            Connect to a server with manager methods to control tasks.
          </Message>
          <footer class="drawer-actions stack stack-tight">
            <Button
              label="Cancel task"
              severity="danger"
              outlined
              :loading="pending === 'cancel'"
              :disabled="!live || selected.status !== 'running' || pending !== undefined"
              @click="requestCancel"
            />
            <Button
              label="Destroy resources created by this task"
              severity="danger"
              :loading="pending === 'destroy-associated'"
              :disabled="!live || selected.status === 'running' || associatedResources.length === 0 || pending !== undefined"
              @click="requestDestroyAssociated"
            />
          </footer>
        </div>
      </template>
    </Drawer>
  </section>
</template>
