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
import PageHeading from '@/components/PageHeading.vue'
import StatusBadge from '@/components/StatusBadge.vue'
import { listTasks } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { Status, Task } from '@/types/domain'
import { useConnectionStore } from '@/stores/connection'

const tasks = ref<Task[]>([])
const route = useRoute()
const router = useRouter()
const query = ref('')
type TaskFilter = 'active' | 'all' | Status
const statusFilter = ref<TaskFilter>('active')
const ownerFilter = ref('Any')
const selected = ref<Task>()
const drawerOpen = ref(false)
const confirm = useConfirm()
const toast = useToast()
const connection = useConnectionStore()
const live = computed(() => connection.state === 'authenticated' && connection.methods.has('concurrency.list'))
const ownerOptions = computed(() => ['Any', ...new Set(tasks.value.map(({ owner }) => owner))])
const filtered = computed(() => tasks.value.filter((task) =>
  `${task.id} ${task.label} ${task.owner}`.toLowerCase().includes(query.value.toLowerCase())
  && matchesStatus(task, statusFilter.value)
  && (ownerFilter.value === 'Any' || task.owner === ownerFilter.value),
))
const statusOptions = [
  { label: 'Active', value: 'active' },
  { label: 'All', value: 'all' },
  { label: 'Running', value: 'running' },
  { label: 'Waiting', value: 'waiting' },
  { label: 'Failed', value: 'failed' },
] satisfies readonly { label: string; value: TaskFilter }[]
function matchesStatus(task: Task, filter: TaskFilter): boolean {
  if (filter === 'all') return true
  if (filter === 'active') return task.status === 'running' || task.status === 'waiting'
  return task.status === filter
}
async function refresh(): Promise<void> {
  const result = await runBackend(listTasks)
  if (result._tag === 'Success') {
    tasks.value = [...result.value]
    selectTaskFromRoute()
  }
}
onMounted(refresh)
watch(() => connection.state, (state) => { if (state === 'authenticated') void refresh() })
function selectTaskFromRoute(): void {
  const taskId = route.params['taskId']
  if (typeof taskId !== 'string') return
  const task = tasks.value.find(({ id }) => id === taskId)
  if (task !== undefined) { selected.value = task; drawerOpen.value = true }
}
function inspect(task: Task): void {
  selected.value = task
  drawerOpen.value = true
  void router.replace(`/tasks/${task.id}`)
}
function closeDrawer(): void {
  selected.value = undefined
  if (route.params['taskId'] !== undefined) void router.replace('/tasks')
}
watch(() => route.params['taskId'], selectTaskFromRoute)
function cancelTask(): void {
  if (!selected.value) return
  confirm.require({ header: `Cancel task #${selected.value.id}?`, message: 'This changes fixture state only.', rejectLabel: 'Keep running', acceptLabel: 'Cancel task', acceptClass: 'p-button-danger', accept: () => { if (selected.value) selected.value.status = 'stopped'; drawerOpen.value = false; toast.add({ severity: 'success', summary: 'Fixture task cancelled', life: 2500 }) } })
}
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Runtime"
      title="Tasks"
      description="Inspect owned work and safely control task lifecycles."
    >
      <Button
        label="Refresh snapshot"
        severity="secondary"
        @click="refresh"
      />
    </PageHeading>
    <article class="panel">
      <div class="table-toolbar">
        <InputText
          v-model="query"
          placeholder="Filter by ID, label, or owner"
          aria-label="Filter tasks"
        /><div>
          <Select
            v-model="statusFilter"
            :options="statusOptions"
            option-label="label"
            option-value="value"
            aria-label="Filter by status"
          /><Select
            v-model="ownerFilter"
            :options="ownerOptions"
            aria-label="Filter by owner"
          />
        </div>
      </div>
      <DataTable
        :value="filtered"
        data-key="id"
        paginator
        :rows="5"
        @row-select="inspect($event.data)"
      >
        <Column
          field="id"
          header="ID"
        >
          <template #body="{ data }">
            #{{ data.id }}
          </template>
        </Column><Column
          field="label"
          header="Task"
        >
          <template #body="{ data }">
            <strong>{{ data.label }}</strong><small class="block">{{ data.detail }}</small>
          </template>
        </Column><Column
          field="owner"
          header="Owner"
        /><Column
          field="status"
          header="Status"
        >
          <template #body="{ data }">
            <StatusBadge :status="data.status" />
          </template>
        </Column><Column
          field="started"
          header="Started"
        /><Column
          field="elapsed"
          header="Elapsed"
        />
      </DataTable>
    </article><Message
      severity="secondary"
      :closable="false"
    >
      Only cooperatively pausable tasks expose Pause. Arbitrary suspension can freeze locks or resources.
    </Message>
    <Drawer
      v-model:visible="drawerOpen"
      position="right"
      header="Task detail"
      aria-label="Task detail"
      :style="{ width: 'min(420px, 100vw)' }"
      @hide="closeDrawer"
    >
      <template v-if="selected">
        <div class="stack stack-loose">
          <header class="drawer-hero">
            <span class="platform-icon">{{ selected.platform[0] }}</span>
            <div>
              <small>Task #{{ selected.id }}</small>
              <h2>{{ selected.label }}</h2>
              <StatusBadge :status="selected.status" />
            </div>
          </header>
          <p class="drawer-description">
            {{ selected.detail }}
          </p>
          <section
            class="drawer-section stack stack-tight"
            aria-labelledby="task-properties"
          >
            <h3 id="task-properties">
              Properties
            </h3>
            <dl class="detail-list">
              <div><dt>Owner</dt><dd><code>{{ selected.owner }}</code></dd></div>
              <div><dt>Started</dt><dd>{{ selected.started }}</dd></div>
              <div><dt>Elapsed</dt><dd>{{ selected.elapsed }}</dd></div>
            </dl>
          </section>
          <footer class="drawer-actions stack stack-tight">
            <Message
              severity="info"
              :closable="false"
              size="small"
            >
              {{ live ? 'Task control is not enabled in this phase.' : 'Demo action — no task will be affected.' }}
            </Message>
            <Button
              label="Cancel task"
              severity="danger"
              fluid
              :disabled="live"
              @click="cancelTask"
            />
          </footer>
        </div>
      </template>
    </Drawer>
  </section>
</template>
