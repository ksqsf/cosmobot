<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
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
import type { Task } from '@/types/domain'
import { useConnectionStore } from '@/stores/connection'

const tasks = ref<Task[]>([])
const query = ref('')
const statusFilter = ref('Active')
const ownerFilter = ref('Any')
const checkedTasks = ref<Task[]>([])
const selected = ref<Task>()
const drawerOpen = ref(false)
const confirm = useConfirm()
const toast = useToast()
const connection = useConnectionStore()
const live = computed(() => connection.state === 'authenticated' && connection.methods.has('concurrency.list'))
const filtered = computed(() => tasks.value.filter((task) =>
  `${task.id} ${task.label} ${task.owner}`.toLowerCase().includes(query.value.toLowerCase())
  && (statusFilter.value === 'All' || statusFilter.value === 'Active' && task.status !== 'failed' && task.status !== 'stopped' || task.status === statusFilter.value.toLowerCase())
  && (ownerFilter.value === 'Any' || task.owner === ownerFilter.value),
))
async function refresh(): Promise<void> {
  const result = await runBackend(listTasks)
  if (result._tag === 'Success') tasks.value = [...result.value]
}
onMounted(refresh)
watch(() => connection.state, (state) => { if (state === 'authenticated') void refresh() })
function inspect(task: Task): void { selected.value = task; drawerOpen.value = true }
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
            :options="['Active', 'All', 'Running', 'Waiting', 'Failed']"
            aria-label="Filter by status"
          /><Select
            v-model="ownerFilter"
            :options="['Any', 'system', 'run_8f2c', 'run_2e09']"
            aria-label="Filter by owner"
          />
        </div>
      </div>
      <DataTable
        v-model:selection="checkedTasks"
        :value="filtered"
        data-key="id"
        paginator
        :rows="5"
        selection-mode="single"
        @row-select="inspect($event.data)"
      >
        <Column
          selection-mode="multiple"
          header-style="width: 3rem"
        /><Column
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
          <section
            class="drawer-section stack stack-tight"
            aria-labelledby="task-activity"
          >
            <h3 id="task-activity">
              Activity
            </h3>
            <ol class="mini-timeline">
              <li><strong>Task registered</strong><small>14:28:16.101</small></li>
              <li><strong>Model turn 2</strong><small>14:28:21.927</small></li>
              <li><strong>Running tool</strong><small><code>run_test</code> · 2.3s</small></li>
            </ol>
          </section>
          <section
            class="drawer-section stack stack-tight"
            aria-labelledby="related-resources"
          >
            <h3 id="related-resources">
              Related resources
            </h3>
            <div class="resource-row">
              <span class="platform-icon">W</span>
              <span><strong>Workspace</strong><small>cosmobot</small></span>
              <StatusBadge status="running" />
            </div>
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
