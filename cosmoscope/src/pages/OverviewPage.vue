<script setup lang="ts">
import { onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import Button from 'primevue/button'
import DataTable from 'primevue/datatable'
import Column from 'primevue/column'
import Drawer from 'primevue/drawer'
import Message from 'primevue/message'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import FixtureState from '@/components/FixtureState.vue'
import StatusBadge from '@/components/StatusBadge.vue'
import { getOverview } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { FixtureScenario, OverviewSnapshot, Task } from '@/types/domain'

const route = useRoute()
const router = useRouter()
const scenario = ref<FixtureScenario>((route.query['scenario'] as FixtureScenario | undefined) ?? 'ready')
const state = ref<FixtureScenario>('loading')
const error = ref('')
const snapshot = ref<OverviewSnapshot>({ tasks: [], activity: [], platforms: [] })
const selectedTask = ref<Task>()
const drawerOpen = ref(false)

async function load(): Promise<void> {
  state.value = 'loading'; error.value = ''
  const result = await runBackend(getOverview(scenario.value))
  if (result._tag === 'Success') {
    snapshot.value = result.value
    state.value = scenario.value === 'empty' ? 'empty' : 'ready'
    return
  }
  error.value = result.error.message
  state.value = result.error._tag === 'OfflineError' ? 'offline' : result.error._tag === 'ForbiddenError' ? 'forbidden' : 'error'
}
function inspect(task: Task): void { selectedTask.value = task; drawerOpen.value = true }
watch(scenario, (value) => { void router.replace({ query: value === 'ready' ? {} : { scenario: value } }); void load() })
onMounted(load)
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Friday, 28 August"
      title="Good afternoon, kosmos."
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
    <FixtureState
      :state="state"
      :message="error"
      @retry="scenario = 'ready'"
    >
      <div class="metric-grid">
        <article class="metric">
          <div class="metric-top">
            <span class="pi pi-bolt metric-icon violet" /><span class="trend">Live</span>
          </div><strong>17</strong><p>Active tasks</p><small>3 started in the last minute</small>
        </article>
        <article class="metric">
          <div class="metric-top">
            <span class="pi pi-comments metric-icon green" /><span class="trend positive">+8.4%</span>
          </div><strong>1,284</strong><p>Messages today</p><small>Across 4 platforms</small>
        </article>
        <article class="metric">
          <div class="metric-top">
            <span class="pi pi-objects-column metric-icon blue" /><span class="trend">Stable</span>
          </div><strong>6 / 7</strong><p>Plugins loaded</p><small>Weather plugin is stopped</small>
        </article>
        <article class="metric">
          <div class="metric-top">
            <span class="pi pi-exclamation-triangle metric-icon red" /><span class="trend negative">Needs review</span>
          </div><strong>3</strong><p>Recent failures</p><small>Last failure 4 minutes ago</small>
        </article>
      </div>
      <div class="overview-grid">
        <article class="panel">
          <div class="panel-heading">
            <div><h2>Active tasks</h2><p>Live work managed by Concurrency</p></div><Button
              label="View all"
              text
              @click="router.push('/tasks')"
            />
          </div>
          <DataTable
            :value="snapshot.tasks.slice(0, 4)"
            data-key="id"
            selection-mode="single"
            @row-select="inspect($event.data)"
          >
            <Column
              field="label"
              header="Task"
            >
              <template #body="{ data }">
                <span class="task-name"><span class="platform-icon">{{ data.platform[0] }}</span><span><strong>{{ data.label }}</strong><small>#{{ data.id }} · {{ data.platform }}</small></span></span>
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
              field="started"
              header="Started"
            /><Column
              field="elapsed"
              header="Elapsed"
            />
          </DataTable>
        </article>
        <article class="panel activity-panel">
          <div class="panel-heading">
            <div><h2>Live activity</h2><p>Audit and system events</p></div><Tag
              value="Live"
              severity="success"
            />
          </div>
          <ol class="activity-list">
            <li
              v-for="item in snapshot.activity"
              :key="item.id"
            >
              <i
                class="pi pi-circle-fill"
                :class="item.tone"
              /><div><p><strong>{{ item.kind }}</strong> {{ item.summary }}</p><small>{{ item.source }} · {{ item.time }}</small></div>
            </li>
          </ol>
        </article>
      </div>
      <article class="panel platform-panel">
        <div class="panel-heading">
          <div><h2>Platforms</h2><p>Connection and message activity</p></div><span class="updated">Updated just now</span>
        </div><div class="platform-grid">
          <div
            v-for="platform in snapshot.platforms"
            :key="platform.id"
            class="platform-card"
          >
            <span class="platform-icon">{{ platform.name[0] }}</span><div><strong>{{ platform.name }}</strong><small><StatusBadge :status="platform.state" /> · {{ platform.messages }} messages</small></div><span
              class="sparkline"
              aria-hidden="true"
            >▂▄▃▆▅▇</span>
          </div>
        </div>
      </article>
    </FixtureState>
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
            <span class="platform-icon">{{ selectedTask.platform[0] }}</span><div><h2>{{ selectedTask.label }}</h2><StatusBadge :status="selectedTask.status" /></div>
          </div>
          <dl class="detail-list">
            <div><dt>ID</dt><dd>#{{ selectedTask.id }}</dd></div><div><dt>Owner</dt><dd><code>{{ selectedTask.owner }}</code></dd></div><div><dt>Started</dt><dd>{{ selectedTask.started }}</dd></div><div><dt>Elapsed</dt><dd>{{ selectedTask.elapsed }}</dd></div>
          </dl>
          <section class="drawer-section stack stack-tight">
            <h3>Activity</h3><ol class="mini-timeline">
              <li><strong>Task registered</strong><small>14:28:16.101</small></li><li><strong>Model turn 2</strong><small>14:28:21.927</small></li><li><strong>Running tool</strong><small><code>run_test</code> · 2.3s</small></li>
            </ol>
          </section>
          <Message
            severity="secondary"
            :closable="false"
          >
            Demo action — no task will be affected.
          </Message>
        </div>
      </template>
    </Drawer>
  </section>
</template>
