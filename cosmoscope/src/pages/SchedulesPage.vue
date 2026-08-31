<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Drawer from 'primevue/drawer'
import FloatLabel from 'primevue/floatlabel'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import MultiSelect from 'primevue/multiselect'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import RunIdLink from '@/components/RunIdLink.vue'
import { deleteSchedule, listSchedules } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { Schedule } from '@/types/domain'
import { useConnectionStore } from '@/stores/connection'
import { useLayeredConfirm, useOverlayLayer } from '@/overlay'

type CadenceFilter = 'one-shot' | 'recurring'
const schedules = ref<Schedule[]>([])
const selected = ref<Schedule>()
const visible = ref(false)
const query = ref('')
const platformFilters = ref<string[]>([])
const cadenceFilters = ref<CadenceFilter[]>([])
const error = ref('')
const loading = ref(true)
const loaded = ref(false)
const deleting = ref(false)
const route = useRoute()
const router = useRouter()
const connection = useConnectionStore()
const confirm = useLayeredConfirm()
const toast = useToast()
const { isTop: drawerIsTop } = useOverlayLayer(visible)
const summary = computed(() => ({
  total: schedules.value.length,
  recurring: schedules.value.filter(({ intervalSeconds }) => intervalSeconds !== null).length,
  oneShot: schedules.value.filter(({ intervalSeconds }) => intervalSeconds === null).length,
  owners: new Set(schedules.value.map(({ ownerId }) => ownerId).filter((owner) => owner !== null)).size,
}))
const platformOptions = computed(() => [...new Set(schedules.value.map(({ platform }) => platform))])
const cadenceOptions = [
  { label: 'One-shot', value: 'one-shot' },
  { label: 'Recurring', value: 'recurring' },
] satisfies readonly { label: string; value: CadenceFilter }[]
const filtered = computed(() => schedules.value.filter((schedule) =>
  `${String(schedule.id)} ${schedule.prompt} ${schedule.platform} ${schedule.chatId ?? ''} ${schedule.ownerId ?? ''} ${schedule.runId ?? ''}`.toLowerCase().includes(query.value.toLowerCase())
  && (platformFilters.value.length === 0 || platformFilters.value.includes(schedule.platform))
  && (cadenceFilters.value.length === 0 || cadenceFilters.value.includes(schedule.intervalSeconds === null ? 'one-shot' : 'recurring')),
))

function remaining(seconds: number): string {
  if (seconds < 60) return `${String(seconds)}s`
  if (seconds < 3600) return `${String(Math.ceil(seconds / 60))}m`
  if (seconds < 86400) return `${String(Math.ceil(seconds / 3600))}h`
  return `${String(Math.ceil(seconds / 86400))}d`
}
async function refresh(): Promise<void> {
  if (connection.state === 'opening' || connection.state === 'reconnecting') { loading.value = true; return }
  if (connection.state !== 'authenticated') {
    loading.value = false
    error.value = connection.error || 'Connect to cosmobot to load schedules.'
    return
  }
  loading.value = true
  const result = await runBackend(listSchedules)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  schedules.value = [...result.value]
  loaded.value = true
  selectFromRoute()
}
function selectFromRoute(): void {
  const raw = route.params['scheduleId']
  if (typeof raw !== 'string') { selected.value = undefined; visible.value = false; return }
  const id = Number(raw)
  const schedule = schedules.value.find((candidate) => candidate.id === id)
  if (schedule === undefined) { error.value = `Schedule #${raw} was not found.`; visible.value = false; return }
  selected.value = schedule
  visible.value = true
}
function inspect(schedule: Schedule): void {
  void router.replace({ name: 'schedules', params: { scheduleId: String(schedule.id) } })
}
function closeDrawer(): void {
  selected.value = undefined
  if (route.params['scheduleId'] !== undefined) void router.replace({ name: 'schedules' })
}
async function doDelete(): Promise<void> {
  if (!selected.value || deleting.value) return
  const id = selected.value.id
  deleting.value = true
  const result = await runBackend(deleteSchedule(id))
  deleting.value = false
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  toast.add(result.value
    ? { severity: 'success', summary: `Schedule #${String(id)} deleted`, life: 2500 }
    : { severity: 'warn', summary: `Schedule #${String(id)} no longer exists`, life: 2500 })
  visible.value = false
  await router.replace({ name: 'schedules' })
  await refresh()
}
function requestDelete(): void {
  if (!selected.value) return
  const id = selected.value.id
  confirm.require({ header: `Delete schedule #${String(id)}?`, message: 'The pending action will not run. This cannot be undone.', rejectLabel: 'Keep schedule', acceptLabel: 'Delete schedule', acceptClass: 'p-button-danger', accept: () => { void doDelete() } })
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
watch(() => route.params['scheduleId'], selectFromRoute)
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Runtime"
      title="Schedules"
      description="Inspect pending one-shot and recurring agent actions."
    >
      <Button
        label="Refresh snapshot"
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
      v-if="loading && !loaded"
      class="panel manager-loading"
      aria-label="Loading schedules"
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
        aria-label="Schedule summary"
      >
        <div><span class="summary-mark violet"><i class="pi pi-calendar-clock" /></span><span><strong>{{ summary.total }}</strong><small>Total</small></span></div>
        <div><span class="summary-mark info"><i class="pi pi-replay" /></span><span><strong>{{ summary.recurring }}</strong><small>Recurring</small></span></div>
        <div><span class="summary-mark success"><i class="pi pi-clock" /></span><span><strong>{{ summary.oneShot }}</strong><small>One-shot</small></span></div>
        <div><span class="summary-mark neutral"><i class="pi pi-user" /></span><span><strong>{{ summary.owners }}</strong><small>Owners</small></span></div>
      </div>
      <article class="panel manager-table">
        <div class="table-toolbar resource-toolbar">
          <InputText
            v-model="query"
            placeholder="Filter by prompt, chat, owner, or run"
            aria-label="Filter schedules"
          />
          <div>
            <FloatLabel variant="on">
              <MultiSelect
                v-model="platformFilters"
                :options="platformOptions"
                input-id="schedule-platform-filter"
                display="chip"
                placeholder="All"
              />
              <label for="schedule-platform-filter">Platform</label>
            </FloatLabel>
            <FloatLabel variant="on">
              <MultiSelect
                v-model="cadenceFilters"
                :options="cadenceOptions"
                input-id="schedule-cadence-filter"
                option-label="label"
                option-value="value"
                display="chip"
                placeholder="All"
              />
              <label for="schedule-cadence-filter">Cadence</label>
            </FloatLabel>
          </div>
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
            field="id"
            header="Schedule"
          >
            <template #body="{ data }">
              <span class="manager-identity"><span class="manager-type-icon"><i class="pi pi-calendar-clock" /></span><span><strong>#{{ data.id }}</strong><small>{{ data.intervalSeconds === null ? 'One-shot' : 'Recurring' }}</small></span></span>
            </template>
          </Column>
          <Column
            field="prompt"
            header="Prompt"
          />
          <Column
            field="platform"
            header="Platform"
          />
          <Column
            field="chatId"
            header="Chat"
          />
          <Column
            field="ownerId"
            header="Owner"
          />
          <Column header="Due">
            <template #body="{ data }">
              {{ remaining(data.remainingSeconds) }}
            </template>
          </Column>
        </DataTable>
      </article>
    </template>
    <Drawer
      v-model:visible="visible"
      header="Schedule detail"
      aria-label="Schedule detail"
      position="right"
      :style="{ width: 'min(460px, 100vw)' }"
      :close-on-escape="drawerIsTop"
      @hide="closeDrawer"
    >
      <template v-if="selected">
        <div class="stack stack-loose">
          <header class="drawer-hero">
            <span class="platform-icon"><i class="pi pi-calendar-clock" /></span><div>
              <small>Schedule</small><h2>#{{ selected.id }}</h2><Tag
                :value="selected.intervalSeconds === null ? 'One-shot' : 'Recurring'"
                :severity="selected.intervalSeconds === null ? 'secondary' : 'info'"
              />
            </div>
          </header>
          <section class="drawer-section stack stack-tight">
            <h3>Prompt</h3><p class="resource-detail">
              {{ selected.prompt }}
            </p>
          </section>
          <dl class="detail-list">
            <div><dt>Platform</dt><dd>{{ selected.platform }}</dd></div>
            <div><dt>Chat</dt><dd><code>{{ selected.chatId ?? 'Unknown' }}</code></dd></div>
            <div><dt>Owner</dt><dd><code>{{ selected.ownerId ?? 'Unknown' }}</code></dd></div>
            <div><dt>Due in</dt><dd>{{ remaining(selected.remainingSeconds) }}</dd></div>
            <div v-if="selected.intervalSeconds">
              <dt>Repeats every</dt><dd>{{ selected.intervalSeconds }} seconds</dd>
            </div>
            <div>
              <dt>Agent run</dt><dd>
                <RunIdLink
                  v-if="selected.runId && connection.methods.has('thread.resolve_run')"
                  :run-id="selected.runId"
                /><code v-else-if="selected.runId">{{ selected.runId }}</code><span v-else>Unavailable</span>
              </dd>
            </div>
          </dl>
          <Message
            v-if="!selected.runId"
            severity="secondary"
            :closable="false"
          >
            This schedule is not linked to an agent run.
          </Message>
          <Button
            label="Delete schedule"
            icon="pi pi-trash"
            severity="danger"
            :loading="deleting"
            :disabled="connection.state !== 'authenticated' || !connection.methods.has('schedule.delete')"
            @click="requestDelete"
          />
        </div>
      </template>
    </Drawer>
  </section>
</template>
