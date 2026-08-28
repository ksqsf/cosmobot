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
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import { destroyResource, getResourceDetail, keepResourceAlive, listResources, makeResourcePermanent, renameResource } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { Resource } from '@/types/domain'
import { useConnectionStore } from '@/stores/connection'

const resourceMethods = ['resource.list', 'resource.detail', 'resource.destroy', 'resource.rename', 'resource.keep_alive', 'resource.make_permanent'] as const
type ProbeFilter = 'all' | 'healthy' | 'failed'
type PendingAction = 'detail' | 'rename' | 'keep-alive' | 'permanent' | 'destroy'
const resources = ref<Resource[]>([])
const selected = ref<Resource>()
const detail = ref('')
const visible = ref(false)
const query = ref('')
const typeFilter = ref('Any type')
const probeFilter = ref<ProbeFilter>('all')
const newId = ref('')
const error = ref('')
const loading = ref(true)
const pending = ref<PendingAction>()
const route = useRoute()
const router = useRouter()
const confirm = useConfirm()
const toast = useToast()
const connection = useConnectionStore()
const live = computed(() => connection.state === 'authenticated' && resourceMethods.every((method) => connection.methods.has(method)))
const summary = computed(() => ({
  total: resources.value.length,
  healthy: resources.value.filter(({ probe }) => probe.ok).length,
  expiring: resources.value.filter(({ remainingLifeMinutes }) => remainingLifeMinutes !== null).length,
  permanent: resources.value.filter(({ remainingLifeMinutes }) => remainingLifeMinutes === null).length,
}))
const typeOptions = computed(() => ['Any type', ...new Set(resources.value.map((resource) => resource.type))])
const probeOptions = [
  { label: 'Any health', value: 'all' },
  { label: 'Healthy', value: 'healthy' },
  { label: 'Probe failed', value: 'failed' },
] satisfies readonly { label: string; value: ProbeFilter }[]
const resourceIcons: Readonly<Record<string, string>> = {
  Workspace: 'pi pi-folder',
  Sandbox: 'pi pi-shield',
  Command: 'pi pi-desktop',
  PythonWorker: 'pi pi-code',
  SubAgent: 'pi pi-users',
}
function resourceIcon(type: string): string {
  return resourceIcons[type] ?? 'pi pi-box'
}
const filtered = computed(() => resources.value.filter((resource) =>
  `${resource.id} ${resource.type} ${resource.description}`.toLowerCase().includes(query.value.toLowerCase())
  && (typeFilter.value === 'Any type' || resource.type === typeFilter.value)
  && (probeFilter.value === 'all' || resource.probe.ok === (probeFilter.value === 'healthy')),
))

async function refresh(): Promise<void> {
  if (connection.state === 'opening' || connection.state === 'reconnecting') { loading.value = true; return }
  if (connection.state !== 'authenticated') {
    loading.value = false
    resources.value = []
    error.value = connection.error || 'Connect to cosmobot to load resources.'
    return
  }
  loading.value = true
  const result = await runBackend(listResources)
  loading.value = false
  if (result._tag === 'Failure') { resources.value = []; error.value = result.error.message; return }
  error.value = ''
  resources.value = [...result.value]
  await selectFromRoute()
}
async function selectFromRoute(): Promise<void> {
  const resourceId = route.params['resourceId']
  if (typeof resourceId !== 'string') return
  const resource = resources.value.find(({ id }) => id === resourceId)
  if (resource === undefined) { error.value = `Resource “${resourceId}” was not found.`; visible.value = false; return }
  selected.value = resource
  newId.value = resource.id
  visible.value = true
  if (!live.value) { detail.value = resource.description; return }
  pending.value = 'detail'
  const result = await runBackend(getResourceDetail(resource.id))
  pending.value = undefined
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  detail.value = result.value
}
function inspect(resource: Resource): void {
  void router.replace(`/resources/${encodeURIComponent(resource.id)}`)
}
function closeDrawer(): void {
  selected.value = undefined
  detail.value = ''
  if (route.params['resourceId'] !== undefined) void router.replace('/resources')
}
async function doRename(): Promise<void> {
  if (!selected.value || pending.value !== undefined) return
  const cleanId = newId.value.trim()
  if (cleanId.length === 0) { toast.add({ severity: 'warn', summary: 'Resource ID cannot be empty', life: 2500 }); return }
  const oldId = selected.value.id
  pending.value = 'rename'
  const result = await runBackend(renameResource(oldId, cleanId))
  pending.value = undefined
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  toast.add({ severity: 'success', summary: `Resource renamed to “${result.value}”`, life: 2500 })
  await router.replace(`/resources/${encodeURIComponent(result.value)}`)
  await refresh()
}
async function doKeepAlive(): Promise<void> {
  if (!selected.value || pending.value !== undefined) return
  const id = selected.value.id
  pending.value = 'keep-alive'
  const result = await runBackend(keepResourceAlive(id))
  pending.value = undefined
  if (result._tag === 'Failure') toast.add({ severity: 'error', summary: result.error.message, life: 3500 })
  else toast.add({ severity: 'success', summary: `Lifetime refreshed for “${id}”`, life: 2500 })
  await refresh()
}
async function doMakePermanent(): Promise<void> {
  if (!selected.value || pending.value !== undefined) return
  const id = selected.value.id
  pending.value = 'permanent'
  const result = await runBackend(makeResourcePermanent(id))
  pending.value = undefined
  if (result._tag === 'Failure') toast.add({ severity: 'error', summary: result.error.message, life: 3500 })
  else toast.add({ severity: 'success', summary: `Resource “${id}” is now permanent`, life: 2500 })
  await refresh()
}
async function doDestroy(): Promise<void> {
  if (!selected.value || pending.value !== undefined) return
  const id = selected.value.id
  pending.value = 'destroy'
  const result = await runBackend(destroyResource(id))
  pending.value = undefined
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  toast.add({ severity: 'success', summary: `Resource “${id}” destroyed`, life: 2500 })
  visible.value = false
  await router.replace('/resources')
  await refresh()
}
function requestDestroy(): void {
  if (!selected.value) return
  const { id, type } = selected.value
  confirm.require({ header: `Destroy resource “${id}”?`, message: `Destroy the ${type} resource “${id}”. It becomes unavailable, active users are cancelled, and cleanup runs.`, rejectLabel: 'Keep resource', acceptLabel: 'Destroy resource', acceptClass: 'p-button-danger', accept: () => { void doDestroy() } })
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
watch(() => route.params['resourceId'], () => { void selectFromRoute() })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Runtime"
      title="Resources"
      description="Inspect and manage long-running objects owned by Cosmobot."
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
      v-if="loading"
      class="panel manager-loading"
      aria-label="Loading resources"
    >
      <Skeleton
        v-for="index in 6"
        :key="index"
        height="3rem"
      />
    </article>
    <template v-else-if="!error">
      <div
        class="manager-summary"
        aria-label="Resource summary"
      >
        <div><span class="summary-mark violet"><i class="pi pi-box" /></span><span><strong>{{ summary.total }}</strong><small>Total</small></span></div>
        <div><span class="summary-mark success"><i class="pi pi-check-circle" /></span><span><strong>{{ summary.healthy }}</strong><small>Healthy</small></span></div>
        <div><span class="summary-mark warning"><i class="pi pi-clock" /></span><span><strong>{{ summary.expiring }}</strong><small>Expiring</small></span></div>
        <div><span class="summary-mark info"><i class="pi pi-lock" /></span><span><strong>{{ summary.permanent }}</strong><small>Permanent</small></span></div>
      </div>
      <article class="panel manager-table">
        <div class="table-toolbar resource-toolbar">
          <InputText
            v-model="query"
            placeholder="Filter by ID, type, or description"
            aria-label="Filter resources"
          />
          <div>
            <Select
              v-model="typeFilter"
              :options="typeOptions"
              aria-label="Filter by resource type"
            /><Select
              v-model="probeFilter"
              :options="probeOptions"
              option-label="label"
              option-value="value"
              aria-label="Filter by health"
            />
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
            header="ID"
          >
            <template #body="{ data }">
              <span class="resource-identity"><span class="resource-type-icon"><i :class="resourceIcon(data.type)" /></span><span><strong>{{ data.id }}</strong><small>{{ data.type }}</small></span></span>
            </template>
          </Column>
          <Column
            field="description"
            header="Description"
          />
          <Column header="Health">
            <template #body="{ data }">
              <Tag
                :value="data.probe.ok ? 'Healthy' : 'Probe failed'"
                :severity="data.probe.ok ? 'success' : 'danger'"
              />
            </template>
          </Column>
          <Column header="Lifetime">
            <template #body="{ data }">
              {{ data.remainingLifeMinutes === null ? 'Permanent' : `${data.remainingLifeMinutes} min` }}
            </template>
          </Column>
        </DataTable>
      </article>
    </template>
    <Drawer
      v-model:visible="visible"
      header="Resource detail"
      aria-label="Resource detail"
      position="right"
      :style="{ width: 'min(460px, 100vw)' }"
      @hide="closeDrawer"
    >
      <template v-if="selected">
        <div class="stack stack-loose">
          <header class="drawer-hero">
            <span class="platform-icon"><i :class="resourceIcon(selected.type)" /></span><div>
              <small>{{ selected.type }}</small><h2>{{ selected.id }}</h2><Tag
                :value="selected.probe.ok ? 'Healthy' : 'Probe failed'"
                :severity="selected.probe.ok ? 'success' : 'danger'"
              />
            </div>
          </header>
          <p>{{ selected.description }}</p>
          <Message
            v-if="!selected.probe.ok"
            severity="error"
            :closable="false"
          >
            {{ selected.probe.error }}
          </Message>
          <dl class="detail-list">
            <div><dt>Session</dt><dd><code>{{ selected.sessionId ?? 'None' }}</code></dd></div>
            <div><dt>Probe</dt><dd>{{ selected.probe.ok ? selected.probe.result : selected.probe.error }}</dd></div>
            <div><dt>Lifetime</dt><dd>{{ selected.remainingLifeMinutes === null ? 'Permanent' : `${selected.remainingLifeMinutes} minutes` }}</dd></div>
          </dl>
          <section class="drawer-section stack stack-tight">
            <h3>Detail</h3><p class="resource-detail">
              {{ pending === 'detail' ? 'Loading…' : detail }}
            </p>
          </section>
          <Message
            v-if="!live"
            severity="secondary"
            :closable="false"
          >
            Connect to a server with manager methods to modify resources.
          </Message>
          <section class="drawer-section stack stack-tight">
            <h3>Rename</h3>
            <div class="resource-rename">
              <InputText
                v-model="newId"
                aria-label="New resource ID"
              /><Button
                label="Rename"
                severity="secondary"
                :loading="pending === 'rename'"
                :disabled="!live || pending !== undefined || newId.trim() === selected.id"
                @click="doRename"
              />
            </div>
          </section>
          <footer class="drawer-actions stack stack-tight">
            <Button
              label="Refresh lifetime"
              severity="secondary"
              :loading="pending === 'keep-alive'"
              :disabled="!live || selected.remainingLifeMinutes === null || pending !== undefined"
              @click="doKeepAlive"
            />
            <Button
              label="Make permanent"
              severity="secondary"
              outlined
              :loading="pending === 'permanent'"
              :disabled="!live || selected.remainingLifeMinutes === null || pending !== undefined"
              @click="doMakePermanent"
            />
            <Button
              label="Destroy resource"
              severity="danger"
              :loading="pending === 'destroy'"
              :disabled="!live || pending !== undefined"
              @click="requestDestroy"
            />
          </footer>
        </div>
      </template>
    </Drawer>
  </section>
</template>
