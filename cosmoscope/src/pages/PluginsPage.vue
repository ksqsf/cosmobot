<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Dialog from 'primevue/dialog'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import { listPlugins, loadPlugin, reloadPlugin, unloadPlugin } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useConnectionStore } from '@/stores/connection'
import type { Plugin } from '@/types/domain'
import { useLayeredConfirm, useOverlayLayer } from '@/overlay'

type PendingAction = 'load' | `reload:${string}` | `unload:${string}`
const plugins = ref<Plugin[]>([])
const loading = ref(true)
const loaded = ref(false)
const error = ref('')
const loadVisible = ref(false)
const loadId = ref('')
const pending = ref<PendingAction>()
const connection = useConnectionStore()
const confirm = useLayeredConfirm()
const toast = useToast()
const { isTop: loadDialogIsTop } = useOverlayLayer(loadVisible)
const totals = computed(() => plugins.value.reduce((summary, plugin) => ({
  loaded: summary.loaded + 1,
  tools: summary.tools + plugin.tools,
  routes: summary.routes + plugin.routes,
  required: summary.required + Number(plugin.required),
}), { loaded: 0, tools: 0, routes: 0, required: 0 }))

function showError(message: string): void {
  error.value = message
  toast.add({ severity: 'error', summary: message, life: 4000 })
}

async function refresh(): Promise<void> {
  if (connection.state === 'opening' || connection.state === 'reconnecting') { loading.value = true; return }
  if (connection.state !== 'authenticated') {
    loading.value = false
    error.value = connection.error || 'Connect to cosmobot to load plugins.'
    return
  }
  loading.value = true
  const result = await runBackend(listPlugins)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  plugins.value = [...result.value]
  loaded.value = true
}

async function doLoad(): Promise<void> {
  const id = loadId.value.trim()
  if (id.length === 0 || pending.value !== undefined) return
  pending.value = 'load'
  const result = await runBackend(loadPlugin(id))
  pending.value = undefined
  if (result._tag === 'Failure') { showError(result.error.message); return }
  loadVisible.value = false
  loadId.value = ''
  toast.add({ severity: 'success', summary: `Loaded “${id}” generation ${String(result.value.generation)}`, life: 3000 })
  await refresh()
}

async function doReload(plugin: Plugin): Promise<void> {
  if (pending.value !== undefined) return
  pending.value = `reload:${plugin.id}`
  const result = await runBackend(reloadPlugin(plugin.id))
  pending.value = undefined
  if (result._tag === 'Failure') { showError(result.error.message); return }
  toast.add({ severity: 'success', summary: `Reloaded “${plugin.id}”: generation ${String(plugin.generation)} → ${String(result.value.generation)}`, life: 3500 })
  await refresh()
}

async function doUnload(plugin: Plugin): Promise<void> {
  if (pending.value !== undefined) return
  pending.value = `unload:${plugin.id}`
  const result = await runBackend(unloadPlugin(plugin.id))
  pending.value = undefined
  if (result._tag === 'Failure') { showError(result.error.message); return }
  toast.add({ severity: 'success', summary: `Unloaded “${plugin.id}”`, life: 3000 })
  await refresh()
}

function requestUnload(plugin: Plugin): void {
  confirm.require({
    header: `Unload plugin “${plugin.id}”?`,
    message: 'Its routes and tools will become unavailable immediately.',
    rejectLabel: 'Keep loaded',
    acceptLabel: 'Unload plugin',
    acceptClass: 'p-button-danger',
    accept: () => { void doUnload(plugin) },
  })
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Extensions"
      title="Plugins"
      description="Inspect and manage plugins currently loaded by Cosmobot."
    >
      <Button
        label="Load plugin"
        icon="pi pi-plus"
        @click="loadVisible = true"
      />
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
      :closable="true"
      @close="error = ''"
    >
      {{ error }}
    </Message>
    <article
      v-if="loading && !loaded"
      class="panel manager-loading"
      aria-label="Loading plugins"
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
        aria-label="Loaded plugin summary"
      >
        <div><span class="summary-mark success"><i class="pi pi-check-circle" /></span><span><strong>{{ totals.loaded }}</strong><small>Loaded</small></span></div>
        <div><span class="summary-mark warning"><i class="pi pi-lock" /></span><span><strong>{{ totals.required }}</strong><small>Required</small></span></div>
        <div><span class="summary-mark violet"><i class="pi pi-wrench" /></span><span><strong>{{ totals.tools }}</strong><small>Tools</small></span></div>
        <div><span class="summary-mark info"><i class="pi pi-directions" /></span><span><strong>{{ totals.routes }}</strong><small>Routes</small></span></div>
      </div>
      <Message
        v-if="plugins.length === 0 && !error"
        severity="secondary"
        :closable="false"
      >
        No plugins are loaded.
      </Message>
      <article
        v-if="plugins.length > 0"
        class="panel manager-table"
      >
        <DataTable
          :value="plugins"
          data-key="id"
          :paginator="plugins.length > 25"
          :rows="25"
          :rows-per-page-options="[25, 50, 100, 200]"
        >
          <Column
            field="id"
            header="Plugin"
          >
            <template #body="{ data }">
              <strong><code>{{ data.id }}</code></strong>
            </template>
          </Column>
          <Column
            field="version"
            header="Version"
          >
            <template #body="{ data }">
              v{{ data.version }}
            </template>
          </Column>
          <Column
            field="generation"
            header="Generation"
          />
          <Column header="Policy">
            <template #body="{ data }">
              <span class="cluster">
                <Tag
                  :value="data.required ? 'Required' : 'Optional'"
                  :severity="data.required ? 'warn' : 'secondary'"
                />
                <Tag
                  :value="data.sandboxed ? 'Sandboxed' : 'Unsandboxed'"
                  :severity="data.sandboxed ? 'info' : 'warn'"
                />
              </span>
            </template>
          </Column>
          <Column header="Capabilities">
            <template #body="{ data }">
              {{ data.tools }} tools · {{ data.routes }} routes
            </template>
          </Column>
          <Column header="Actions">
            <template #body="{ data: plugin }">
              <span class="cluster">
                <Button
                  label="Reload"
                  icon="pi pi-refresh"
                  severity="secondary"
                  size="small"
                  :loading="pending === `reload:${plugin.id}`"
                  :disabled="pending !== undefined"
                  @click="doReload(plugin)"
                />
                <Button
                  label="Unload"
                  icon="pi pi-eject"
                  severity="danger"
                  variant="text"
                  size="small"
                  :loading="pending === `unload:${plugin.id}`"
                  :disabled="plugin.required || pending !== undefined"
                  @click="requestUnload(plugin)"
                />
              </span>
            </template>
          </Column>
        </DataTable>
      </article>
    </template>
    <Dialog
      v-model:visible="loadVisible"
      modal
      header="Load plugin"
      :style="{ width: '28rem' }"
      :close-on-escape="loadDialogIsTop"
    >
      <div class="stack">
        <p>Enter the ID of a plugin installed in the server plugin directory.</p>
        <InputText
          v-model="loadId"
          autofocus
          placeholder="plugin-id"
          aria-label="Plugin ID"
          @keyup.enter="doLoad"
        />
      </div>
      <template #footer>
        <Button
          label="Cancel"
          severity="secondary"
          variant="text"
          @click="loadVisible = false"
        />
        <Button
          label="Load plugin"
          :loading="pending === 'load'"
          :disabled="loadId.trim().length === 0"
          @click="doLoad"
        />
      </template>
    </Dialog>
  </section>
</template>
