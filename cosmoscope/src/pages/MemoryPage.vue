<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Dialog from 'primevue/dialog'
import Drawer from 'primevue/drawer'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import DisplayIdentity from '@/components/DisplayIdentity.vue'
import { getMemory, getMemoryHistory, getMemoryRevision, listMemories, revertMemory } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { renderMarkdown } from '@/markdown'
import { useConnectionStore } from '@/stores/connection'
import { useLayeredConfirm, useOverlayLayer } from '@/overlay'
import type { MemoryDetail, MemoryHistoryEntry, MemorySummary } from '@/types/domain'

const memories = ref<readonly MemorySummary[]>([])
const selected = ref<MemoryDetail | null>(null)
const history = ref<readonly MemoryHistoryEntry[]>([])
const historicContent = ref<string | null>()
const historicEntry = ref<MemoryHistoryEntry>()
const query = ref('')
const loading = ref(true)
const loaded = ref(false)
const detailLoading = ref(false)
const reverting = ref('')
const historyLoading = ref(false)
const error = ref('')
const connection = useConnectionStore()
const confirm = useLayeredConfirm()
const toast = useToast()
let detailRequest = 0
let revisionRequest = 0
const drawerVisible = computed({
  get: () => selected.value !== null,
  set: (visible) => {
    if (!visible) {
      detailRequest += 1
      selected.value = null
    }
  },
})
const historyDialogVisible = computed({
  get: () => historicEntry.value !== undefined,
  set: (visible) => {
    if (!visible) {
      revisionRequest += 1
      historicEntry.value = undefined
    }
  },
})
const { isTop: drawerIsTop } = useOverlayLayer(drawerVisible)
const { isTop: revisionIsTop } = useOverlayLayer(historyDialogVisible)
const filtered = computed(() => {
  const needle = query.value.trim().toLocaleLowerCase()
  return needle === '' ? memories.value : memories.value.filter((memory) =>
    `${memory.platform} ${memory.scope} ${memory.scopeId} ${memory.username ?? ''} ${memory.displayName ?? ''}`.toLocaleLowerCase().includes(needle))
})

async function refresh(): Promise<void> {
  if (connection.state !== 'authenticated') {
    loading.value = false
    error.value = connection.error || 'Connect to cosmobot to load memory.'
    return
  }
  loading.value = true
  const result = await runBackend(listMemories)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  memories.value = result.value
  loaded.value = true
  const inspected = selected.value
  if (inspected !== null) {
    const current = result.value.find((memory) => sameMemory(memory, inspected))
    if (current === undefined) {
      detailRequest += 1
      selected.value = null
    }
    else await inspect(current)
  }
}

function sameMemory(left: MemorySummary, right: MemorySummary): boolean {
  return left.platform === right.platform && left.scope === right.scope && left.scopeId === right.scopeId
}

async function inspect(memory: MemorySummary): Promise<void> {
  const request = ++detailRequest
  selected.value = { ...memory, content: '' }
  history.value = []
  detailLoading.value = true
  const [detailResult, historyResult] = await Promise.all([
    runBackend(getMemory(memory)),
    runBackend(getMemoryHistory(memory)),
  ])
  if (request !== detailRequest) return
  detailLoading.value = false
  if (detailResult._tag === 'Failure') { error.value = detailResult.error.message; selected.value = null; return }
  selected.value = detailResult.value
  if (historyResult._tag === 'Failure') { error.value = historyResult.error.message; return }
  history.value = historyResult.value
}

async function inspectRevision(entry: MemoryHistoryEntry): Promise<void> {
  const memory = selected.value
  if (memory === null) return
  const request = ++revisionRequest
  historicEntry.value = entry
  historicContent.value = undefined
  historyLoading.value = true
  const result = await runBackend(getMemoryRevision(memory, entry.revision))
  if (request !== revisionRequest) return
  historyLoading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; historicEntry.value = undefined; return }
  historicContent.value = result.value?.content ?? null
}

function requestRevert(entry: MemoryHistoryEntry): void {
  if (selected.value === null) return
  confirm.require({
    header: 'Revert memory?',
    message: `Restore the version from ${new Date(entry.committedAt).toLocaleString()}? The current version remains in history.`,
    rejectLabel: 'Cancel',
    acceptLabel: 'Revert',
    acceptClass: 'p-button-danger',
    accept: () => { void doRevert(entry) },
  })
}

async function doRevert(entry: MemoryHistoryEntry): Promise<void> {
  const memory = selected.value
  if (memory === null) return
  reverting.value = entry.revision
  const result = await runBackend(revertMemory(memory, entry.revision))
  reverting.value = ''
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  selected.value = result.value
  toast.add({ severity: 'success', summary: 'Memory reverted', life: 3000 })
  await refresh()
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="State"
      title="Memory"
      description="Inspect all persistent sender and chat memories, including their Git history."
    >
      <Button
        label="Refresh"
        icon="pi pi-refresh"
        severity="secondary"
        :loading="loading"
        @click="refresh"
      />
    </PageHeading>
    <Message
      v-if="error"
      severity="error"
      closable
      @close="error = ''"
    >
      {{ error }}
    </Message>
    <article
      v-if="loading && !loaded"
      class="panel manager-loading"
      aria-label="Loading memories"
    >
      <Skeleton
        v-for="index in 6"
        :key="index"
        height="3rem"
      />
    </article>
    <article
      v-else-if="loaded"
      class="panel manager-table"
    >
      <div class="table-toolbar">
        <InputText
          v-model="query"
          placeholder="Filter memory"
          aria-label="Filter memory"
        />
        <span class="muted">{{ filtered.length }} memories</span>
      </div>
      <DataTable
        :value="filtered"
        :paginator="filtered.length > 25"
        :rows="25"
        :rows-per-page-options="[25, 50, 100, 200]"
        selection-mode="single"
        @row-click="inspect($event.data)"
      >
        <Column
          field="platform"
          header="Platform"
        >
          <template #body="{ data }">
            <Tag
              :value="data.platform.toUpperCase()"
              severity="secondary"
            />
          </template>
        </Column>
        <Column
          field="scope"
          header="Scope"
        >
          <template #body="{ data }">
            {{ data.scope === 'sender' ? 'Sender' : 'Chat' }}
          </template>
        </Column>
        <Column
          field="scopeId"
          header="Identity"
        >
          <template #body="{ data }">
            <DisplayIdentity
              :id="data.scopeId"
              :name="data.displayName"
            />
          </template>
        </Column>
        <Column
          field="characters"
          header="Size"
        >
          <template #body="{ data }">
            {{ data.characters.toLocaleString() }} characters
          </template>
        </Column>
      </DataTable>
    </article>
    <Drawer
      v-model:visible="drawerVisible"
      position="right"
      class="inspector-drawer"
      header="Memory"
      :close-on-escape="drawerIsTop"
    >
      <template v-if="selected">
        <Skeleton
          v-if="detailLoading"
          height="12rem"
        />
        <template v-else>
          <div class="inspector-meta">
            <Tag
              :value="selected.platform.toUpperCase()"
              severity="secondary"
            /><Tag
              :value="selected.scope"
              severity="info"
            /><DisplayIdentity
              :id="selected.scopeId"
              :name="selected.displayName"
            />
          </div>
          <div
            class="markdown-body"
            :innerHTML="renderMarkdown(selected.content || '*This memory is empty.*')"
          />
          <section class="memory-history">
            <h3>History</h3>
            <Message
              v-if="history.length === 0"
              severity="secondary"
              :closable="false"
            >
              No committed history.
            </Message>
            <div
              v-else
              class="history-list"
            >
              <div
                v-for="entry in history"
                :key="entry.revision"
                class="history-entry"
              >
                <span><strong>{{ entry.subject || 'Memory update' }}</strong><small>{{ new Date(entry.committedAt).toLocaleString() }} · <code>{{ entry.revision.slice(0, 8) }}</code></small></span>
                <span class="cluster">
                  <Button
                    label="View"
                    icon="pi pi-eye"
                    severity="secondary"
                    size="small"
                    @click="inspectRevision(entry)"
                  />
                  <Button
                    label="Revert"
                    icon="pi pi-history"
                    severity="danger"
                    size="small"
                    :loading="reverting === entry.revision"
                    :disabled="reverting !== ''"
                    @click="requestRevert(entry)"
                  />
                </span>
              </div>
            </div>
          </section>
        </template>
      </template>
    </Drawer>
    <Dialog
      v-model:visible="historyDialogVisible"
      modal
      :header="historicEntry ? `Memory at ${new Date(historicEntry.committedAt).toLocaleString()}` : 'Memory revision'"
      class="revision-dialog"
      :close-on-escape="revisionIsTop"
    >
      <Skeleton
        v-if="historyLoading"
        height="12rem"
      />
      <Message
        v-else-if="historicContent === null"
        severity="secondary"
        :closable="false"
      >
        This memory did not exist at this revision.
      </Message>
      <div
        v-else-if="historicContent !== undefined"
        class="markdown-body"
        :innerHTML="renderMarkdown(historicContent)"
      />
    </Dialog>
  </section>
</template>
