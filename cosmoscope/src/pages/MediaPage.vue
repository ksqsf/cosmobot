<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { refDebounced } from '@vueuse/core'
import { useRoute, useRouter } from 'vue-router'
import { useConfirm } from 'primevue/useconfirm'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Dialog from 'primevue/dialog'
import Drawer from 'primevue/drawer'
import FloatLabel from 'primevue/floatlabel'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import MultiSelect from 'primevue/multiselect'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import { collectMediaGarbage, deleteMedia, getMedia, listMedia, searchMedia } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { formatBytes } from '@/format'
import { useConnectionStore } from '@/stores/connection'
import type { MediaDetail, MediaGcSettings, MediaItem, MediaSourceKind, MediaStats } from '@/types/domain'

type PendingAction = 'detail' | 'delete' | 'batch-delete' | 'gc'
const emptyStats: MediaStats = { files: 0, existingFiles: 0, missingFiles: 0, totalBytes: 0, sources: 0, platformRefs: 0, platformAssociations: 0, mimeTypes: [], platforms: [] }
const emptyGcSettings: MediaGcSettings = { enabled: false, maxAgeSeconds: 0, intervalHours: 0 }
const platformIcons: Readonly<Record<string, string>> = {
  telegram: 'pi pi-send', matrix: 'pi pi-th-large', qq: 'pi pi-comments', discord: 'pi pi-comments', rpc: 'pi pi-desktop', acp: 'pi pi-code',
}
const platformLabels: Readonly<Record<string, string>> = {
  matrix: 'Matrix', qq: 'QQ', telegram: 'Telegram', discord: 'Discord', rpc: 'RPC', acp: 'ACP',
}
const sourceKindLabels: Readonly<Record<MediaSourceKind, string>> = { chat: 'Chat', 'generated-image': 'Generated image', 'tool-result': 'Tool result', sandbox: 'Sandbox file' }
const sourceKindIcons: Readonly<Record<MediaSourceKind, string>> = { chat: 'pi pi-comments', 'generated-image': 'pi pi-image', 'tool-result': 'pi pi-wrench', sandbox: 'pi pi-box' }
const noPlatform = '__none__'
const media = ref<readonly MediaItem[]>([])
const stats = ref<MediaStats>(emptyStats)
const gcSettings = ref<MediaGcSettings>(emptyGcSettings)
const selection = ref<MediaItem[]>([])
const detail = ref<MediaDetail>()
const drawerOpen = ref(false)
const zoomOpen = ref(false)
const query = ref('')
const debouncedQuery = refDebounced(query, 250)
const platforms = ref<string[]>([])
const mimeTypes = ref<string[]>([])
const sourceKinds = ref<MediaSourceKind[]>([])
const error = ref('')
const loading = ref(true)
const loaded = ref(false)
const pending = ref<PendingAction>()
const searchResults = ref<readonly MediaItem[]>()
const searching = ref(false)
const searchError = ref('')
let searchGeneration = 0
const route = useRoute()
const router = useRouter()
const confirm = useConfirm()
const toast = useToast()
const connection = useConnectionStore()

const platformOptions = computed(() => [
  ...stats.value.platforms.map((value) => ({ label: platformLabel(value), value })),
  { label: 'No platform', value: noPlatform },
])
const mimeTypeOptions = computed(() => [...stats.value.mimeTypes])
const sourceKindOptions = (['chat', 'generated-image', 'tool-result', 'sandbox'] as const).map((value) => ({ label: sourceKindLabels[value], value }))
const filtered = computed(() => searchResults.value ?? media.value)

function formatTime(seconds: number): string { return new Date(seconds * 1000).toLocaleString() }
function effectiveSourceKinds(item: Pick<MediaItem, 'sourceKinds'>): readonly MediaSourceKind[] {
  return item.sourceKinds.length === 0 ? ['chat'] : item.sourceKinds
}
function formatAge(seconds: number): string {
  const days = seconds / 86_400
  return `${Number.isInteger(days) ? String(days) : days.toFixed(1)} days`
}
function mediaIcon(mimeType: string): string {
  if (mimeType.startsWith('image/')) return 'pi pi-image'
  if (mimeType.startsWith('audio/')) return 'pi pi-volume-up'
  if (mimeType.startsWith('video/')) return 'pi pi-video'
  return 'pi pi-file'
}
function sourcePlatform(source: string): string {
  const normalized = source.trim().toLowerCase()
  if (normalized.includes('qq.com') || normalized.startsWith('qq:') || normalized.startsWith('qqfile:')) return 'qq'
  if (normalized.startsWith('mxc://') || normalized.startsWith('matrix:') || normalized.includes('matrix.to')) return 'matrix'
  if (normalized.startsWith('telegram:') || normalized.includes('telegram.org') || normalized.includes('t.me/')) return 'telegram'
  if (normalized.startsWith('discord:') || ['discord.com', 'discordapp.com', 'discordapp.net'].some((host) => normalized.includes(host))) return 'discord'
  return 'web'
}
function platformIcon(platform: string): string { return platformIcons[platform.toLowerCase()] ?? 'pi pi-link' }
function platformLabel(platform: string): string { return platformLabels[platform.toLowerCase()] ?? platform }

async function refresh(): Promise<void> {
  if (connection.state === 'opening' || connection.state === 'reconnecting') { loading.value = true; return }
  if (connection.state !== 'authenticated') {
    loading.value = false
    error.value = connection.error || 'Connect to cosmobot to load media.'
    return
  }
  loading.value = true
  const result = await runBackend(listMedia())
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  media.value = result.value.files
  stats.value = result.value.stats
  gcSettings.value = result.value.gcSettings
  loaded.value = true
  selection.value = selection.value.filter(({ mediaId }) => media.value.some((item) => item.mediaId === mediaId))
  await selectFromRoute()
  void refreshSearch()
}
async function refreshSearch(): Promise<void> {
  const generation = ++searchGeneration
  const selectedPlatforms = platforms.value.filter((platform) => platform !== noPlatform)
  const text = debouncedQuery.value.trim()
  const hasConditions = text !== '' || platforms.value.length > 0 || mimeTypes.value.length > 0 || sourceKinds.value.length > 0
  if (!hasConditions) {
    searchResults.value = undefined
    searchError.value = ''
    searching.value = false
    return
  }
  searching.value = true
  searchError.value = ''
  const result = await runBackend(searchMedia({
    ...(text === '' ? {} : { query: text }),
    platforms: selectedPlatforms,
    withoutPlatform: platforms.value.includes(noPlatform),
    mimeTypes: mimeTypes.value,
    sourceKinds: sourceKinds.value,
    limit: 500,
  }))
  if (generation !== searchGeneration) return
  searching.value = false
  if (result._tag === 'Failure') {
    searchError.value = result.error.message
    return
  }
  searchResults.value = result.value
}
async function selectFromRoute(): Promise<void> {
  const mediaId = route.params['mediaId']
  if (typeof mediaId !== 'string') return
  pending.value = 'detail'
  const result = await runBackend(getMedia(mediaId))
  pending.value = undefined
  if (result._tag === 'Failure') { drawerOpen.value = false; error.value = result.error.message; return }
  detail.value = result.value
  drawerOpen.value = true
}
function inspect(item: MediaItem): void { void router.replace(`/media/${encodeURIComponent(item.mediaId)}`) }
function closeDrawer(): void {
  detail.value = undefined
  zoomOpen.value = false
  if (route.params['mediaId'] !== undefined) void router.replace('/media')
}
async function remove(ids: readonly string[]): Promise<void> {
  if (pending.value !== undefined) return
  pending.value = ids.length === 1 ? 'delete' : 'batch-delete'
  const results = []
  for (const id of ids) results.push(await runBackend(deleteMedia(id)))
  pending.value = undefined
  const failed = results.filter(({ _tag }) => _tag === 'Failure').length
  const deleted = ids.length - failed
  toast.add({ severity: failed === 0 ? 'success' : 'warn', summary: `Deleted ${String(deleted)} media object${deleted === 1 ? '' : 's'}`, detail: failed === 0 ? undefined : `${String(failed)} could not be deleted`, life: 3500 })
  drawerOpen.value = false
  await router.replace('/media')
  await refresh()
}
function requestDelete(items: readonly MediaItem[]): void {
  if (items.length === 0) return
  confirm.require({
    header: `Delete ${String(items.length)} media object${items.length === 1 ? '' : 's'}?`,
    message: 'This removes the selected media records and files when they are no longer shared by another record.',
    rejectLabel: 'Keep media', acceptLabel: 'Delete', acceptClass: 'p-button-danger',
    accept: () => { void remove(items.map(({ mediaId }) => mediaId)) },
  })
}
async function runGc(force: boolean): Promise<void> {
  if (pending.value !== undefined) return
  const before = stats.value.totalBytes
  pending.value = 'gc'
  const result = await runBackend(collectMediaGarbage(force ? 0 : undefined))
  pending.value = undefined
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  await refresh()
  const freed = Math.max(0, before - stats.value.totalBytes)
  toast.add({
    severity: 'success',
    summary: `GC deleted ${String(result.value.deleted)} media object${result.value.deleted === 1 ? '' : 's'}`,
    detail: `${formatBytes(freed)} freed · ${String(result.value.retainedReferencedFiles)} referenced objects retained`,
    life: 4500,
  })
}
function requestGc(force = false): void {
  confirm.require({
    header: force ? 'Force garbage collection?' : 'Garbage collect media?',
    message: force || gcSettings.value.maxAgeSeconds === 0
      ? 'Delete all unreferenced media regardless of age.'
      : `Delete unreferenced media older than ${formatAge(gcSettings.value.maxAgeSeconds)}.`,
    rejectLabel: 'Cancel', acceptLabel: 'Run garbage collection',
    acceptClass: force ? 'p-button-danger' : undefined,
    accept: () => { void runGc(force) },
  })
}
async function copyPublicUrl(): Promise<void> {
  if (!detail.value) return
  try {
    await navigator.clipboard.writeText(detail.value.publicUrl)
    toast.add({ severity: 'success', summary: 'Public URL copied', life: 2000 })
  } catch { toast.add({ severity: 'error', summary: 'Could not copy the public URL', life: 3000 }) }
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
watch(() => route.params['mediaId'], () => { void selectFromRoute() })
watch([debouncedQuery, platforms, mimeTypes, sourceKinds], () => { void refreshSearch() }, { deep: true })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Data Store"
      title="Media"
      description="Inspect media by provenance, preview content, and reclaim unreferenced storage."
    >
      <Button
        v-if="gcSettings.maxAgeSeconds > 0"
        label="Garbage collect"
        icon="pi pi-trash"
        severity="success"
        :loading="pending === 'gc'"
        @click="requestGc()"
      />
      <Button
        label="Force GC"
        icon="pi pi-trash"
        severity="danger"
        :loading="pending === 'gc'"
        @click="requestGc(true)"
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
      :closable="false"
    >
      {{ error }}
    </Message>
    <article
      v-if="loading && !loaded"
      class="panel manager-loading"
      aria-label="Loading media"
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
        aria-label="Media summary"
      >
        <div><span class="summary-mark violet"><i class="pi pi-images" /></span><span><strong>{{ stats.files }}</strong><small>Objects</small></span></div>
        <div><span class="summary-mark info"><i class="pi pi-database" /></span><span><strong>{{ formatBytes(stats.totalBytes) }}</strong><small>Total size</small></span></div>
        <div><span class="summary-mark success"><i class="pi pi-link" /></span><span><strong>{{ stats.platformAssociations }}</strong><small>Platform associations</small></span></div>
        <div>
          <span
            class="summary-mark"
            :class="stats.missingFiles === 0 ? 'neutral' : 'warning'"
          ><i class="pi pi-exclamation-circle" /></span><span><strong>{{ stats.missingFiles }}</strong><small>Missing files</small></span>
        </div>
      </div>
      <article class="panel manager-table">
        <div class="table-toolbar resource-toolbar">
          <InputText
            v-model="query"
            placeholder="Search all media"
            aria-label="Search all media"
          />
          <div>
            <FloatLabel variant="on">
              <MultiSelect
                v-model="sourceKinds"
                :options="sourceKindOptions"
                input-id="media-source-filter"
                option-label="label"
                option-value="value"
                display="chip"
                placeholder="All"
              />
              <label for="media-source-filter">Source</label>
            </FloatLabel>
            <FloatLabel variant="on">
              <MultiSelect
                v-model="platforms"
                :options="platformOptions"
                input-id="media-platform-filter"
                option-label="label"
                option-value="value"
                display="chip"
                placeholder="All"
              />
              <label for="media-platform-filter">Platform</label>
            </FloatLabel>
            <FloatLabel variant="on">
              <MultiSelect
                v-model="mimeTypes"
                :options="mimeTypeOptions"
                input-id="media-type-filter"
                display="chip"
                placeholder="All"
              />
              <label for="media-type-filter">Type</label>
            </FloatLabel>
            <Button
              label="Delete selected"
              icon="pi pi-trash"
              severity="danger"
              outlined
              :disabled="selection.length === 0 || pending !== undefined"
              @click="requestDelete(selection)"
            />
          </div>
        </div>
        <DataTable
          v-model:selection="selection"
          :value="filtered"
          :loading="searching"
          data-key="mediaId"
          :paginator="filtered.length > 25"
          :rows="25"
          :rows-per-page-options="[25, 50, 100, 200]"
          @row-click="inspect($event.data)"
        >
          <Column
            selection-mode="multiple"
            header-style="width: 3rem"
          />
          <Column
            field="sourceName"
            header="Media"
          >
            <template #body="{ data }">
              <span class="manager-identity"><span class="manager-type-icon"><i :class="mediaIcon(data.mimeType)" /></span><span><strong>{{ data.sourceName || data.mediaId }}</strong><small>{{ data.mediaId }}</small></span></span>
            </template>
          </Column>
          <Column header="Platform">
            <template #body="{ data }">
              <span class="tag-list"><Tag
                v-for="name in data.platforms"
                :key="name"
                :value="platformLabel(name)"
                :icon="platformIcon(name)"
                severity="secondary"
              /><small v-if="data.platforms.length === 0">No platform</small></span>
            </template>
          </Column>
          <Column header="Source">
            <template #body="{ data }">
              <span class="tag-list"><Tag
                v-for="kind in effectiveSourceKinds(data)"
                :key="kind"
                :value="sourceKindLabels[kind as MediaSourceKind]"
                :icon="sourceKindIcons[kind as MediaSourceKind]"
                severity="secondary"
              /></span>
            </template>
          </Column>
          <Column
            field="mimeType"
            header="Type"
          />
          <Column header="Size">
            <template #body="{ data }">
              {{ formatBytes(data.size) }}
            </template>
          </Column>
          <Column header="Last used">
            <template #body="{ data }">
              {{ formatTime(data.lastUsedAtUnix) }}
            </template>
          </Column>
          <Column header="State">
            <template #body="{ data }">
              <Tag
                :value="data.exists ? 'Available' : 'Missing'"
                :severity="data.exists ? 'success' : 'danger'"
              />
            </template>
          </Column>
        </DataTable>
        <Message
          v-if="searchError"
          severity="error"
          :closable="false"
        >
          {{ searchError }}
        </Message>
      </article>
    </template>
    <Drawer
      v-model:visible="drawerOpen"
      header="Media detail"
      aria-label="Media detail"
      position="right"
      :style="{ width: 'min(520px, 100vw)' }"
      @hide="closeDrawer"
    >
      <div
        v-if="pending === 'detail'"
        class="manager-loading"
      >
        <Skeleton
          v-for="index in 5"
          :key="index"
          height="3rem"
        />
      </div>
      <div
        v-else-if="detail"
        class="stack stack-loose"
      >
        <button
          v-if="detail.exists && detail.mimeType.startsWith('image/')"
          class="chat-image-button"
          type="button"
          aria-label="Zoom image"
          @click="zoomOpen = true"
        >
          <img
            :src="detail.publicUrl"
            :alt="detail.sourceName || detail.mediaId"
          />
        </button>
        <video
          v-else-if="detail.exists && detail.mimeType.startsWith('video/')"
          class="object-preview"
          controls
          preload="metadata"
        >
          <source
            :src="detail.publicUrl"
            :type="detail.mimeType"
          />
          Your browser cannot play this video.
        </video>
        <audio
          v-else-if="detail.exists && detail.mimeType.startsWith('audio/')"
          controls
          preload="metadata"
        >
          <source
            :src="detail.publicUrl"
            :type="detail.mimeType"
          />
          Your browser cannot play this audio.
        </audio>
        <header class="drawer-hero">
          <span class="platform-icon"><i :class="mediaIcon(detail.mimeType)" /></span><div>
            <small>{{ detail.mimeType }}</small><h2>{{ detail.sourceName || detail.mediaId }}</h2><Tag
              :value="detail.exists ? 'Available' : 'Missing'"
              :severity="detail.exists ? 'success' : 'danger'"
            />
          </div>
        </header>
        <dl class="detail-list">
          <div><dt>Media ID</dt><dd>{{ detail.mediaId }}</dd></div><div><dt>File ID</dt><dd>{{ detail.fileId }}</dd></div><div><dt>Size</dt><dd>{{ formatBytes(detail.size) }}</dd></div><div><dt>Created</dt><dd>{{ formatTime(detail.createdAtUnix) }}</dd></div><div><dt>Last used</dt><dd>{{ formatTime(detail.lastUsedAtUnix) }}</dd></div><div><dt>Digest</dt><dd>{{ detail.digest }}</dd></div>
        </dl>
        <div class="stack">
          <strong>Public URL</strong><div class="resource-rename">
            <InputText
              :model-value="detail.publicUrl"
              readonly
              aria-label="Public URL"
            /><Button
              icon="pi pi-copy"
              aria-label="Copy public URL"
              severity="secondary"
              @click="copyPublicUrl"
            /><Button
              icon="pi pi-external-link"
              aria-label="Open public URL"
              as="a"
              :href="detail.publicUrl"
              target="_blank"
              rel="noopener"
            />
          </div>
        </div>
        <div
          class="stack"
        >
          <strong>Source tags</strong><span class="tag-list"><Tag
            v-for="kind in effectiveSourceKinds(detail)"
            :key="kind"
            :value="sourceKindLabels[kind]"
            :icon="sourceKindIcons[kind]"
            severity="secondary"
          /></span>
        </div>
        <div
          v-if="detail.platforms.length"
          class="stack"
        >
          <strong>Platforms</strong><span class="tag-list"><Tag
            v-for="name in detail.platforms"
            :key="name"
            :value="platformLabel(name)"
            :icon="platformIcon(name)"
            severity="secondary"
          /></span>
        </div>
        <div
          v-if="detail.platformRefs.length"
          class="stack"
        >
          <strong>Platform upload cache</strong><ul class="associated-resource-list">
            <li
              v-for="platformRef in detail.platformRefs"
              :key="`${platformRef.platform}:${platformRef.scope}:${platformRef.platformRef}`"
            >
              <span><strong>{{ platformLabel(platformRef.platform) }}</strong><small class="block">{{ platformRef.scope }}</small></span><code>{{ platformRef.platformRef }}</code>
            </li>
          </ul>
        </div>
        <div
          v-if="detail.sourceRefs.length"
          class="stack"
        >
          <strong>Source references</strong><span class="tag-list"><Tag
            v-for="source in detail.sourceRefs"
            :key="source"
            :value="source"
            :icon="platformIcon(sourcePlatform(source))"
            severity="secondary"
          /></span>
        </div>
        <Button
          label="Delete media"
          icon="pi pi-trash"
          severity="danger"
          :loading="pending === 'delete'"
          @click="requestDelete([detail])"
        />
      </div>
    </Drawer>
    <Dialog
      v-model:visible="zoomOpen"
      modal
      dismissable-mask
      header="Image preview"
    >
      <img
        v-if="detail"
        class="object-preview"
        :src="detail.publicUrl"
        :alt="detail.sourceName || detail.mediaId"
      />
    </Dialog>
  </section>
</template>
