<script setup lang="ts">
import { onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import PageHeading from '@/components/PageHeading.vue'
import MediaFilterBar from '@/components/media/MediaFilterBar.vue'
import MediaPreviewDrawer from '@/components/media/MediaPreviewDrawer.vue'
import MediaTable from '@/components/media/MediaTable.vue'
import { getMedia, listMedia } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useLatest } from '@/async'
import { useMediaOperations } from '@/composables/useMediaOperations'
import { useMediaSearch } from '@/composables/useMediaSearch'
import { formatBytes } from '@/format'
import { useConnectionStore } from '@/stores/connection'
import type { MediaDetail, MediaGcSettings, MediaItem, MediaStats } from '@/types/domain'

const emptyStats: MediaStats = { files: 0, existingFiles: 0, missingFiles: 0, totalBytes: 0, sources: 0, platformRefs: 0, platformAssociations: 0, mimeTypes: [], platforms: [] }
const emptyGcSettings: MediaGcSettings = { enabled: false, maxAgeSeconds: 0, intervalHours: 0 }
const media = ref<readonly MediaItem[]>([])
const stats = ref<MediaStats>(emptyStats)
const gcSettings = ref<MediaGcSettings>(emptyGcSettings)
const selection = ref<MediaItem[]>([])
const detail = ref<MediaDetail>()
const drawerOpen = ref(false)
const error = ref('')
const loading = ref(true)
const loaded = ref(false)
const listLatest = useLatest()
const detailLatest = useLatest()
const route = useRoute()
const router = useRouter()
const toast = useToast()
const connection = useConnectionStore()
const search = useMediaSearch(media, stats)

async function refresh(): Promise<void> {
  const token = listLatest.begin()
  if (connection.state === 'opening' || connection.state === 'reconnecting') { loading.value = true; return }
  if (connection.state !== 'authenticated') { loading.value = false; error.value = connection.error || 'Connect to cosmobot to load media.'; return }
  loading.value = true
  const result = await runBackend(listMedia())
  if (!listLatest.current(token)) return
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  media.value = result.value.files
  stats.value = result.value.stats
  gcSettings.value = result.value.gcSettings
  loaded.value = true
  selection.value = selection.value.filter(({ mediaId }) => media.value.some((item) => item.mediaId === mediaId))
  await selectFromRoute()
  void search.refresh()
}

const operations = useMediaOperations(stats, gcSettings, refresh)

async function selectFromRoute(): Promise<void> {
  const token = detailLatest.begin()
  const mediaId = route.params['mediaId']
  if (typeof mediaId !== 'string') {
    detail.value = undefined; drawerOpen.value = false
    if (operations.pending.value === 'detail') operations.pending.value = undefined
    return
  }
  operations.pending.value = 'detail'
  const result = await runBackend(getMedia(mediaId))
  if (!detailLatest.current(token)) return
  operations.pending.value = undefined
  if (result._tag === 'Failure') { drawerOpen.value = false; error.value = result.error.message; return }
  detail.value = result.value
  drawerOpen.value = true
}

function inspect(item: MediaItem): void { void router.replace(`/media/${encodeURIComponent(item.mediaId)}`) }
function closeDrawer(): void {
  detailLatest.invalidate()
  detail.value = undefined
  if (operations.pending.value === 'detail') operations.pending.value = undefined
  if (route.params['mediaId'] !== undefined) void router.replace('/media')
}
async function copyPublicUrl(): Promise<void> {
  if (!detail.value) return
  try {
    await navigator.clipboard.writeText(detail.value.publicUrl)
    toast.add({ severity: 'success', summary: 'Public URL copied', life: 2000 })
  } catch { toast.add({ severity: 'error', summary: 'Could not copy the public URL', life: 3000 }) }
}

onMounted(() => { void refresh() })
watch([() => connection.state, () => connection.methods], () => { void refresh() })
watch(() => route.params['mediaId'], () => { void selectFromRoute() })
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
        :loading="operations.pending.value === 'gc'"
        @click="operations.requestGc()"
      />
      <Button
        label="Force GC"
        icon="pi pi-trash"
        severity="danger"
        :loading="operations.pending.value === 'gc'"
        @click="operations.requestGc(true)"
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
        <MediaFilterBar
          v-model:query="search.query.value"
          v-model:source-kinds="search.sourceKinds.value"
          v-model:platforms="search.platforms.value"
          v-model:mime-types="search.mimeTypes.value"
          :platform-options="search.platformOptions.value"
          :mime-type-options="search.mimeTypeOptions.value"
          :source-kind-options="search.sourceKindOptions"
          :selection="selection"
          :disabled="operations.pending.value !== undefined"
          @delete="operations.requestDelete(selection)"
        />
        <MediaTable
          v-model:selection="selection"
          :items="search.filtered.value"
          :loading="search.searching.value"
          @inspect="inspect"
        />
        <Message
          v-if="search.error.value"
          severity="error"
          :closable="false"
        >
          {{ search.error.value }}
        </Message>
      </article>
    </template>
    <MediaPreviewDrawer
      v-model:visible="drawerOpen"
      :detail="detail"
      :loading="operations.pending.value === 'detail'"
      :deleting="operations.pending.value === 'delete'"
      @hide="closeDrawer"
      @copy="copyPublicUrl"
      @delete="detail && operations.requestDelete([detail])"
    />
  </section>
</template>
