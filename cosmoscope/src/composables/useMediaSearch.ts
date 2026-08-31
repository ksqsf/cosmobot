import { computed, ref, watch, type ComputedRef, type Ref } from 'vue'
import { refDebounced } from '@vueuse/core'
import { searchMedia } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useLatest } from '@/async'
import { platformLabel, sourceKindLabels } from '@/domain/media'
import type { MediaItem, MediaSourceKind, MediaStats } from '@/types/domain'

const noPlatform = '__none__'

export interface MediaSearch {
  query: Ref<string>
  platforms: Ref<string[]>
  mimeTypes: Ref<string[]>
  sourceKinds: Ref<MediaSourceKind[]>
  filtered: ComputedRef<readonly MediaItem[]>
  searching: Ref<boolean>
  error: Ref<string>
  platformOptions: ComputedRef<{ label: string; value: string }[]>
  mimeTypeOptions: ComputedRef<string[]>
  sourceKindOptions: { label: string; value: MediaSourceKind }[]
  refresh: () => Promise<void>
}

export function useMediaSearch(media: Ref<readonly MediaItem[]>, stats: Ref<MediaStats>): MediaSearch {
  const query = ref('')
  const debouncedQuery = refDebounced(query, 250)
  const platforms = ref<string[]>([])
  const mimeTypes = ref<string[]>([])
  const sourceKinds = ref<MediaSourceKind[]>([])
  const results = ref<readonly MediaItem[]>()
  const searching = ref(false)
  const error = ref('')
  const latest = useLatest()
  const filtered = computed(() => results.value ?? media.value)
  const platformOptions = computed(() => [
    ...stats.value.platforms.map((value) => ({ label: platformLabel(value), value })),
    { label: 'No platform', value: noPlatform },
  ])
  const mimeTypeOptions = computed(() => [...stats.value.mimeTypes])
  const sourceKindOptions = (['chat', 'generated-image', 'tool-result', 'sandbox'] as const).map((value) => ({ label: sourceKindLabels[value], value }))

  async function refresh(): Promise<void> {
    const token = latest.begin()
    const text = debouncedQuery.value.trim()
    if (text === '' && platforms.value.length === 0 && mimeTypes.value.length === 0 && sourceKinds.value.length === 0) {
      results.value = undefined; error.value = ''; searching.value = false; return
    }
    searching.value = true
    error.value = ''
    const result = await runBackend(searchMedia({
      ...(text === '' ? {} : { query: text }),
      platforms: platforms.value.filter((platform) => platform !== noPlatform),
      withoutPlatform: platforms.value.includes(noPlatform),
      mimeTypes: mimeTypes.value,
      sourceKinds: sourceKinds.value,
      limit: 500,
    }))
    if (!latest.current(token)) return
    searching.value = false
    if (result._tag === 'Failure') { error.value = result.error.message; return }
    results.value = result.value
  }

  watch([debouncedQuery, platforms, mimeTypes, sourceKinds], () => { void refresh() }, { deep: true })
  return { query, platforms, mimeTypes, sourceKinds, filtered, searching, error, platformOptions, mimeTypeOptions, sourceKindOptions, refresh }
}
