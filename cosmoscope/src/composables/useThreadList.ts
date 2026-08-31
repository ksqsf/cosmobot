import { computed, ref, type ComputedRef, type Ref } from 'vue'
import { refDebounced } from '@vueuse/core'
import type { DataTablePageEvent } from 'primevue/datatable'
import { listThreads } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useLatest } from '@/async'
import { useConnectionStore } from '@/stores/connection'
import type { AuditPlatform, ThreadSummary } from '@/types/domain'

interface ThreadList {
  readonly threads: Ref<ThreadSummary[]>
  readonly query: Ref<string>
  readonly debouncedQuery: Readonly<Ref<string>>
  readonly platform: Ref<AuditPlatform | 'all'>
  readonly first: Ref<number>
  readonly rows: Ref<number>
  readonly total: Ref<number>
  readonly error: Ref<string>
  readonly loading: Ref<boolean>
  readonly tableLoading: Ref<boolean>
  readonly loaded: Ref<boolean>
  readonly summary: ComputedRef<{ threads: number, nodes: number, leaves: number, platforms: number }>
  readonly refresh: () => Promise<void>
  readonly changePage: (event: DataTablePageEvent) => void
}

export function useThreadList(afterRefresh: () => void | Promise<void>): ThreadList {
  const connection = useConnectionStore()
  const latest = useLatest()
  const threads = ref<ThreadSummary[]>([])
  const query = ref('')
  const debouncedQuery = refDebounced(query, 250)
  const platform = ref<AuditPlatform | 'all'>('all')
  const first = ref(0)
  const rows = ref(25)
  const total = ref(0)
  const nodeTotal = ref(0)
  const leafTotal = ref(0)
  const platformTotal = ref(0)
  const error = ref('')
  const loading = ref(true)
  const tableLoading = ref(false)
  const loaded = ref(false)
  const summary = computed(() => ({
    threads: total.value,
    nodes: nodeTotal.value,
    leaves: leafTotal.value,
    platforms: platformTotal.value,
  }))

  async function refresh(): Promise<void> {
    const token = latest.begin()
    if (!latest.current(token)) return
    if (connection.state === 'opening' || connection.state === 'reconnecting') {
      if (loaded.value) tableLoading.value = true
      else loading.value = true
      return
    }
    if (connection.state !== 'authenticated') {
      loading.value = false
      tableLoading.value = false
      error.value = connection.error || 'Connect to cosmobot to load threads.'
      return
    }
    if (loaded.value) tableLoading.value = true
    else loading.value = true
    const result = await runBackend(listThreads({
      offset: first.value,
      limit: rows.value,
      ...(debouncedQuery.value.trim() === '' ? {} : { query: debouncedQuery.value.trim() }),
      ...(platform.value === 'all' ? {} : { platform: platform.value }),
    }))
    if (!latest.current(token)) return
    loading.value = false
    tableLoading.value = false
    if (result._tag === 'Failure') {
      error.value = result.error.message
      return
    }
    threads.value = [...result.value.threads]
    total.value = result.value.total
    nodeTotal.value = result.value.nodes
    leafTotal.value = result.value.leaves
    platformTotal.value = result.value.platforms
    loaded.value = true
    error.value = ''
    await afterRefresh()
  }

  function changePage(event: DataTablePageEvent): void {
    first.value = event.first
    rows.value = event.rows
    void refresh()
  }

  return {
    threads, query, debouncedQuery, platform, first, rows, total, error, loading, tableLoading, loaded, summary,
    refresh, changePage,
  }
}
