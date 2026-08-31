import { computed, onMounted, ref, watch, type ComputedRef, type Ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { parseAuditSearch } from '@/backend/audit'

export interface AuditScopeRoute {
  query: Ref<string>
  submittedSearch: ComputedRef<ReturnType<typeof parseAuditSearch>>
  scopeKey: ComputedRef<string>
  requestedAuditId: () => number | undefined
  requestedRunId: () => string | undefined
  requestedThreadId: () => number | undefined
  submitSearch: (value: string) => void
  selectAuditId: (id: number) => void
  openThread: (runId: string) => void
  openMedia: (mediaId: string) => void
}

export function useAuditScopeRoute(reload: () => void, select: (id: number) => void): AuditScopeRoute {
  const route = useRoute()
  const router = useRouter()
  const query = ref('')
  const submittedQuery = ref('')
  const search = computed(() => parseAuditSearch(query.value))
  const submittedSearch = computed(() => parseAuditSearch(submittedQuery.value))
  const scopeKey = computed(() => {
    const scope = submittedSearch.value.scope
    return scope === undefined ? '' : `${scope.kind}:${String(scope.value)}`
  })

  function requestedAuditId(): number | undefined {
    const raw = route.params['auditId']
    if (typeof raw !== 'string') return undefined
    const id = Number(raw)
    return Number.isSafeInteger(id) && id > 0 ? id : undefined
  }

  function requestedRunId(): string | undefined {
    const scope = submittedSearch.value.scope
    if (scope !== undefined) return scope.kind === 'run' ? scope.value : undefined
    const runId = route.query['run']
    return typeof runId === 'string' && runId.trim() !== '' ? runId.trim() : undefined
  }

  function requestedThreadId(): number | undefined {
    const scope = submittedSearch.value.scope
    if (scope !== undefined) return scope.kind === 'thread' ? scope.value : undefined
    const raw = route.query['thread']
    if (typeof raw !== 'string') return undefined
    const id = Number(raw)
    return Number.isSafeInteger(id) && id > 0 ? id : undefined
  }

  function syncFromRoute(): void {
    const runId = route.query['run']
    const threadId = route.query['thread']
    const token = typeof runId === 'string' && runId.trim() !== ''
      ? `run:${runId.trim()}`
      : typeof threadId === 'string' && Number.isSafeInteger(Number(threadId)) && Number(threadId) > 0
        ? `thread:${threadId}`
        : undefined
    if (token === undefined) {
      if (submittedSearch.value.scope !== undefined) {
        query.value = search.value.text
        submittedQuery.value = query.value
      }
    } else if (token !== scopeKey.value) {
      query.value = `${token} ${search.value.text}`.trim()
      submittedQuery.value = query.value
    }
  }

  async function updateRouteAndReload(): Promise<void> {
    const scope = submittedSearch.value.scope
    const routeKey = typeof route.query['run'] === 'string'
      ? `run:${route.query['run']}`
      : typeof route.query['thread'] === 'string' ? `thread:${route.query['thread']}` : ''
    if (scopeKey.value !== routeKey) {
      if (scope === undefined) await router.replace({ name: 'audit' })
      else await router.replace({ name: 'audit', query: { [scope.kind]: String(scope.value) } })
    }
    reload()
  }

  function submitSearch(value: string): void {
    submittedQuery.value = value.trim()
    void updateRouteAndReload()
  }

  function selectAuditId(id: number): void {
    if (!Number.isSafeInteger(id) || id < 1) return
    select(id)
    void router.replace({ name: 'audit', params: { auditId: String(id) } })
  }

  watch(() => route.params['auditId'], () => {
    const id = requestedAuditId()
    if (id !== undefined) select(id)
  })
  watch([() => route.query['run'], () => route.query['thread']], () => {
    const previous = scopeKey.value
    syncFromRoute()
    if (scopeKey.value !== previous) reload()
  })
  onMounted(syncFromRoute)

  return {
    query, submittedSearch, scopeKey, requestedAuditId, requestedRunId, requestedThreadId,
    submitSearch, selectAuditId,
    openThread: (runId) => { void router.push({ name: 'threads', query: { run: runId } }) },
    openMedia: (mediaId) => { void router.push({ name: 'media', params: { mediaId } }) },
  }
}
