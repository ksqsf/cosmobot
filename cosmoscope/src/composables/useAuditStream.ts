import { computed, onMounted, ref, watch, type ComputedRef, type Ref } from 'vue'
import {
  getAudit, getRunAudit, getThreadAudit, recentAudit, resolveThreadRun, RpcBackendError, searchAudit, subscribeAudit,
  type BackendError,
} from '@/backend/AdminBackend'
import {
  auditPlatform, auditPresentation, isAuditFailure, mergeAuditRecords,
} from '@/backend/audit'
import { runBackend, type BackendResult } from '@/backend/runBackend'
import { useLatest, useLatestSubscription } from '@/async'
import { useConnectionStore } from '@/stores/connection'
import type { AuditEvent, AuditPlatform, AuditRecord } from '@/types/domain'
import type { AuditScopeRoute } from './useAuditScopeRoute'

export type AuditEventFilter = 'tool' | 'model' | 'failure'
export type AuditPlatformFilter = AuditPlatform | 'unlinked'
export type AuditPageState = 'loading' | 'ready' | 'unavailable' | 'error'

export const auditPlatformOptions = [
  { label: 'QQ', value: 'PlatformQQ' },
  { label: 'Telegram', value: 'PlatformTelegram' },
  { label: 'Matrix', value: 'PlatformMatrix' },
  { label: 'Discord', value: 'PlatformDiscord' },
  { label: 'RPC', value: 'PlatformRPC' },
  { label: 'ACP', value: 'PlatformACP' },
  { label: 'Unlinked', value: 'unlinked' },
] satisfies readonly { readonly label: string; readonly value: AuditPlatformFilter }[]

export const auditEventTypeOptions = [
  { label: 'Tool calls', value: 'tool' },
  { label: 'Model turns', value: 'model' },
  { label: 'Failures', value: 'failure' },
] satisfies readonly { readonly label: string; readonly value: AuditEventFilter }[]

const loadedLimit = 200
const bufferedLimit = 100
const eventFilters = {
  tool: (event: AuditEvent) => auditPresentation(event).category === 'tool',
  model: (event: AuditEvent) => auditPresentation(event).category === 'model',
  failure: isAuditFailure,
} satisfies Record<AuditEventFilter, (event: AuditEvent) => boolean>

export interface AuditStream {
  state: Ref<AuditPageState>
  error: Ref<string>
  events: Ref<AuditRecord[]>
  buffered: Ref<AuditRecord[]>
  paused: Ref<boolean>
  selectedId: Ref<number | undefined>
  selected: ComputedRef<AuditRecord | undefined>
  related: Ref<AuditRecord[]>
  detailError: Ref<string>
  threadError: Ref<string>
  platforms: Ref<AuditPlatformFilter[]>
  eventTypes: Ref<AuditEventFilter[]>
  filteredEvents: ComputedRef<AuditRecord[]>
  loadSnapshot: () => Promise<void>
  loadSelection: (id: number) => Promise<void>
  installSubscription: () => Promise<void>
  platformLabel: (record: AuditRecord) => string
  resume: () => void
  discard: () => void
}

export function useAuditStream(scope: AuditScopeRoute): AuditStream {
  const connection = useConnectionStore()
  const state = ref<AuditPageState>('loading')
  const error = ref('')
  const events = ref<AuditRecord[]>([])
  const buffered = ref<AuditRecord[]>([])
  const paused = ref(false)
  const selectedId = ref<number>()
  const selectedDetail = ref<AuditRecord>()
  const related = ref<AuditRecord[]>([])
  const detailError = ref('')
  const threadError = ref('')
  const platforms = ref<AuditPlatformFilter[]>([])
  const eventTypes = ref<AuditEventFilter[]>([])
  const snapshotLatest = useLatest()
  const detailLatest = useLatest()
  const subscription = useLatestSubscription()
  const allKnownEvents = computed(() => mergeAuditRecords(events.value, buffered.value, loadedLimit + bufferedLimit))
  const selected = computed(() => selectedDetail.value ?? events.value.find(({ id }) => id === selectedId.value))
  const filteredEvents = computed(() => [...events.value].reverse().filter((record) => {
    const presentation = auditPresentation(record.event)
    const searchable = `${presentation.kind} ${presentation.summary} ${record.event.runId}`.toLowerCase()
    const recordPlatform = auditPlatform(allKnownEvents.value, record.event.runId)
    const platformMatches = platforms.value.length === 0 || platforms.value.some((platform) =>
      platform === 'unlinked' ? recordPlatform === undefined : platform === recordPlatform)
    return searchable.includes(scope.submittedSearch.value.text.trim().toLowerCase())
      && platformMatches
      && (eventTypes.value.length === 0 || eventTypes.value.some((eventType) => eventFilters[eventType](record.event)))
  }))
  const requiredMethods = ['audit.recent', 'audit.search', 'audit.get', 'audit.thread', 'audit.subscribe'] as const
  const supportsAudit = computed(() => requiredMethods.every((method) => connection.methods.has(method)))

  async function loadThreadAudit(threadId: number): Promise<BackendResult<readonly AuditRecord[], BackendError>> {
    const result = await runBackend(getThreadAudit(threadId))
    if (result._tag === 'Failure') return result
    return result.value === null
      ? { _tag: 'Failure', error: new RpcBackendError({ message: `Thread #${String(threadId)} was not found.` }) }
      : { _tag: 'Success', value: result.value }
  }

  async function loadRequestedAudit(): Promise<BackendResult<readonly AuditRecord[], BackendError>> {
    const runId = scope.requestedRunId()
    if (runId !== undefined) return runBackend(getRunAudit(runId))
    const threadId = scope.requestedThreadId()
    if (threadId !== undefined) return loadThreadAudit(threadId)
    const searchText = scope.submittedSearch.value.text.trim()
    return runBackend(searchText === '' ? recentAudit(loadedLimit) : searchAudit(searchText))
  }

  async function loadSnapshot(): Promise<void> {
    const token = snapshotLatest.begin()
    const result = await loadRequestedAudit()
    if (!snapshotLatest.current(token)) return
    if (result._tag === 'Failure') {
      error.value = result.error.message
      if (state.value !== 'ready') state.value = 'error'
      return
    }
    error.value = ''
    if (paused.value) {
      const unseen = result.value.filter(({ id }) => !events.value.some((record) => record.id === id))
      buffered.value = mergeAuditRecords(buffered.value, unseen, bufferedLimit)
    } else {
      const completeResult = scope.requestedRunId() !== undefined
        || scope.requestedThreadId() !== undefined
        || scope.submittedSearch.value.text.trim() !== ''
      events.value = completeResult ? [...result.value] : mergeAuditRecords([], result.value, loadedLimit)
    }
    state.value = 'ready'
    const requested = scope.requestedAuditId()
    if (requested !== undefined) await loadSelection(requested)
    else if (selectedId.value === undefined && events.value.length > 0) scope.selectAuditId(events.value.at(-1)?.id ?? 0)
  }

  function receive(record: AuditRecord): void {
    const runId = scope.requestedRunId()
    if (runId !== undefined && record.event.runId !== runId) return
    if (scope.requestedThreadId() !== undefined && !events.value.some(({ event }) => event.runId === record.event.runId)) return
    if (paused.value) buffered.value = mergeAuditRecords(buffered.value, [record], bufferedLimit)
    else events.value = mergeAuditRecords(events.value, [record], loadedLimit)
  }

  async function installSubscription(): Promise<void> {
    const token = subscription.begin()
    snapshotLatest.invalidate()
    if (connection.state === 'opening' || connection.state === 'reconnecting') {
      state.value = events.value.length === 0 ? 'loading' : 'ready'
      return
    }
    if (connection.state !== 'authenticated' || !supportsAudit.value) {
      error.value = connection.state === 'authenticated'
        ? 'The server does not provide every Audit RPC method required by this page.'
        : connection.error || 'Connect to cosmobot to load audit events.'
      state.value = events.value.length === 0 ? 'unavailable' : 'ready'
      return
    }
    state.value = events.value.length === 0 ? 'loading' : 'ready'
    const result = await runBackend(subscribeAudit(
      () => subscription.current(token) ? loadSnapshot() : Promise.resolve(),
      (record) => { if (subscription.current(token)) receive(record) },
    ))
    if (!subscription.current(token)) {
      if (result._tag === 'Success') subscription.own(token, result.value)
      return
    }
    if (result._tag === 'Failure') {
      error.value = result.error.message
      state.value = events.value.length === 0 ? 'error' : 'ready'
    } else subscription.own(token, result.value)
  }

  async function loadSelection(id: number): Promise<void> {
    selectedId.value = id
    selectedDetail.value = undefined
    related.value = []
    detailError.value = ''
    threadError.value = ''
    const token = detailLatest.begin()
    const result = await runBackend(getAudit(id))
    if (!detailLatest.current(token)) return
    if (result._tag === 'Failure') { detailError.value = result.error.message; return }
    if (result.value === null) { detailError.value = `Audit event #${String(id)} was not found.`; return }
    selectedDetail.value = result.value
    const target = await runBackend(resolveThreadRun(result.value.event.runId))
    if (!detailLatest.current(token)) return
    if (target._tag === 'Failure' || target.value.threadId === null) {
      related.value = events.value.filter(({ event }) => event.runId === result.value?.event.runId)
      return
    }
    const threadResult = await loadThreadAudit(target.value.threadId)
    if (!detailLatest.current(token)) return
    if (threadResult._tag === 'Success') related.value = [...threadResult.value]
    else threadError.value = threadResult.error.message
  }

  function platformLabel(record: AuditRecord): string {
    const value = auditPlatform(allKnownEvents.value, record.event.runId)
    return auditPlatformOptions.find((option) => option.value === value)?.label ?? 'Unlinked'
  }

  function resume(): void {
    events.value = mergeAuditRecords(events.value, buffered.value, loadedLimit)
    buffered.value = []
    paused.value = false
  }

  function discard(): void { buffered.value = []; paused.value = false }

  watch([() => connection.state, () => connection.methods], () => { void installSubscription() })
  onMounted(() => { void installSubscription() })

  return {
    state, error, events, buffered, paused, selectedId, selected, related, detailError, threadError,
    platforms, eventTypes, filteredEvents, loadSnapshot, loadSelection, installSubscription,
    platformLabel, resume, discard,
  }
}
