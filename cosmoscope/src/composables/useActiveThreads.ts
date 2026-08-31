import { computed, onScopeDispose, ref, type ComputedRef, type Ref } from 'vue'
import { getRunAudit, listActiveThreads, subscribeAudit } from '@/backend/AdminBackend'
import { mergeAuditRecords } from '@/backend/audit'
import { runBackend } from '@/backend/runBackend'
import { threadToolActivity } from '@/backend/thread'
import { threadStats } from '@/backend/threadStats'
import type { ThreadStats } from '@/backend/threadStats'
import type { ThreadToolActivity } from '@/backend/thread'
import { useLatest, useLatestSubscription, type Latest, type LatestToken } from '@/async'
import { useConnectionStore } from '@/stores/connection'
import type { ActiveThread, AuditRecord, StoredThreadMessage } from '@/types/domain'

interface ActiveThreadOptions {
  readonly visible: Ref<boolean>
  readonly inspectorLatest: Latest
  readonly loadMediaForMessages: (messages: readonly StoredThreadMessage[], current?: () => boolean) => Promise<void>
  readonly transcriptIsPinned: () => boolean
  readonly scrollTranscriptToEnd: () => Promise<void>
  readonly onFinalized: (active: ActiveThread, selectionToken: LatestToken) => Promise<void>
}

interface ActiveThreads {
  readonly activeThreads: Ref<ActiveThread[]>
  readonly activeTaskId: Ref<number | undefined>
  readonly activeSnapshot: Ref<ActiveThread | undefined>
  readonly auditRecords: Ref<AuditRecord[]>
  readonly error: Ref<string>
  readonly auditError: Ref<string>
  readonly selected: ComputedRef<ActiveThread | undefined>
  readonly stats: ComputedRef<ThreadStats>
  readonly tools: ComputedRef<ThreadToolActivity[]>
  readonly transcriptMessages: ComputedRef<StoredThreadMessage[]>
  readonly selectionToken: () => LatestToken | undefined
  readonly refresh: () => Promise<void>
  readonly refreshAudit: (runId: string) => Promise<void>
  readonly select: (active: ActiveThread, token: LatestToken) => void
  readonly clear: () => void
  readonly startPolling: () => void
  readonly poll: () => Promise<void>
}

export function useActiveThreads(options: ActiveThreadOptions): ActiveThreads {
  const connection = useConnectionStore()
  const latest = useLatest()
  const auditLatest = useLatest()
  const auditSubscription = useLatestSubscription()
  const activeThreads = ref<ActiveThread[]>([])
  const activeTaskId = ref<number>()
  const activeSnapshot = ref<ActiveThread>()
  const auditRecords = ref<AuditRecord[]>([])
  const error = ref('')
  const auditError = ref('')
  const selected = computed(() => activeThreads.value.find(({ taskId }) => taskId === activeTaskId.value) ?? activeSnapshot.value)
  const stats = computed(() => threadStats(auditRecords.value))
  const tools = computed(() => threadToolActivity(auditRecords.value))
  const transcriptMessages = computed<StoredThreadMessage[]>(() => [...(selected.value?.messages ?? [])])
  let selectionToken: LatestToken | undefined
  let monitorTimer: number | undefined
  let polling = false

  async function refresh(): Promise<void> {
    const token = latest.begin()
    if (!latest.current(token)) return
    if (connection.state !== 'authenticated') { activeThreads.value = []; return }
    const monitored = selected.value
    const currentSelection = selectionToken
    const keepTranscriptPinned = options.transcriptIsPinned()
    const result = await runBackend(listActiveThreads)
    if (!latest.current(token)) return
    if (result._tag === 'Failure') { error.value = result.error.message; return }
    error.value = ''
    activeThreads.value = [...result.value]
    const current = result.value.find(({ taskId }) => taskId === activeTaskId.value)
    if (current !== undefined) {
      activeSnapshot.value = current
      await Promise.all([
        auditSubscription.owned() ? Promise.resolve() : refreshAudit(current.runId),
        options.loadMediaForMessages(current.messages, () => currentSelection === selectionToken),
      ])
      if (latest.current(token) && keepTranscriptPinned) await options.scrollTranscriptToEnd()
    } else if (monitored !== undefined && currentSelection !== undefined && options.visible.value) {
      await options.onFinalized(monitored, currentSelection)
    }
  }

  async function refreshAudit(runId: string): Promise<void> {
    const token = auditLatest.begin()
    if (!auditLatest.current(token)) return
    const result = await runBackend(getRunAudit(runId))
    if (!auditLatest.current(token) || selected.value?.runId !== runId) return
    if (result._tag === 'Failure') { auditError.value = result.error.message; return }
    auditError.value = ''
    auditRecords.value = [...result.value]
    await options.loadMediaForMessages(result.value.flatMap(({ event }) => event.tag === 'ToolCallFinished'
      ? [{ role: 'tool', content: event.result } satisfies StoredThreadMessage]
      : []), () => selected.value?.runId === runId)
  }

  async function installAuditSubscription(): Promise<void> {
    const token = auditSubscription.begin()
    if (!auditSubscription.current(token)) return
    const runId = selected.value?.runId
    if (runId === undefined) return
    const result = await runBackend(subscribeAudit(
      async () => {
        if (auditSubscription.current(token)) await refreshAudit(runId)
      },
      (record) => {
        if (!auditSubscription.current(token) || record.event.runId !== runId) return
        auditRecords.value = mergeAuditRecords(auditRecords.value, [record], 2_000)
      },
    ))
    if (result._tag === 'Success') {
      if (auditSubscription.current(token) && (!options.visible.value || selected.value?.runId !== runId)) auditSubscription.invalidate()
      auditSubscription.own(token, result.value)
    }
  }

  function select(active: ActiveThread, token: LatestToken): void {
    if (!options.inspectorLatest.current(token)) return
    selectionToken = token
    activeTaskId.value = active.taskId
    activeSnapshot.value = active
    auditRecords.value = []
    auditError.value = ''
    void installAuditSubscription()
    void options.loadMediaForMessages(active.messages, () => selectionToken === token)
  }

  function clear(): void {
    auditSubscription.invalidate()
    auditLatest.invalidate()
    selectionToken = undefined
    activeTaskId.value = undefined
    activeSnapshot.value = undefined
    auditRecords.value = []
  }

  function stopPolling(): void {
    if (monitorTimer !== undefined) window.clearTimeout(monitorTimer)
    monitorTimer = undefined
  }

  function schedulePolling(): void {
    stopPolling()
    if (!polling || document.hidden || connection.state !== 'authenticated') return
    monitorTimer = window.setTimeout(() => { void poll() }, 1000)
  }

  async function poll(): Promise<void> {
    await refresh()
    schedulePolling()
  }

  function visibilityChanged(): void {
    if (document.hidden) stopPolling()
    else void poll()
  }

  function startPolling(): void {
    polling = true
    void poll()
    document.addEventListener('visibilitychange', visibilityChanged)
  }

  function dispose(): void {
    polling = false
    stopPolling()
    clear()
    document.removeEventListener('visibilitychange', visibilityChanged)
  }

  onScopeDispose(dispose)

  return {
    activeThreads, activeTaskId, activeSnapshot, auditRecords, error, auditError, selected, stats, tools,
    transcriptMessages, selectionToken: () => selectionToken, refresh, refreshAudit, select, clear, startPolling, poll,
  }
}
