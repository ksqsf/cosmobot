import { computed, ref, type ComputedRef, type Ref } from 'vue'
import { getRunAudit, getRunTranscript, resolveThreadRun, subscribeAudit } from '@/backend/AdminBackend'
import { useVisibilityPolling } from '@/composables/useVisibilityPolling'
import { runBackend } from '@/backend/runBackend'
import { mergeAuditRecords } from '@/backend/audit'
import { useLatest, useLatestSubscription } from '@/async'
import type { SubagentRun } from '@/domain/subagents'
import type { AuditRecord, StoredThreadMessage } from '@/types/domain'

interface SubagentInspector {
  trail: Ref<SubagentRun[]>
  selected: ComputedRef<SubagentRun | undefined>
  records: Ref<AuditRecord[]>
  loading: Ref<boolean>
  error: Ref<string>
  liveError: Ref<string>
  linkError: Ref<string>
  hasThread: Ref<boolean>
  messages: Ref<readonly StoredThreadMessage[] | null>
  transcriptError: Ref<string>
  transcriptLoading: Ref<boolean>
  select: (child: SubagentRun) => void
  back: (index: number) => void
  reload: () => Promise<void>
}

export function useSubagentInspector(): SubagentInspector {
  const trail = ref<SubagentRun[]>([])
  const selected = computed(() => trail.value.at(-1))
  const records = ref<AuditRecord[]>([])
  const loading = ref(false)
  const error = ref('')
  const liveError = ref('')
  const linkError = ref('')
  const hasThread = ref(false)
  const subscription = useLatestSubscription()
  const snapshot = useLatest()
  const link = useLatest()
  const transcriptLatest = useLatest()
  const messages = ref<readonly StoredThreadMessage[] | null>(null)
  const transcriptError = ref('')
  const transcriptLoading = ref(false)
  const polling = useVisibilityPolling(refreshTranscript, { interval: 1000 })

  async function refreshTranscript(): Promise<void> {
    const runId = selected.value?.childRunId
    if (runId === undefined) return
    const token = transcriptLatest.begin()
    const result = await runBackend(getRunTranscript(runId))
    if (!transcriptLatest.current(token)) return
    transcriptLoading.value = false
    if (result._tag === 'Failure') { transcriptError.value = result.error.message; return }
    transcriptError.value = ''
    messages.value = result.value
  }

  async function load(): Promise<void> {
    const token = subscription.begin()
    snapshot.invalidate()
    link.invalidate()
    transcriptLatest.invalidate()
    polling.stop()
    messages.value = null
    transcriptError.value = ''
    transcriptLoading.value = selected.value !== undefined
    records.value = []
    error.value = ''
    liveError.value = ''
    linkError.value = ''
    hasThread.value = false
    const child = selected.value
    loading.value = child !== undefined
    if (child === undefined) return
    const runId = child.childRunId
    polling.start()

    async function resolveLink(): Promise<void> {
      if (!subscription.current(token)) return
      const request = link.begin()
      const result = await runBackend(resolveThreadRun(runId))
      if (!subscription.current(token) || !link.current(request)) return
      if (result._tag === 'Failure') { linkError.value = result.error.message; return }
      linkError.value = ''
      hasThread.value = result.value.threadId !== null || result.value.taskId !== null
    }

    async function refresh(): Promise<void> {
      if (!subscription.current(token)) return
      const request = snapshot.begin()
      const result = await runBackend(getRunAudit(runId))
      if (!subscription.current(token) || !snapshot.current(request)) return
      loading.value = false
      if (result._tag === 'Failure') { error.value = result.error.message; return }
      error.value = ''
      records.value = mergeAuditRecords(result.value.filter(({ event }) => event.runId === runId), records.value, Number.POSITIVE_INFINITY)
    }

    await Promise.all([
      (async () => {
        const result = await runBackend(subscribeAudit(async () => { await Promise.all([refresh(), resolveLink()]) }, (record) => {
          if (subscription.current(token) && record.event.runId === runId) {
            records.value = mergeAuditRecords(records.value, [record], Number.POSITIVE_INFINITY)
            if (record.event.tag === 'AgentThreadLinked') void resolveLink()
          }
        }))
        if (result._tag === 'Success') subscription.own(token, result.value)
        else if (subscription.current(token)) liveError.value = result.error.message
      })(),
      refresh(),
      resolveLink(),
      refreshTranscript(),
    ])
  }

  function select(child: SubagentRun): void {
    if (trail.value.some(({ childRunId }) => childRunId === child.childRunId)) return
    trail.value.push(child)
    void load()
  }

  function back(index: number): void {
    trail.value = trail.value.slice(0, index)
    void load()
  }

  return { trail, selected, records, loading, error, liveError, linkError, hasThread, messages, transcriptError, transcriptLoading, select, back, reload: load }
}
