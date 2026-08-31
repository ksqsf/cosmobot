import { computed, ref, type ComputedRef, type Ref } from 'vue'
import { getThread, getThreadAudit } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { threadMessageKeyId, threadPathTo } from '@/backend/thread'
import { auditRecordsLinkedTo, threadStats } from '@/backend/threadStats'
import { useLatest, type Latest, type LatestToken } from '@/async'
import { transcriptEntries, type ThreadTranscriptEntry } from '@/domain/threadTranscript'
import type { ThreadStats } from '@/backend/threadStats'
import type { AuditRecord, StoredThreadMessage, ThreadDetail, ThreadNode } from '@/types/domain'

interface ThreadInspector {
  readonly latest: Latest
  readonly detail: Ref<ThreadDetail | undefined>
  readonly auditRecords: Ref<AuditRecord[]>
  readonly selectedNode: Ref<ThreadNode | undefined>
  readonly selectedKeys: Ref<Record<string, boolean>>
  readonly expandedKeys: Ref<Record<string, boolean>>
  readonly visible: Ref<boolean>
  readonly treeFocused: Ref<boolean>
  readonly treeZoom: Ref<number>
  readonly detailError: Ref<string>
  readonly statsError: Ref<string>
  readonly detailLoading: Ref<boolean>
  readonly nodeLookup: ComputedRef<Map<string, ThreadNode>>
  readonly selectedPath: ComputedRef<ThreadNode[]>
  readonly transcript: ComputedRef<ThreadTranscriptEntry[]>
  readonly stats: ComputedRef<ThreadStats>
  readonly inspectThread: (threadId: number, selectionToken?: LatestToken) => Promise<void>
  readonly selectNode: (node: ThreadNode) => void
  readonly reset: () => void
}

export function useThreadInspector(
  clearActiveSelection: () => void,
  resetMedia: () => void,
  loadMediaForMessages: (messages: readonly StoredThreadMessage[], current?: () => boolean) => Promise<void>,
): ThreadInspector {
  const latest = useLatest()
  const detail = ref<ThreadDetail>()
  const auditRecords = ref<AuditRecord[]>([])
  const selectedNode = ref<ThreadNode>()
  const selectedKeys = ref<Record<string, boolean>>({})
  const expandedKeys = ref<Record<string, boolean>>({})
  const visible = ref(false)
  const treeFocused = ref(false)
  const treeZoom = ref(100)
  const detailError = ref('')
  const statsError = ref('')
  const detailLoading = ref(false)
  const nodeLookup = computed(() => new Map((detail.value?.nodes ?? []).map((node) => [threadMessageKeyId(node.messageKey), node])))
  const selectedPath = computed(() => selectedNode.value === undefined ? [] : threadPathTo(selectedNode.value, nodeLookup.value))
  const transcript = computed(() => transcriptEntries(selectedPath.value))
  const stats = computed(() => threadStats(auditRecordsLinkedTo(auditRecords.value, selectedPath.value.map(({ messageKey }) => messageKey))))

  async function inspectThread(threadId: number, selectionToken = latest.begin()): Promise<void> {
    if (!latest.current(selectionToken)) return
    clearActiveSelection()
    visible.value = true
    detailLoading.value = true
    detailError.value = ''
    statsError.value = ''
    detail.value = undefined
    auditRecords.value = []
    selectedNode.value = undefined
    treeFocused.value = false
    treeZoom.value = 100
    resetMedia()
    const result = await runBackend(getThread(threadId))
    if (!latest.current(selectionToken)) return
    detailLoading.value = false
    if (result._tag === 'Failure') { detailError.value = result.error.message; return }
    if (result.value === null) { detailError.value = `Thread #${String(threadId)} was not found.`; return }
    detail.value = result.value
    expandedKeys.value = Object.fromEntries(result.value.nodes.map((node) => [threadMessageKeyId(node.messageKey), true]))
    const selected = nodeLookup.value.get(threadMessageKeyId(result.value.summary.latestKey)) ?? result.value.nodes.at(-1)
    if (selected !== undefined) selectNode(selected)
    await Promise.all([loadThreadMedia(result.value, selectionToken), loadThreadStats(result.value, selectionToken)])
  }

  async function loadThreadStats(thread: ThreadDetail, selectionToken: LatestToken): Promise<void> {
    const result = await runBackend(getThreadAudit(thread.summary.threadId))
    if (!latest.current(selectionToken)) return
    if (result._tag === 'Failure') { statsError.value = result.error.message; return }
    if (result.value === null) { statsError.value = `Thread #${String(thread.summary.threadId)} was not found.`; return }
    auditRecords.value = [...result.value]
  }

  async function loadThreadMedia(thread: ThreadDetail, selectionToken: LatestToken): Promise<void> {
    await loadMediaForMessages(thread.nodes.flatMap(({ messages }) => messages), () => latest.current(selectionToken))
  }

  function selectNode(node: ThreadNode): void {
    selectedNode.value = node
    selectedKeys.value = { [threadMessageKeyId(node.messageKey)]: true }
  }

  function reset(): void {
    latest.invalidate()
    visible.value = false
    detail.value = undefined
    selectedNode.value = undefined
    treeFocused.value = false
  }

  return {
    latest, detail, auditRecords, selectedNode, selectedKeys, expandedKeys, visible, treeFocused, treeZoom,
    detailError, statsError, detailLoading, nodeLookup, selectedPath, transcript, stats,
    inspectThread, selectNode, reset,
  }
}
