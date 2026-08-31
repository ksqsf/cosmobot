import { ref, type Ref } from 'vue'
import { useRouter } from 'vue-router'
import { useToast } from 'primevue/usetoast'
import { collectMediaGarbage, deleteMedia } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useLatest } from '@/async'
import { formatBytes } from '@/format'
import { useLayeredConfirm } from '@/overlay'
import type { MediaGcSettings, MediaItem, MediaStats } from '@/types/domain'

export type MediaPendingAction = 'detail' | 'delete' | 'batch-delete' | 'gc'

export interface MediaOperations {
  pending: Ref<MediaPendingAction | undefined>
  requestDelete: (items: readonly MediaItem[]) => void
  requestGc: (force?: boolean) => void
}

export function useMediaOperations(stats: Ref<MediaStats>, gcSettings: Ref<MediaGcSettings>, refresh: () => Promise<void>): MediaOperations {
  const pending = ref<MediaPendingAction>()
  const router = useRouter()
  const toast = useToast()
  const confirm = useLayeredConfirm()
  const page = useLatest()
  const pageToken = page.begin()

  async function remove(ids: readonly string[]): Promise<void> {
    if (pending.value !== undefined) return
    pending.value = ids.length === 1 ? 'delete' : 'batch-delete'
    const results = []
    for (const id of ids) results.push(await runBackend(deleteMedia(id)))
    if (!page.current(pageToken)) return
    pending.value = undefined
    const failed = results.filter(({ _tag }) => _tag === 'Failure').length
    const deleted = ids.length - failed
    toast.add({ severity: failed === 0 ? 'success' : 'warn', summary: `Deleted ${String(deleted)} media object${deleted === 1 ? '' : 's'}`, detail: failed === 0 ? undefined : `${String(failed)} could not be deleted`, life: 3500 })
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
    if (!page.current(pageToken)) return
    pending.value = undefined
    if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
    await refresh()
    toast.add({
      severity: 'success',
      summary: `GC deleted ${String(result.value.deleted)} media object${result.value.deleted === 1 ? '' : 's'}`,
      detail: `${formatBytes(Math.max(0, before - stats.value.totalBytes))} freed · ${String(result.value.retainedReferencedFiles)} referenced objects retained`,
      life: 4500,
    })
  }

  function requestGc(force = false): void {
    const days = gcSettings.value.maxAgeSeconds / 86_400
    const age = `${Number.isInteger(days) ? String(days) : days.toFixed(1)} days`
    confirm.require({
      header: force ? 'Force garbage collection?' : 'Garbage collect media?',
      message: force || gcSettings.value.maxAgeSeconds === 0 ? 'Delete all unreferenced media regardless of age.' : `Delete unreferenced media older than ${age}.`,
      rejectLabel: 'Cancel', acceptLabel: 'Run garbage collection', acceptClass: force ? 'p-button-danger' : undefined,
      accept: () => { void runGc(force) },
    })
  }

  return { pending, requestDelete, requestGc }
}
