import type { Activity, AuditRecord, Task } from '@/types/domain'
import { auditPresentation, isAuditFailure, mergeAuditRecords as mergeAuditRecordWindow } from './audit'

export function taskCounts(tasks: readonly Task[]): { active: number; completed: number; failed: number } {
  return tasks.reduce((counts, task) => {
    if (task.status === 'running' || task.status === 'waiting') counts.active += 1
    else if (task.status === 'completed') counts.completed += 1
    else if (task.status === 'failed') counts.failed += 1
    return counts
  }, { active: 0, completed: 0, failed: 0 })
}

export function auditFailureCount(records: readonly AuditRecord[]): number {
  return records.filter(({ event }) => isAuditFailure(event)).length
}

export function mergeAuditRecords(records: readonly AuditRecord[], incoming: AuditRecord, limit = 20): AuditRecord[] {
  return mergeAuditRecordWindow(records, [incoming], limit)
}

export function auditActivity(records: readonly AuditRecord[]): Activity[] {
  return [...records].reverse().map(({ id, occurredAt, event }) => ({
    id: String(id),
    ...auditPresentation(event),
    source: event.runId,
    time: new Date(occurredAt).toLocaleTimeString(),
  }))
}
