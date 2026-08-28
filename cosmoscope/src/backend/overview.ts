import type { Activity, AuditEvent, AuditRecord, Task } from '@/types/domain'

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
  return [...records.filter(({ id }) => id !== incoming.id), incoming]
    .sort((left, right) => left.id - right.id)
    .slice(-limit)
}

export function auditActivity(records: readonly AuditRecord[]): Activity[] {
  return [...records].reverse().map(({ id, occurredAt, event }) => ({
    id: String(id),
    ...auditPresentation(event),
    source: event.runId,
    time: new Date(occurredAt).toLocaleTimeString(),
  }))
}

function isAuditFailure(event: AuditEvent): boolean {
  return event.tag === 'AgentRunInterrupted' || event.tag === 'ToolCallFinished' && event.status !== 'ok'
}

function auditPresentation(event: AuditEvent): Pick<Activity, 'kind' | 'summary' | 'tone'> {
  switch (event.tag) {
    case 'AgentRunStarted': return { kind: 'Agent started', summary: 'Run accepted', tone: 'info' }
    case 'ModelTurnStarted': return { kind: 'Model turn started', summary: `Turn ${String(event.turn)}`, tone: 'info' }
    case 'ModelTurnFinished': return { kind: 'Model turn finished', summary: event.answerKind, tone: 'success' }
    case 'ContextCompacted': return { kind: 'Context compacted', summary: `After turn ${String(event.turn)}`, tone: 'warning' }
    case 'RecursiveTranscriptFlushed': return { kind: 'Transcript flushed', summary: `After turn ${String(event.turn)}`, tone: 'info' }
    case 'SubAgentRunStarted': return { kind: 'Sub-agent started', summary: event.subagentId, tone: 'info' }
    case 'ToolCallStarted': return { kind: 'Tool called', summary: event.toolCall.name, tone: 'info' }
    case 'ToolCallFinished': return { kind: event.status === 'ok' ? 'Tool finished' : 'Tool failed', summary: event.toolName, tone: event.status === 'ok' ? 'success' : 'danger' }
    case 'AgentRunFinished': return { kind: 'Agent finished', summary: event.status, tone: 'success' }
    case 'AgentRunInterrupted': return { kind: 'Agent interrupted', summary: event.reason, tone: 'danger' }
    case 'AgentThreadLinked': return { kind: 'Thread linked', summary: 'Conversation link recorded', tone: 'info' }
  }
}
