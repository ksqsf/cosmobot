import type { Activity, AuditEvent, AuditPlatform, AuditRecord, ThreadMessageKey } from '@/types/domain'

export type AuditCategory = 'agent' | 'model' | 'tool' | 'context' | 'thread'
export type AuditTone = Activity['tone']

export interface AuditPresentation {
  readonly kind: string
  readonly summary: string
  readonly category: AuditCategory
  readonly tone: AuditTone
}

export interface AuditDetailField {
  readonly label: string
  readonly value: string
  readonly kind?: 'run'
}

export type AuditSearchScope =
  | { readonly kind: 'thread'; readonly value: number }
  | { readonly kind: 'run'; readonly value: string }

export function parseAuditSearch(value: string): { readonly text: string; readonly scope?: AuditSearchScope } {
  let scope: AuditSearchScope | undefined
  const text = value.split(/\s+/).filter((token) => {
    if (scope !== undefined) return true
    const threadId = /^thread:(\d+)$/.exec(token)?.[1]
    if (threadId !== undefined) {
      const value = Number(threadId)
      if (Number.isSafeInteger(value) && value > 0) scope = { kind: 'thread', value }
      return false
    }
    const runId = /^run:(.+)$/.exec(token)?.[1]
    if (runId !== undefined) {
      scope = { kind: 'run', value: runId }
      return false
    }
    return true
  }).join(' ')
  return scope === undefined ? { text } : { text, scope }
}

export function mergeAuditRecords(records: readonly AuditRecord[], incoming: readonly AuditRecord[], limit: number): AuditRecord[] {
  const byId = new Map(records.map((record) => [record.id, record]))
  for (const record of incoming) byId.set(record.id, record)
  return [...byId.values()].sort((left, right) => left.id - right.id).slice(-limit)
}

export function isAuditFailure(event: AuditEvent): boolean {
  return event.tag === 'AgentRunInterrupted' || event.tag === 'ToolCallFinished' && event.status !== 'ok'
}

export function auditPresentation(event: AuditEvent): AuditPresentation {
  switch (event.tag) {
    case 'AgentRunStarted': return { kind: 'Agent started', summary: `Up to ${String(event.maxTurns)} turns`, category: 'agent', tone: 'info' }
    case 'ModelTurnStarted': return { kind: 'Model turn started', summary: `${String(event.messageCount)} transcript ${event.messageCount === 1 ? 'message' : 'messages'}`, category: 'model', tone: 'info' }
    case 'ModelTurnFinished': return { kind: 'Model turn finished', summary: event.answerKind, category: 'model', tone: 'success' }
    case 'ContextCompacted': return { kind: 'Context compacted', summary: `${String(event.messageCount)} messages remain`, category: 'context', tone: 'warning' }
    case 'RecursiveTranscriptFlushed': return { kind: 'Transcript flushed', summary: `After turn ${String(event.turn)}`, category: 'context', tone: 'info' }
    case 'SubAgentRunStarted': return { kind: 'Sub-agent started', summary: event.subagentId, category: 'agent', tone: 'info' }
    case 'ToolCallStarted': return { kind: 'Tool called', summary: event.toolCall.name, category: 'tool', tone: 'info' }
    case 'ToolCallFinished': return { kind: event.status === 'ok' ? 'Tool finished' : 'Tool failed', summary: event.toolName, category: 'tool', tone: event.status === 'ok' ? 'success' : 'danger' }
    case 'AgentRunFinished': return { kind: 'Agent finished', summary: event.status, category: 'agent', tone: 'success' }
    case 'AgentRunInterrupted': return { kind: 'Agent interrupted', summary: event.reason, category: 'agent', tone: 'danger' }
    case 'AgentThreadLinked': return { kind: 'Thread linked', summary: event.linkedMessageId, category: 'thread', tone: 'info' }
  }
}

export function auditDetailFields(event: AuditEvent): readonly AuditDetailField[] {
  const common = [{ label: 'Run', value: event.runId, kind: 'run' as const }]
  switch (event.tag) {
    case 'AgentRunStarted': return [...common, { label: 'Maximum turns', value: String(event.maxTurns) }, { label: 'Tools exposed', value: String(event.exposedTools.length) }, { label: 'Context strategy', value: event.contextStrategy ?? 'Default' }]
    case 'ModelTurnStarted': return [...common, { label: 'Turn', value: String(event.turn) }, { label: 'Messages', value: String(event.messageCount) }, { label: 'Tools exposed', value: String(event.exposedTools.length) }]
    case 'ModelTurnFinished': return [...common, { label: 'Turn', value: String(event.turn) }, { label: 'Answer kind', value: event.answerKind }, { label: 'Content length', value: formatSize(event.contentLength) }, { label: 'Tool calls', value: String(event.toolCalls.length) }]
    case 'ContextCompacted': return [...common, { label: 'Turn', value: String(event.turn) }, { label: 'Messages', value: String(event.messageCount) }]
    case 'RecursiveTranscriptFlushed': return [...common, { label: 'Turn', value: String(event.turn) }]
    case 'SubAgentRunStarted': return [...common, { label: 'Child run', value: event.childRunId, kind: 'run' }, { label: 'Sub-agent', value: event.subagentId }]
    case 'ToolCallStarted': return [...common, { label: 'Turn', value: String(event.turn) }, { label: 'Tool', value: event.toolCall.name }, { label: 'Call ID', value: event.toolCall.id }]
    case 'ToolCallFinished': return [...common, { label: 'Turn', value: String(event.turn) }, { label: 'Tool', value: event.toolName }, { label: 'Call ID', value: event.toolCallId }, { label: 'Status', value: event.status }, { label: 'Result size', value: formatSize(event.resultLength) }]
    case 'AgentRunFinished': return [...common, { label: 'Status', value: event.status }, { label: 'Turns used', value: String(event.turnsUsed) }, { label: 'Final length', value: formatSize(event.finalLength) }]
    case 'AgentRunInterrupted': return [...common, { label: 'Reason', value: event.reason }]
    case 'AgentThreadLinked': return [...common, { label: 'Message', value: event.linkedMessageId }, { label: 'Parent message', value: event.parentMessageId ?? 'None' }]
  }
}

export function auditArguments(event: AuditEvent): string | undefined {
  return event.tag === 'ToolCallStarted' ? event.toolCall.arguments : undefined
}

export function auditResult(event: AuditEvent): string | undefined {
  return event.tag === 'ToolCallFinished' ? event.result : undefined
}

export function linkedThread(records: readonly AuditRecord[], runId: string): ThreadMessageKey | undefined {
  for (let index = records.length - 1; index >= 0; index -= 1) {
    const event = records[index]?.event
    if (event?.runId === runId && event.tag === 'AgentThreadLinked' && event.linkedMessageKey !== null) return event.linkedMessageKey
  }
  return undefined
}

export function auditPlatform(records: readonly AuditRecord[], runId: string): AuditPlatform | undefined {
  return linkedThread(records, runId)?.platform
}

export function boundedStructuredText(value: string, limit = 12_000): { readonly text: string; readonly truncated: boolean } {
  if (value.length > limit) return { text: `${value.slice(0, limit)}\n…`, truncated: true }
  try {
    return { text: JSON.stringify(JSON.parse(value), null, 2), truncated: false }
  } catch {
    return { text: value, truncated: false }
  }
}

function formatSize(length: number): string {
  return length < 1_024 ? `${String(length)} characters` : `${(length / 1_024).toFixed(1)} KiB`
}
