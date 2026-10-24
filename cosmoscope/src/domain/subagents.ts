import type { AuditRecord } from '@/types/domain'

export interface SubagentRun {
  readonly runId: string
  readonly childRunId: string
  readonly subagentId: string
}

export function subagentRuns(records: readonly AuditRecord[]): SubagentRun[] {
  const children = new Map<string, SubagentRun>()
  for (const { event } of [...records].sort((a, b) => a.id - b.id)) {
    if (event.tag === 'SubAgentRunStarted') children.set(event.childRunId, event)
  }
  return [...children.values()]
}

export function subagentStatus(records: readonly AuditRecord[]): string {
  const terminal = [...records].sort((a, b) => b.id - a.id).find(({ event }) =>
    event.tag === 'AgentRunFinished' || event.tag === 'AgentRunInterrupted' || event.tag === 'AgentChildrenWait' || event.tag === 'ModelTurnStarted',
  )?.event
  if (terminal?.tag === 'AgentChildrenWait') return terminal.waiting ? 'Waiting for subagents' : 'Running'
  if (terminal?.tag === 'ModelTurnStarted') return 'Running'
  if (terminal?.tag === 'AgentRunFinished') return terminal.status
  if (terminal?.tag === 'AgentRunInterrupted') return `Interrupted: ${terminal.reason}`
  return 'Unknown'
}
