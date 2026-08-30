import { threadMessageKeyId } from '@/backend/thread'
import type { AuditRecord, ThreadMessageKey, TokenUsage } from '@/types/domain'

export interface ThreadStats {
  readonly runs: number
  readonly modelTurns: number
  readonly toolCalls: number
  readonly failedTools: number
  readonly compactions: number
  readonly subagents: number
  readonly contextMessages: number
  readonly tokens: TokenUsage | null
  readonly cachedPromptTokens: number | null
  readonly promptCacheHitRate: number | null
  readonly modelMilliseconds: number
  readonly toolMilliseconds: number
  readonly wallMilliseconds: number
  readonly unreportedModelTurns: number
  readonly unreportedToolCalls: number
}

export function auditRecordsLinkedTo(records: readonly AuditRecord[], messageKeys: readonly ThreadMessageKey[]): AuditRecord[] {
  const wanted = new Set(messageKeys.map(threadMessageKeyId))
  const selected: AuditRecord[] = []
  let occurrence: AuditRecord[] = []
  for (const record of records) {
    occurrence.push(record)
    if (record.event.tag !== 'AgentThreadLinked') continue
    if (record.event.linkedMessageKey !== null && wanted.has(threadMessageKeyId(record.event.linkedMessageKey))) selected.push(...occurrence)
    occurrence = []
  }
  return selected
}

export function threadStats(records: readonly AuditRecord[]): ThreadStats {
  const ordered = [...records].sort((left, right) => left.id - right.id)
  const modelStarts = ordered.filter((record) => record.event.tag === 'ModelTurnStarted')
  const toolStarts = ordered.filter((record) => record.event.tag === 'ToolCallStarted')
  const modelDurations = modelStarts.map((start) => durationTo(ordered, start, (candidate) =>
    candidate.event.tag === 'ModelTurnFinished'
      && start.event.tag === 'ModelTurnStarted'
      && candidate.event.runId === start.event.runId
      && candidate.event.turn === start.event.turn,
  ))
  const toolDurations = toolStarts.map((start) => durationTo(ordered, start, (candidate) =>
    candidate.event.tag === 'ToolCallFinished'
      && start.event.tag === 'ToolCallStarted'
      && candidate.event.toolCallId === start.event.toolCall.id,
  ))
  const runStarts = ordered.filter((record) => record.event.tag === 'AgentRunStarted')
  const wallDurations = runStarts.flatMap((start) => {
    const duration = durationTo(ordered, start, (candidate) =>
      (candidate.event.tag === 'AgentRunFinished' || candidate.event.tag === 'AgentRunInterrupted')
        && candidate.event.runId === start.event.runId,
    )
    return duration === undefined ? [] : [duration]
  })
  const reportedUsages = ordered.flatMap(({ event }) => event.tag === 'ModelTurnFinished' && event.tokenUsage !== null ? [event.tokenUsage] : [])
  const usages = reportedUsages.length === modelStarts.length && reportedUsages.length > 0 ? reportedUsages : null
  const cached = usages?.map(({ prompt_tokens_details }) => prompt_tokens_details?.cached_tokens).filter((value): value is number => value !== undefined) ?? []
  const tokens = usages === null ? null : {
    prompt_tokens: usages.reduce((sum, usage) => sum + usage.prompt_tokens, 0),
    completion_tokens: usages.reduce((sum, usage) => sum + usage.completion_tokens, 0),
    total_tokens: usages.reduce((sum, usage) => sum + usage.total_tokens, 0),
    ...(cached.length === usages.length && cached.length > 0 ? { prompt_tokens_details: { cached_tokens: cached.reduce((sum, value) => sum + value, 0) } } : {}),
  }
  const cachedPromptTokens = usages !== null && cached.length === usages.length ? cached.reduce((sum, value) => sum + value, 0) : null
  return {
    runs: new Set(runStarts.map(({ event }) => event.runId)).size,
    modelTurns: modelStarts.length,
    toolCalls: toolStarts.length,
    failedTools: ordered.filter(({ event }) => event.tag === 'ToolCallFinished' && !/^(?:ok|success|succeeded)$/i.test(event.status)).length,
    compactions: ordered.filter(({ event }) => event.tag === 'ContextCompacted').length,
    subagents: new Set(ordered.flatMap(({ event }) => event.tag === 'SubAgentRunStarted' ? [event.childRunId] : [])).size,
    contextMessages: Math.max(0, ...ordered.flatMap(({ event }) => event.tag === 'ModelTurnStarted' ? [event.messageCount] : [])),
    tokens,
    cachedPromptTokens,
    promptCacheHitRate: cachedPromptTokens === null || tokens === null || tokens.prompt_tokens === 0 ? null : cachedPromptTokens / tokens.prompt_tokens,
    modelMilliseconds: sumReported(modelDurations),
    toolMilliseconds: sumReported(toolDurations),
    wallMilliseconds: wallDurations.reduce((sum, value) => sum + value, 0),
    unreportedModelTurns: modelDurations.filter((value) => value === undefined).length,
    unreportedToolCalls: toolDurations.filter((value) => value === undefined).length,
  }
}

function durationTo(records: readonly AuditRecord[], start: AuditRecord, matches: (record: AuditRecord) => boolean): number | undefined {
  const finish = records.find((record) => record.id > start.id && matches(record))
  if (finish === undefined) return undefined
  return Math.max(0, Date.parse(finish.occurredAt) - Date.parse(start.occurredAt))
}

function sumReported(values: readonly (number | undefined)[]): number {
  return values.reduce<number>((sum, value) => sum + (value ?? 0), 0)
}
