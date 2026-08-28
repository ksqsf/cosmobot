export const fixtureScenarios = ['ready', 'loading', 'empty', 'error', 'offline', 'forbidden'] as const
export type FixtureScenario = typeof fixtureScenarios[number]
export type Status = 'running' | 'waiting' | 'completed' | 'failed' | 'stopped' | 'degraded'

export interface Task {
  id: string
  label: string
  detail: string
  owner: string
  platform: string
  status: Status
  started: string
  elapsed: string
}

export interface Activity {
  id: string
  kind: string
  summary: string
  source: string
  time: string
  tone: 'success' | 'info' | 'warning' | 'danger'
}

export interface AuditRecord {
  id: number
  occurredAt: string
  event: AuditEvent
}

export type AuditPlatform = 'PlatformQQ' | 'PlatformTelegram' | 'PlatformMatrix' | 'PlatformDiscord' | 'PlatformRPC' | 'PlatformACP'

export interface ThreadMessageKey {
  readonly platform: AuditPlatform
  readonly chatId: string | null
  readonly messageId: string
}

export interface ToolCallTrace {
  readonly id: string
  readonly name: string
  readonly arguments: string
}

export interface TokenUsage {
  readonly prompt_tokens: number
  readonly completion_tokens: number
  readonly total_tokens: number
  readonly prompt_tokens_details?: { readonly cached_tokens: number } | undefined
}

export interface ChatSession {
  readonly sessionId: string
  readonly label: string | null
  readonly parentSessionId: string | null
  readonly parentMessageId: string | null
}

export interface ChatAttachment {
  readonly attachmentId: string
  readonly name: string
  readonly mediaType: string
  readonly kind: 'image' | 'audio' | 'file'
  readonly size: number
  readonly url: string
}

export interface ChatMessage {
  readonly sessionId: string
  readonly messageId: string
  readonly sender: 'user' | 'assistant'
  readonly text: string
  readonly imageUrls: readonly string[]
  readonly attachments: readonly ChatAttachment[]
  readonly replyToMessageId: string | null
  readonly parentMessageId: string | null
}

export interface ChatSend {
  readonly sessionId: string
  readonly text: string
  readonly imageUrls?: readonly string[]
  readonly attachments?: readonly ChatAttachment[]
}

export type AuditEvent =
  | { tag: 'AgentRunStarted'; runId: string; messageId: string | null; maxTurns: number; exposedTools: readonly string[]; contextStrategy: string | null }
  | { tag: 'ModelTurnStarted'; runId: string; turn: number; messageCount: number; exposedTools: readonly string[]; toolGroups: readonly (readonly [string, number])[] | null }
  | { tag: 'ModelTurnFinished'; runId: string; turn: number; answerKind: string; contentLength: number; toolCalls: readonly ToolCallTrace[]; tokenUsage: TokenUsage | null }
  | { tag: 'ContextCompacted'; runId: string; turn: number; messageCount: number; tokenUsage: TokenUsage | null }
  | { tag: 'RecursiveTranscriptFlushed'; runId: string; turn: number }
  | { tag: 'SubAgentRunStarted'; runId: string; childRunId: string; subagentId: string }
  | { tag: 'ToolCallStarted'; runId: string; turn: number; toolCall: ToolCallTrace }
  | { tag: 'ToolCallFinished'; runId: string; turn: number; toolCallId: string; toolName: string; status: string; result: string; resultLength: number; messageIds: readonly (string | null)[] }
  | { tag: 'AgentRunFinished'; runId: string; status: string; finalLength: number; turnsUsed: number }
  | { tag: 'AgentRunInterrupted'; runId: string; reason: string }
  | { tag: 'AgentThreadLinked'; runId: string; linkedMessageId: string; linkedMessageKey: ThreadMessageKey | null; parentMessageId: string | null }

export interface PlatformSummary {
  id: string
  name: string
  state: 'Connected' | 'Degraded'
  messages: number
}

export interface Plugin {
  id: string
  name: string
  description: string
  version: string
  tools: number
  routes: number
  status: 'Loaded' | 'Stopped'
  error?: string
}

export interface LogEntry {
  id: string
  time: string
  level: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR'
  source: string
  message: string
  fields: string
}

export interface OverviewSnapshot {
  tasks: Task[]
  activity: Activity[]
  platforms: PlatformSummary[]
  sessionCount: number
  resourceCount: number
}
