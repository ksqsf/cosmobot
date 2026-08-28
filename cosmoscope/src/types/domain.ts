export type Status = 'running' | 'waiting' | 'completed' | 'failed' | 'stopped' | 'degraded'

export type TaskStatus = 'running' | 'completed' | 'failed' | 'cancelled'

export interface Task {
  id: number
  label: string
  status: TaskStatus
  error: string | null
  startedAt: string
  finishedAt: string | null
}

export type ResourceProbe =
  | { readonly ok: true; readonly result: string }
  | { readonly ok: false; readonly error: string }

export interface Resource {
  readonly id: string
  readonly type: string
  readonly sessionId: string | null
  readonly description: string
  readonly probe: ResourceProbe
  readonly remainingLifeMinutes: number | null
}

export interface ResourceOperationResult {
  readonly ok: boolean
  readonly code?: string | undefined
  readonly error?: string | undefined
}

export interface AssociatedResource {
  readonly id: string
  readonly type: string
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

export interface ThreadSummary {
  readonly threadId: number
  readonly rootPreview: string
  readonly rootKey: ThreadMessageKey
  readonly latestKey: ThreadMessageKey
  readonly nodeCount: number
  readonly leafCount: number
}

export interface ThreadListQuery {
  readonly offset: number
  readonly limit: number
  readonly query?: string | undefined
  readonly platform?: AuditPlatform | undefined
}

export interface ThreadSnapshot {
  readonly threads: readonly ThreadSummary[]
  readonly total: number
  readonly nodes: number
  readonly leaves: number
  readonly platforms: number
}

export interface StoredThreadContentPart {
  readonly type: string
  readonly text?: string | undefined
  readonly image_url?: string | { readonly url: string } | undefined
}

export interface StoredThreadToolCall {
  readonly id: string
  readonly type: string
  readonly function: { readonly name: string; readonly arguments: string }
}

export interface StoredThreadMessage {
  readonly role: string
  readonly content?: string | readonly StoredThreadContentPart[] | null | undefined
  readonly tool_calls?: readonly StoredThreadToolCall[] | undefined
  readonly tool_call_id?: string | null | undefined
}

export interface ThreadNode {
  readonly messageKey: ThreadMessageKey
  readonly parentMessageKey: ThreadMessageKey | null
  readonly messages: readonly StoredThreadMessage[]
}

export interface ThreadDetail {
  readonly summary: ThreadSummary
  readonly nodes: readonly ThreadNode[]
}

export interface ActiveThread {
  readonly taskId: number
  readonly runId: string
  readonly prompt: string
  readonly parentMessageKey: ThreadMessageKey | null
  readonly messageKeys: readonly ThreadMessageKey[]
  readonly pendingSteers: number
  readonly messages: readonly StoredThreadMessage[]
}

export interface ThreadRunTarget {
  readonly threadId: number | null
  readonly taskId: number | null
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

export interface Plugin {
  id: string
  version: string
  generation: number
  required: boolean
  sandboxed: boolean
  tools: number
  routes: number
}

export type MemoryPlatform = 'qq' | 'telegram' | 'matrix' | 'discord' | 'rpc' | 'acp'
export type MemoryScope = 'sender' | 'chat'

export interface MemoryKey {
  readonly platform: MemoryPlatform
  readonly scope: MemoryScope
  readonly scopeId: string
}

export interface MemorySummary extends MemoryKey {
  readonly characters: number
}

export interface MemoryDetail extends MemorySummary {
  readonly content: string
}

export interface MemoryHistoryEntry {
  readonly revision: string
  readonly committedAt: string
  readonly subject: string
}

export interface SkillSummary {
  readonly name: string
  readonly description: string | null
}

export interface SkillDetail {
  readonly name: string
  readonly content: string
}

export interface MediaPlatformRef {
  readonly platform: string
  readonly scope: string
  readonly platformRef: string
}

export interface MediaItem {
  readonly mediaId: string
  readonly fileId: string
  readonly digest: string
  readonly mimeType: string
  readonly sourceName: string | null
  readonly size: number
  readonly createdAtUnix: number
  readonly lastUsedAtUnix: number
  readonly exists: boolean
  readonly sourceRefs: readonly string[]
  readonly platformRefs: readonly MediaPlatformRef[]
  readonly platforms: readonly string[]
  readonly sourceKinds: readonly MediaSourceKind[]
}

export type MediaSourceKind = 'chat' | 'generated-image' | 'tool-result' | 'sandbox'

export interface MediaDetail extends MediaItem {
  readonly publicUrl: string
}

export interface MediaStats {
  readonly files: number
  readonly existingFiles: number
  readonly missingFiles: number
  readonly totalBytes: number
  readonly sources: number
  readonly platformRefs: number
  readonly platformAssociations: number
  readonly mimeTypes: readonly string[]
  readonly platforms: readonly string[]
}

export interface MediaGcSettings {
  readonly enabled: boolean
  readonly maxAgeSeconds: number
  readonly intervalHours: number
}

export interface MediaSnapshot {
  readonly stats: MediaStats
  readonly files: readonly MediaItem[]
  readonly gcSettings: MediaGcSettings
}

export interface MediaSearch {
  readonly query?: string
  readonly platforms: readonly string[]
  readonly withoutPlatform: boolean
  readonly mimeTypes: readonly string[]
  readonly sourceKinds: readonly MediaSourceKind[]
  readonly limit?: number
}

export interface MediaGcResult {
  readonly deleted: number
  readonly retainedReferencedFiles: number
  readonly maxAgeSeconds: number
}

export interface LogEntry {
  id: string
  time: string
  level: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR'
  source: string
  message: string
  fields: string
}
