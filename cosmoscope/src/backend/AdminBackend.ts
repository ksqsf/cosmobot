import { Context, Data, type Effect } from 'effect'
import type { AuditRecord, ChatAttachment, ChatMessage, ChatSend, ChatSession, FixtureScenario, LogEntry, OverviewSnapshot, Plugin, Task } from '@/types/domain'

export class OfflineError extends Data.TaggedError('OfflineError')<{ readonly message: string }> {}
export class ForbiddenError extends Data.TaggedError('ForbiddenError')<{ readonly message: string }> {}
export class FixtureError extends Data.TaggedError('FixtureError')<{ readonly message: string }> {}
export class RpcBackendError extends Data.TaggedError('RpcBackendError')<{ readonly message: string }> {}
export type BackendError = OfflineError | ForbiddenError | FixtureError | RpcBackendError
export type BackendEffect<A> = Effect.Effect<A, BackendError>

export interface AdminBackend {
  readonly system: { readonly overview: (scenario?: FixtureScenario) => BackendEffect<OverviewSnapshot> }
  readonly tasks: { readonly list: () => BackendEffect<readonly Task[]> }
  readonly audit: {
    readonly recent: () => BackendEffect<readonly AuditRecord[]>
    readonly subscribe: (refresh: () => Promise<void>, handler: (record: AuditRecord) => void) => BackendEffect<() => void>
  }
  readonly chat: {
    readonly sessionCount: () => BackendEffect<number>
    readonly list: () => BackendEffect<readonly ChatSession[]>
    readonly open: (label?: string) => BackendEffect<ChatSession>
    readonly history: (sessionId: string) => BackendEffect<readonly ChatMessage[]>
    readonly fork: (sessionId: string, messageId: string, label?: string) => BackendEffect<ChatSession>
    readonly rename: (sessionId: string, label: string) => BackendEffect<ChatSession>
    readonly delete: (sessionId: string) => BackendEffect<boolean>
    readonly upload: (file: File) => BackendEffect<ChatAttachment>
    readonly discardAttachment: (attachmentId: string) => BackendEffect<boolean>
    readonly send: (message: ChatSend) => BackendEffect<string>
    readonly subscribe: (sessionId: string, refresh: () => Promise<void>, handler: (message: ChatMessage) => void, done: (messageId: string) => void) => BackendEffect<() => void>
  }
  readonly resources: { readonly count: () => BackendEffect<number> }
  readonly plugins: { readonly list: () => BackendEffect<readonly Plugin[]> }
  readonly logs: { readonly list: () => BackendEffect<readonly LogEntry[]> }
}

export const AdminBackendService = Context.Service<AdminBackend>('Cosmoscope/AdminBackend')
export const getOverview = (scenario?: FixtureScenario): Effect.Effect<OverviewSnapshot, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.system.overview(scenario))
export const listTasks: Effect.Effect<readonly Task[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.tasks.list())
export const recentAudit: Effect.Effect<readonly AuditRecord[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.audit.recent())
export const subscribeAudit = (refresh: () => Promise<void>, handler: (record: AuditRecord) => void): Effect.Effect<() => void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.audit.subscribe(refresh, handler))
export const countSessions: Effect.Effect<number, BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.chat.sessionCount())
export const listChatSessions: Effect.Effect<readonly ChatSession[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.chat.list())
export const openChatSession = (label?: string): Effect.Effect<ChatSession, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.open(label))
export const loadChatHistory = (sessionId: string): Effect.Effect<readonly ChatMessage[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.history(sessionId))
export const forkChatSession = (sessionId: string, messageId: string, label?: string): Effect.Effect<ChatSession, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.fork(sessionId, messageId, label))
export const renameChatSession = (sessionId: string, label: string): Effect.Effect<ChatSession, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.rename(sessionId, label))
export const deleteChatSession = (sessionId: string): Effect.Effect<boolean, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.delete(sessionId))
export const uploadChatAttachment = (file: File): Effect.Effect<ChatAttachment, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.upload(file))
export const discardChatAttachment = (attachmentId: string): Effect.Effect<boolean, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.discardAttachment(attachmentId))
export const sendChatMessage = (message: ChatSend): Effect.Effect<string, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.send(message))
export const subscribeChat = (sessionId: string, refresh: () => Promise<void>, handler: (message: ChatMessage) => void, done: (messageId: string) => void): Effect.Effect<() => void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.chat.subscribe(sessionId, refresh, handler, done))
export const countResources: Effect.Effect<number, BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.resources.count())
export const listPlugins: Effect.Effect<readonly Plugin[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.plugins.list())
export const listLogs: Effect.Effect<readonly LogEntry[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.logs.list())
