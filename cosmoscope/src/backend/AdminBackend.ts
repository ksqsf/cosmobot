import { Context, Data, type Effect } from 'effect'
import type { AssociatedResource, AuditRecord, ChatAttachment, ChatMessage, ChatSend, ChatSession, LogEntry, MediaDetail, MediaGcResult, MediaItem, MediaSearch, MediaSnapshot, Plugin, Resource, ResourceOperationResult, Task, ThreadMessageKey } from '@/types/domain'

export class OfflineError extends Data.TaggedError('OfflineError')<{ readonly message: string }> {}
export class RpcBackendError extends Data.TaggedError('RpcBackendError')<{ readonly message: string }> {}
export type BackendError = OfflineError | RpcBackendError
export type BackendEffect<A> = Effect.Effect<A, BackendError>

export interface AdminBackend {
  readonly tasks: {
    readonly list: () => BackendEffect<readonly Task[]>
    readonly lookup: (id: number) => BackendEffect<Task | null>
    readonly cancel: (id: number) => BackendEffect<boolean>
    readonly await: (id: number) => BackendEffect<void>
    readonly associated: (id: number) => BackendEffect<readonly AssociatedResource[]>
    readonly destroyAssociated: (id: number) => BackendEffect<readonly ResourceOperationResult[]>
  }
  readonly audit: {
    readonly recent: (limit?: number) => BackendEffect<readonly AuditRecord[]>
    readonly get: (id: number) => BackendEffect<AuditRecord | null>
    readonly thread: (key: ThreadMessageKey) => BackendEffect<readonly AuditRecord[]>
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
  readonly resources: {
    readonly count: () => BackendEffect<number>
    readonly list: () => BackendEffect<readonly Resource[]>
    readonly detail: (id: string) => BackendEffect<string>
    readonly destroy: (id: string) => BackendEffect<void>
    readonly rename: (id: string, newId: string) => BackendEffect<string>
    readonly keepAlive: (id: string) => BackendEffect<void>
    readonly makePermanent: (id: string) => BackendEffect<void>
  }
  readonly media: {
    readonly list: (limit?: number) => BackendEffect<MediaSnapshot>
    readonly search: (search: MediaSearch) => BackendEffect<readonly MediaItem[]>
    readonly get: (id: string) => BackendEffect<MediaDetail>
    readonly delete: (id: string) => BackendEffect<boolean>
    readonly gc: (maxAgeSeconds?: number) => BackendEffect<MediaGcResult>
  }
  readonly plugins: {
    readonly list: () => BackendEffect<readonly Plugin[]>
    readonly load: (id: string) => BackendEffect<Plugin>
    readonly reload: (id: string) => BackendEffect<Plugin>
    readonly unload: (id: string) => BackendEffect<void>
  }
  readonly logs: { readonly list: () => BackendEffect<readonly LogEntry[]> }
}

export const AdminBackendService = Context.Service<AdminBackend>('Cosmoscope/AdminBackend')
export const listTasks: Effect.Effect<readonly Task[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.tasks.list())
export const lookupTask = (id: number): Effect.Effect<Task | null, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.tasks.lookup(id))
export const cancelTask = (id: number): Effect.Effect<boolean, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.tasks.cancel(id))
export const awaitTask = (id: number): Effect.Effect<void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.tasks.await(id))
export const listTaskResources = (id: number): Effect.Effect<readonly AssociatedResource[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.tasks.associated(id))
export const destroyTaskResources = (id: number): Effect.Effect<readonly ResourceOperationResult[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.tasks.destroyAssociated(id))
export const recentAudit = (limit = 20): Effect.Effect<readonly AuditRecord[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.audit.recent(limit))
export const getAudit = (id: number): Effect.Effect<AuditRecord | null, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.audit.get(id))
export const getAuditThread = (key: ThreadMessageKey): Effect.Effect<readonly AuditRecord[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.audit.thread(key))
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
export const listResources: Effect.Effect<readonly Resource[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.resources.list())
export const getResourceDetail = (id: string): Effect.Effect<string, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.resources.detail(id))
export const destroyResource = (id: string): Effect.Effect<void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.resources.destroy(id))
export const renameResource = (id: string, newId: string): Effect.Effect<string, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.resources.rename(id, newId))
export const keepResourceAlive = (id: string): Effect.Effect<void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.resources.keepAlive(id))
export const makeResourcePermanent = (id: string): Effect.Effect<void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.resources.makePermanent(id))
export const listMedia = (limit = 200): Effect.Effect<MediaSnapshot, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.media.list(limit))
export const searchMedia = (search: MediaSearch): Effect.Effect<readonly MediaItem[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.media.search(search))
export const getMedia = (id: string): Effect.Effect<MediaDetail, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.media.get(id))
export const deleteMedia = (id: string): Effect.Effect<boolean, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.media.delete(id))
export const collectMediaGarbage = (maxAgeSeconds?: number): Effect.Effect<MediaGcResult, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.media.gc(maxAgeSeconds))
export const listPlugins: Effect.Effect<readonly Plugin[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.plugins.list())
export const loadPlugin = (id: string): Effect.Effect<Plugin, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.plugins.load(id))
export const reloadPlugin = (id: string): Effect.Effect<Plugin, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.plugins.reload(id))
export const unloadPlugin = (id: string): Effect.Effect<void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.plugins.unload(id))
export const listLogs: Effect.Effect<readonly LogEntry[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.logs.list())
