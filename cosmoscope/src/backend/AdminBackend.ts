import { Context, Data, type Effect } from 'effect'
import type { ActiveThread, AssociatedResource, AuditRecord, ChatAttachment, ChatMessage, ChatSend, ChatSession, LogEntry, MediaDetail, MediaGcResult, MediaItem, MediaSearch, MediaSnapshot, MemoryDetail, MemoryHistoryEntry, MemoryKey, MemorySummary, Plugin, Resource, ResourceOperationResult, SkillDetail, SkillSummary, Task, ThreadDetail, ThreadListQuery, ThreadMessageKey, ThreadSnapshot } from '@/types/domain'

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
    readonly threadMessages: (keys: readonly ThreadMessageKey[]) => BackendEffect<readonly AuditRecord[]>
    readonly subscribe: (refresh: () => Promise<void>, handler: (record: AuditRecord) => void) => BackendEffect<() => void>
  }
  readonly threads: {
    readonly list: (query: ThreadListQuery) => BackendEffect<ThreadSnapshot>
    readonly get: (id: number) => BackendEffect<ThreadDetail | null>
    readonly active: () => BackendEffect<readonly ActiveThread[]>
    readonly halt: (taskId: number) => BackendEffect<boolean>
  }
  readonly memory: {
    readonly list: () => BackendEffect<readonly MemorySummary[]>
    readonly get: (key: MemoryKey) => BackendEffect<MemoryDetail | null>
    readonly history: (key: MemoryKey) => BackendEffect<readonly MemoryHistoryEntry[]>
    readonly getRevision: (key: MemoryKey, revision: string) => BackendEffect<MemoryDetail | null>
    readonly revert: (key: MemoryKey, revision: string) => BackendEffect<MemoryDetail | null>
  }
  readonly skills: {
    readonly list: () => BackendEffect<readonly SkillSummary[]>
    readonly get: (name: string) => BackendEffect<SkillDetail | null>
    readonly remove: (name: string) => BackendEffect<boolean>
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
export const getAuditThreadMessages = (keys: readonly ThreadMessageKey[]): Effect.Effect<readonly AuditRecord[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.audit.threadMessages(keys))
export const subscribeAudit = (refresh: () => Promise<void>, handler: (record: AuditRecord) => void): Effect.Effect<() => void, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.audit.subscribe(refresh, handler))
export const listThreads = (query: ThreadListQuery): Effect.Effect<ThreadSnapshot, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.threads.list(query))
export const getThread = (id: number): Effect.Effect<ThreadDetail | null, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.threads.get(id))
export const listActiveThreads: Effect.Effect<readonly ActiveThread[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.threads.active())
export const haltActiveThread = (taskId: number): Effect.Effect<boolean, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.threads.halt(taskId))
export const listMemories: Effect.Effect<readonly MemorySummary[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.memory.list())
export const getMemory = (key: MemoryKey): Effect.Effect<MemoryDetail | null, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.memory.get(key))
export const getMemoryHistory = (key: MemoryKey): Effect.Effect<readonly MemoryHistoryEntry[], BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.memory.history(key))
export const getMemoryRevision = (key: MemoryKey, revision: string): Effect.Effect<MemoryDetail | null, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.memory.getRevision(key, revision))
export const revertMemory = (key: MemoryKey, revision: string): Effect.Effect<MemoryDetail | null, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.memory.revert(key, revision))
export const listSkills: Effect.Effect<readonly SkillSummary[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.skills.list())
export const getSkill = (name: string): Effect.Effect<SkillDetail | null, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.skills.get(name))
export const removeSkill = (name: string): Effect.Effect<boolean, BackendError, AdminBackend> =>
  AdminBackendService.use((backend) => backend.skills.remove(name))
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
