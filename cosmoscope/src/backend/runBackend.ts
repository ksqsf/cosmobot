import { Effect, Layer, ManagedRuntime } from 'effect'
import type { AdminBackend } from './AdminBackend'
import { AdminBackendService } from './AdminBackend'
import { mockBackend } from './mockBackend'

export type BackendResult<A, E> =
  | { readonly _tag: 'Success'; readonly value: A }
  | { readonly _tag: 'Failure'; readonly error: E }

let activeBackend = mockBackend
const backendProxy: AdminBackend = {
  tasks: {
    list: () => activeBackend.tasks.list(),
    lookup: (id) => activeBackend.tasks.lookup(id),
    cancel: (id) => activeBackend.tasks.cancel(id),
    associated: (id) => activeBackend.tasks.associated(id),
    destroyAssociated: (id) => activeBackend.tasks.destroyAssociated(id),
  },
  audit: {
    recent: (limit) => activeBackend.audit.recent(limit),
    count: () => activeBackend.audit.count(),
    search: (query, limit) => activeBackend.audit.search(query, limit),
    get: (id) => activeBackend.audit.get(id),
    run: (runId) => activeBackend.audit.run(runId),
    thread: (threadId) => activeBackend.audit.thread(threadId),
    subscribe: (refresh, handler) => activeBackend.audit.subscribe(refresh, handler),
  },
  threads: {
    list: (query) => activeBackend.threads.list(query),
    get: (id) => activeBackend.threads.get(id),
    resolveRun: (runId) => activeBackend.threads.resolveRun(runId),
    active: () => activeBackend.threads.active(),
    halt: (taskId) => activeBackend.threads.halt(taskId),
  },
  memory: {
    list: () => activeBackend.memory.list(),
    get: (key) => activeBackend.memory.get(key),
    history: (key) => activeBackend.memory.history(key),
    getRevision: (key, revision) => activeBackend.memory.getRevision(key, revision),
    revert: (key, revision) => activeBackend.memory.revert(key, revision),
  },
  skills: {
    list: () => activeBackend.skills.list(),
    get: (name) => activeBackend.skills.get(name),
    remove: (name) => activeBackend.skills.remove(name),
  },
  chat: {
    sessionCount: () => activeBackend.chat.sessionCount(),
    list: () => activeBackend.chat.list(),
    open: (label) => activeBackend.chat.open(label),
    history: (sessionId, beforeMessageId, limit) => activeBackend.chat.history(sessionId, beforeMessageId, limit),
    fork: (sessionId, messageId, label) => activeBackend.chat.fork(sessionId, messageId, label),
    rename: (sessionId, label) => activeBackend.chat.rename(sessionId, label),
    delete: (sessionId) => activeBackend.chat.delete(sessionId),
    upload: (file) => activeBackend.chat.upload(file),
    discardAttachment: (attachmentId) => activeBackend.chat.discardAttachment(attachmentId),
    send: (message) => activeBackend.chat.send(message),
    subscribe: (sessionId, refresh, handler, done) => activeBackend.chat.subscribe(sessionId, refresh, handler, done),
  },
  resources: {
    count: () => activeBackend.resources.count(),
    list: () => activeBackend.resources.list(),
    detail: (id) => activeBackend.resources.detail(id),
    destroy: (id) => activeBackend.resources.destroy(id),
    rename: (id, newId) => activeBackend.resources.rename(id, newId),
    keepAlive: (id) => activeBackend.resources.keepAlive(id),
    makePermanent: (id) => activeBackend.resources.makePermanent(id),
  },
  schedules: { list: () => activeBackend.schedules.list(), delete: (id) => activeBackend.schedules.delete(id) },
  media: {
    list: (limit) => activeBackend.media.list(limit),
    search: (search) => activeBackend.media.search(search),
    get: (id) => activeBackend.media.get(id),
    delete: (id) => activeBackend.media.delete(id),
    gc: (maxAgeSeconds) => activeBackend.media.gc(maxAgeSeconds),
  },
  plugins: {
    list: () => activeBackend.plugins.list(),
    load: (id) => activeBackend.plugins.load(id),
    reload: (id) => activeBackend.plugins.reload(id),
    unload: (id) => activeBackend.plugins.unload(id),
  },
  chatLogs: {
    list: () => activeBackend.chatLogs.list(),
    window: (query) => activeBackend.chatLogs.window(query),
  },
  config: {
    get: () => activeBackend.config.get(),
    validate: (revision, changes) => activeBackend.config.validate(revision, changes),
    update: (revision, changes) => activeBackend.config.update(revision, changes),
    rollback: (revision, backupRevision) => activeBackend.config.rollback(revision, backupRevision),
    restart: () => activeBackend.config.restart(),
  },
}
const backendRuntime = ManagedRuntime.make(Layer.succeed(AdminBackendService, backendProxy))

export function setAdminBackend(backend: AdminBackend): void {
  activeBackend = backend
}

export function runBackend<A, E>(program: Effect.Effect<A, E, AdminBackend>): Promise<BackendResult<A, E>> {
  return backendRuntime.runPromise(Effect.match(program, {
    onFailure: (error): BackendResult<A, E> => ({ _tag: 'Failure', error }),
    onSuccess: (value): BackendResult<A, E> => ({ _tag: 'Success', value }),
  }))
}

export function disposeBackendRuntime(): Promise<void> {
  return backendRuntime.dispose()
}
