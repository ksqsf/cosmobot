import { Effect, Layer, ManagedRuntime } from 'effect'
import type { AdminBackend } from './AdminBackend'
import { AdminBackendService } from './AdminBackend'
import { mockBackend } from './mockBackend'

export type BackendResult<A, E> =
  | { readonly _tag: 'Success'; readonly value: A }
  | { readonly _tag: 'Failure'; readonly error: E }

let activeBackend = mockBackend
const backendProxy: AdminBackend = {
  system: { overview: (scenario) => activeBackend.system.overview(scenario) },
  tasks: { list: () => activeBackend.tasks.list() },
  audit: {
    recent: () => activeBackend.audit.recent(),
    subscribe: (refresh, handler) => activeBackend.audit.subscribe(refresh, handler),
  },
  chat: {
    sessionCount: () => activeBackend.chat.sessionCount(),
    list: () => activeBackend.chat.list(),
    open: (label) => activeBackend.chat.open(label),
    history: (sessionId) => activeBackend.chat.history(sessionId),
    fork: (sessionId, messageId, label) => activeBackend.chat.fork(sessionId, messageId, label),
    rename: (sessionId, label) => activeBackend.chat.rename(sessionId, label),
    delete: (sessionId) => activeBackend.chat.delete(sessionId),
    upload: (file) => activeBackend.chat.upload(file),
    discardAttachment: (attachmentId) => activeBackend.chat.discardAttachment(attachmentId),
    send: (message) => activeBackend.chat.send(message),
    subscribe: (sessionId, refresh, handler, done) => activeBackend.chat.subscribe(sessionId, refresh, handler, done),
  },
  resources: { count: () => activeBackend.resources.count() },
  plugins: { list: () => activeBackend.plugins.list() },
  logs: { list: () => activeBackend.logs.list() },
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
