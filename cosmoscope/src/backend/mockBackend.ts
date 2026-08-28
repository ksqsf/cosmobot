import { Effect, Layer } from 'effect'
import { logs, plugins } from '@/fixtures/data'
import { AdminBackendService, OfflineError, type AdminBackend, type BackendEffect } from './AdminBackend'

const copy = <T>(value: T): T => structuredClone(value)

const unavailable = <A>(): BackendEffect<A> => Effect.fail(new OfflineError({ message: 'Connect to cosmobot to load this data.' }))

const mockBackend: AdminBackend = {
  tasks: {
    list: unavailable, lookup: unavailable, cancel: unavailable, await: unavailable,
    associated: unavailable, destroyAssociated: unavailable,
  },
  audit: {
    recent: unavailable, get: unavailable, thread: unavailable, subscribe: unavailable,
  },
  chat: {
    sessionCount: unavailable, list: unavailable, open: unavailable, history: unavailable,
    fork: unavailable, rename: unavailable, delete: unavailable, upload: unavailable,
    discardAttachment: unavailable, send: unavailable, subscribe: unavailable,
  },
  resources: {
    count: unavailable, list: unavailable, detail: unavailable, destroy: unavailable,
    rename: unavailable, keepAlive: unavailable, makePermanent: unavailable,
  },
  plugins: { list: () => Effect.succeed(copy(plugins)) },
  logs: { list: () => Effect.succeed(copy(logs)) },
}

export const mockBackendLayer = Layer.succeed(AdminBackendService, mockBackend)
export { mockBackend }
