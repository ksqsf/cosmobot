import { Effect, Layer, ManagedRuntime } from 'effect'
import type { AdminBackend } from './AdminBackend'
import { AdminBackendService } from './AdminBackend'
import { mockBackend } from './mockBackend'

export type BackendResult<A, E> =
  | { readonly _tag: 'Success'; readonly value: A }
  | { readonly _tag: 'Failure'; readonly error: E }

let activeBackend = mockBackend
const backendProxy: AdminBackend = {
  get tasks() { return activeBackend.tasks },
  get audit() { return activeBackend.audit },
  get threads() { return activeBackend.threads },
  get memory() { return activeBackend.memory },
  get skills() { return activeBackend.skills },
  get chat() { return activeBackend.chat },
  get resources() { return activeBackend.resources },
  get schedules() { return activeBackend.schedules },
  get media() { return activeBackend.media },
  get plugins() { return activeBackend.plugins },
  get chatLogs() { return activeBackend.chatLogs },
  get config() { return activeBackend.config },
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
