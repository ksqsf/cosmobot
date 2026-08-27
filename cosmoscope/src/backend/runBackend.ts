import { Effect, ManagedRuntime } from 'effect'
import type { AdminBackend } from './AdminBackend'
import { mockBackendLayer } from './mockBackend'

export type BackendResult<A, E> =
  | { readonly _tag: 'Success'; readonly value: A }
  | { readonly _tag: 'Failure'; readonly error: E }

const backendRuntime = ManagedRuntime.make(mockBackendLayer)

export function runBackend<A, E>(program: Effect.Effect<A, E, AdminBackend>): Promise<BackendResult<A, E>> {
  return backendRuntime.runPromise(Effect.match(program, {
    onFailure: (error): BackendResult<A, E> => ({ _tag: 'Failure', error }),
    onSuccess: (value): BackendResult<A, E> => ({ _tag: 'Success', value }),
  }))
}

export function disposeBackendRuntime(): Promise<void> {
  return backendRuntime.dispose()
}
