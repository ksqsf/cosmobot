import { Context, Data, type Effect } from 'effect'
import type { AuditRecord, FixtureScenario, LogEntry, OverviewSnapshot, Plugin, Task } from '@/types/domain'

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
  readonly chat: { readonly sessionCount: () => BackendEffect<number> }
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
export const countResources: Effect.Effect<number, BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.resources.count())
export const listPlugins: Effect.Effect<readonly Plugin[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.plugins.list())
export const listLogs: Effect.Effect<readonly LogEntry[], BackendError, AdminBackend> =
  AdminBackendService.use((backend) => backend.logs.list())
