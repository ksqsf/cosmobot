import { Effect } from 'effect'
import { RpcBackendError, type AdminBackend } from './AdminBackend'
import { mockBackend } from './mockBackend'
import { auditRecordSchema, chatSessionsSchema, concurrencyListSchema, recentAuditSchema, resourceListSchema } from '@/rpc/schemas'
import type { RpcClient } from '@/rpc/client'
import type { LiveAdminMethod } from '@/rpc/protocol'
import type { Task } from '@/types/domain'

export function makeRpcBackend(client: RpcClient, methods: ReadonlySet<string>): AdminBackend {
  const supports = (method: LiveAdminMethod): boolean => methods.has(method)
  return {
    system: mockBackend.system,
    tasks: {
      list: supports('concurrency.list') ? () => Effect.tryPromise({
        try: async () => concurrencyListSchema.parse(await client.request('concurrency.list')).entries.map(toTask),
        catch: () => new RpcBackendError({ message: 'Could not load the task snapshot.' }),
      }) : mockBackend.tasks.list,
    },
    audit: {
      recent: supports('audit.recent') ? () => Effect.tryPromise({
        try: async () => recentAuditSchema.parse(await client.request('audit.recent', { limit: 20 })),
        catch: () => new RpcBackendError({ message: 'Could not load recent audit activity.' }),
      }) : mockBackend.audit.recent,
      subscribe: supports('audit.subscribe') ? (refresh, handler) => Effect.sync(() => client.subscribe(
        'overview.audit', 'audit.subscribe', 'audit.unsubscribe', {}, 'audit.event', refresh,
        (params) => handler(auditRecordSchema.parse(params)),
      )) : mockBackend.audit.subscribe,
    },
    chat: {
      sessionCount: supports('chat.list_sessions') ? () => Effect.tryPromise({
        try: async () => chatSessionsSchema.parse(await client.request('chat.list_sessions')).sessions.length,
        catch: () => new RpcBackendError({ message: 'Could not load the session count.' }),
      }) : mockBackend.chat.sessionCount,
    },
    resources: {
      count: supports('resource.list') ? () => Effect.tryPromise({
        try: async () => resourceListSchema.parse(await client.request('resource.list')).resources.length,
        catch: () => new RpcBackendError({ message: 'Could not load the resource count.' }),
      }) : mockBackend.resources.count,
    },
    plugins: mockBackend.plugins,
    logs: mockBackend.logs,
  }
}

function toTask(entry: ReturnType<typeof concurrencyListSchema.parse>['entries'][number]): Task {
  const elapsedUntil = entry.finishedAt === null ? Date.now() : Date.parse(entry.finishedAt)
  const elapsedSeconds = Math.max(0, Math.round((elapsedUntil - Date.parse(entry.startedAt)) / 1_000))
  return {
    id: String(entry.id),
    label: entry.label,
    detail: entry.error ?? 'Managed cosmobot task (owner metadata unavailable)',
    owner: 'Unavailable',
    platform: 'runtime',
    status: entry.status === 'cancelled' ? 'stopped' : entry.status,
    started: new Date(entry.startedAt).toLocaleTimeString(),
    elapsed: `${String(elapsedSeconds)}s`,
  }
}
