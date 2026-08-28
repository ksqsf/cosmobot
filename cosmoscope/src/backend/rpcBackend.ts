import { Effect } from 'effect'
import { RpcBackendError, type AdminBackend } from './AdminBackend'
import { mockBackend } from './mockBackend'
import { concurrencyListSchema } from '@/rpc/schemas'
import type { RpcClient } from '@/rpc/client'
import type { Task } from '@/types/domain'

export function makeRpcBackend(client: RpcClient): AdminBackend {
  return {
    source: { ...mockBackend.source, tasks: 'rpc' },
    system: mockBackend.system,
    tasks: {
      list: () => Effect.tryPromise({
        try: async () => concurrencyListSchema.parse(await client.request('concurrency.list')).entries.map(toTask),
        catch: () => new RpcBackendError({ message: 'Could not load the task snapshot.' }),
      }),
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
