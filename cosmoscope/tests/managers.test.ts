import { Effect } from 'effect'
import { describe, expect, it, vi } from 'vitest'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { RpcClient } from '@/rpc/client'
import { liveAdminMethods } from '@/rpc/protocol'

describe('manager RPC backends', () => {
  it('preserves resource ownership and schedule thread identity', async () => {
    const client = new RpcClient()
    vi.spyOn(client, 'request').mockImplementation((method) => {
      if (method === 'resource.list') return Promise.resolve({ resources: [{
        id: 'sandbox-1', type: 'Sandbox', platform: 'telegram', chatId: '100', ownerId: '200', sessionId: null,
        description: 'ready', probe: { ok: true, result: 'ready' }, remainingLifeMinutes: 5,
      }] })
      if (method === 'schedule.delete') return Promise.resolve({ id: 1, deleted: true })
      return Promise.resolve({ schedules: [{
        id: 1, remainingSeconds: 60, intervalSeconds: 60, prompt: 'continue', platform: 'telegram',
        chatId: '100', ownerId: '200', runId: 'run-1',
      }] })
    })
    const backend = makeRpcBackend(client, new Set(liveAdminMethods))

    await expect(Effect.runPromise(backend.resources.list())).resolves.toMatchObject([{ platform: 'telegram', chatId: '100', ownerId: '200' }])
    await expect(Effect.runPromise(backend.schedules.list())).resolves.toMatchObject([{ runId: 'run-1', ownerId: '200', intervalSeconds: 60 }])
    await expect(Effect.runPromise(backend.schedules.delete(1))).resolves.toBe(true)
  })
})
