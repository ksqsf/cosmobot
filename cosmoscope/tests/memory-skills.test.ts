import { Effect } from 'effect'
import { describe, expect, it, vi } from 'vitest'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { RpcClient } from '@/rpc/client'
import { liveAdminMethods } from '@/rpc/protocol'

const key = { platform: 'rpc', scope: 'sender', scopeId: 'user-1' } as const
const memory = { ...key, characters: 5, content: 'hello' }
const revision = '0123456789abcdef0123456789abcdef01234567'

describe('memory and skills RPC backends', () => {
  it('maps independent RPC namespaces, including historical content', async () => {
    const client = new RpcClient()
    const request = vi.spyOn(client, 'request').mockImplementation((method) => {
      if (method === 'memory.list') return Promise.resolve({ memories: [{ ...key, characters: 5 }] })
      if (method === 'memory.history') return Promise.resolve({ history: [{ revision, committedAt: '2026-01-01T00:00:00Z', subject: 'Update memory' }] })
      if (method === 'memory.revert') return Promise.resolve({ reverted: true, memory })
      if (method.startsWith('memory.')) return Promise.resolve(memory)
      if (method === 'skills.list') return Promise.resolve({ skills: [{ name: 'haskell', description: 'Haskell help' }] })
      if (method === 'skills.remove') return Promise.resolve({ name: 'haskell', removed: true })
      return Promise.resolve({ name: 'haskell', content: '# Haskell' })
    })
    const backend = makeRpcBackend(client, new Set(liveAdminMethods))

    await expect(Effect.runPromise(backend.memory.list())).resolves.toHaveLength(1)
    await Effect.runPromise(backend.memory.get(key))
    await Effect.runPromise(backend.memory.history(key))
    await Effect.runPromise(backend.memory.getRevision(key, revision))
    await Effect.runPromise(backend.memory.revert(key, revision))
    await expect(Effect.runPromise(backend.skills.list())).resolves.toHaveLength(1)
    await expect(Effect.runPromise(backend.skills.get('haskell'))).resolves.toMatchObject({ content: '# Haskell' })
    await expect(Effect.runPromise(backend.skills.remove('haskell'))).resolves.toBe(true)

    expect(request.mock.calls.map(([method]) => method)).toEqual([
      'memory.list', 'memory.get', 'memory.history', 'memory.get_revision', 'memory.revert', 'skills.list', 'skills.get', 'skills.remove',
    ])
  })
})
