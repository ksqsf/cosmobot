import { Effect } from 'effect'
import { describe, expect, it, vi } from 'vitest'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { RpcClient } from '@/rpc/client'
import { liveAdminMethods } from '@/rpc/protocol'

const status = {
  pluginId: 'echo', version: '1.2.3', generation: 4,
  required: false, sandboxed: true, routeCount: 2, toolCount: 3,
}

describe('plugin backend', () => {
  it('maps the lifecycle RPC contract without fixture fields', async () => {
    const client = new RpcClient()
    const request = vi.spyOn(client, 'request').mockImplementation((method) => {
      if (method === 'plugin.list') return Promise.resolve({ plugins: [status] })
      if (method === 'plugin.unload') return Promise.resolve({ pluginId: 'echo', unloaded: true })
      return Promise.resolve(status)
    })
    const backend = makeRpcBackend(client, new Set(liveAdminMethods))

    await expect(Effect.runPromise(backend.plugins.list())).resolves.toEqual([{
      id: 'echo', version: '1.2.3', generation: 4,
      required: false, sandboxed: true, routes: 2, tools: 3,
    }])
    await Effect.runPromise(backend.plugins.load('echo'))
    await Effect.runPromise(backend.plugins.reload('echo'))
    await Effect.runPromise(backend.plugins.unload('echo'))

    expect(request.mock.calls.map(([method]) => method)).toEqual([
      'plugin.list', 'plugin.load', 'plugin.reload', 'plugin.unload',
    ])
  })
})
