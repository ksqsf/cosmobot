import { Effect } from 'effect'
import { describe, expect, it, vi } from 'vitest'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { RpcClient } from '@/rpc/client'

describe('shared audit subscription', () => {
  it('fans out events and reconnect refreshes, unsubscribing only after the last observer', async () => {
    const client = new RpcClient()
    const stop = vi.fn()
    const subscribe = vi.spyOn(client, 'subscribe').mockReturnValue(stop)
    const backend = makeRpcBackend(client, new Set(['audit.subscribe']))
    const parent = { refresh: vi.fn(() => Promise.resolve()), handler: vi.fn() }
    const child = { refresh: vi.fn(() => Promise.resolve()), handler: vi.fn() }
    const releaseParent = await Effect.runPromise(backend.audit.subscribe(parent.refresh, parent.handler))
    const refreshedBackend = makeRpcBackend(client, new Set(['audit.subscribe']))
    const releaseChild = await Effect.runPromise(refreshedBackend.audit.subscribe(child.refresh, child.handler))
    expect(subscribe).toHaveBeenCalledOnce()
    expect(child.refresh).toHaveBeenCalledOnce()
    const transport = subscribe.mock.calls[0]
    if (!transport) throw new Error('Missing subscription')
    await transport[5]()
    expect(parent.refresh).toHaveBeenCalledOnce()
    expect(child.refresh).toHaveBeenCalledTimes(2)
    const event = { id: 1, occurredAt: '2026-01-01T00:00:00Z', event: { tag: 'AgentRunFinished', runId: 'child', status: 'success', finalLength: 0, turnsUsed: 1 } }
    transport[6]('audit.event', event)
    expect(parent.handler).toHaveBeenCalledWith(event)
    expect(child.handler).toHaveBeenCalledWith(event)
    releaseChild()
    expect(stop).not.toHaveBeenCalled()
    transport[6]('audit.event', event)
    expect(parent.handler).toHaveBeenCalledTimes(2)
    expect(child.handler).toHaveBeenCalledOnce()
    releaseParent()
    expect(stop).toHaveBeenCalledOnce()
  })
})
