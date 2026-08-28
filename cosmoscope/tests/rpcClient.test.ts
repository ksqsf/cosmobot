import { describe, expect, it, vi } from 'vitest'
import { RpcClient, validateRpcEndpoint, type WebSocketConnection } from '@/rpc/client'

class FakeSocket {
  readyState = 0
  onopen: WebSocketConnection['onopen'] = null
  onmessage: WebSocketConnection['onmessage'] = null
  onclose: WebSocketConnection['onclose'] = null
  onerror: WebSocketConnection['onerror'] = null
  readonly sent: string[] = []

  send(data: string): void { this.sent.push(data) }
  close(): void { this.readyState = 3 }
  open(): void { this.readyState = 1; this.onopen?.(new Event('open')) }
  receive(value: unknown): void { this.onmessage?.(new MessageEvent('message', { data: typeof value === 'string' ? value : JSON.stringify(value) })) }
  remoteClose(): void { this.readyState = 3; this.onclose?.(new CloseEvent('close')) }
  fail(): void { this.onerror?.(new Event('error')) }
  request(index: number): { readonly id: number; readonly method: string } { return JSON.parse(this.sent[index] ?? '') as { id: number; method: string } }
  succeed(index: number, result: unknown): void { this.receive({ jsonrpc: '2.0', id: this.request(index).id, result }) }
}

const capabilities = {
  serverVersion: '0.1.0.0', methods: ['concurrency.list'], topics: ['audit.event'], permissions: [], features: { serviceLogs: 'demo' },
}

async function authenticate(socket: FakeSocket, connection: Promise<unknown>): Promise<void> {
  socket.open()
  socket.succeed(0, { authenticated: true })
  await Promise.resolve()
  socket.succeed(1, capabilities)
  await connection
}

describe('RPC client', () => {
  it('rejects malformed responses and clears the pending request', async () => {
    const socket = new FakeSocket()
    const client = new RpcClient({ createSocket: () => socket })
    const connection = client.connect('ws://127.0.0.1:38765/rpc', 'secret')
    await authenticate(socket, connection)
    const request = client.request('concurrency.list')
    socket.receive('{broken')
    await expect(request).rejects.toMatchObject({ name: 'RpcProtocolError' })
  })

  it('times out and rejects requests when the connection closes', async () => {
    vi.useFakeTimers()
    const sockets: FakeSocket[] = []
    const client = new RpcClient({ createSocket: () => { const socket = new FakeSocket(); sockets.push(socket); return socket }, requestTimeoutMs: 10 })
    const connection = client.connect('ws://127.0.0.1:38765/rpc', 'secret')
    const socket = sockets[0]
    expect(socket).toBeDefined()
    if (socket === undefined) throw new Error('socket missing')
    await authenticate(socket, connection)
    const timedOut = expect(client.request('concurrency.list')).rejects.toMatchObject({ code: 'timeout' })
    await vi.advanceTimersByTimeAsync(11)
    await timedOut
    const disconnected = client.request('concurrency.list')
    socket.remoteClose()
    await expect(disconnected).rejects.toMatchObject({ code: 'disconnected' })
    client.disconnect()
    vi.useRealTimers()
  })

  it('restores subscriptions and refreshes before releasing notifications', async () => {
    vi.useFakeTimers()
    const sockets: FakeSocket[] = []
    const client = new RpcClient({ createSocket: () => { const socket = new FakeSocket(); sockets.push(socket); return socket }, reconnectBaseMs: 1, random: () => 0 })
    const firstConnection = client.connect('ws://127.0.0.1:38765/rpc', 'secret')
    const first = sockets[0]
    expect(first).toBeDefined()
    if (first === undefined) throw new Error('socket missing')
    await authenticate(first, firstConnection)

    const order: string[] = []
    let finishRefresh = (): void => undefined
    const refresh = (): Promise<void> => new Promise((resolve) => { finishRefresh = () => { order.push('snapshot'); resolve() } })
    client.subscribe('audit', 'audit.subscribe', 'audit.unsubscribe', {}, ['audit.event'], refresh, () => order.push('event'))
    await Promise.resolve()
    first.succeed(2, { subscribed: true })
    await Promise.resolve()
    first.receive({ jsonrpc: '2.0', method: 'audit.event', params: { id: 1 } })
    finishRefresh()
    await Promise.resolve()
    expect(order).toEqual(['snapshot', 'event'])

    first.remoteClose()
    await vi.advanceTimersByTimeAsync(1)
    const second = sockets[1]
    expect(second).toBeDefined()
    if (second === undefined) throw new Error('reconnect socket missing')
    second.open()
    second.succeed(0, { authenticated: true })
    await Promise.resolve()
    second.succeed(1, capabilities)
    while (second.sent.length < 3) await Promise.resolve()
    second.succeed(2, { subscribed: true })
    await Promise.resolve()
    second.receive({ jsonrpc: '2.0', method: 'audit.event', params: { id: 2 } })
    finishRefresh()
    await Promise.resolve()
    expect(order.slice(-2)).toEqual(['snapshot', 'event'])
    client.disconnect()
    vi.useRealTimers()
  })

  it('buffers every selected streaming notification method behind the refreshed history', async () => {
    const socket = new FakeSocket()
    const client = new RpcClient({ createSocket: () => socket })
    const connection = client.connect('ws://127.0.0.1:38765/rpc', 'secret')
    await authenticate(socket, connection)
    const order: string[] = []
    let finishRefresh = (): void => undefined
    client.subscribe(
      'chat:session-1', 'chat.subscribe', 'chat.unsubscribe', { sessionId: 'session-1' },
      ['chat.message', 'chat.message_update', 'chat.message_done'],
      () => new Promise((resolve) => { finishRefresh = () => { order.push('history'); resolve() } }),
      (method) => order.push(method),
    )
    await Promise.resolve()
    socket.succeed(2, { subscribed: true })
    await Promise.resolve()
    socket.receive({ jsonrpc: '2.0', method: 'chat.message', params: { messageId: 'message-1' } })
    socket.receive({ jsonrpc: '2.0', method: 'chat.message_update', params: { messageId: 'message-1', text: 'draft' } })
    finishRefresh()
    await new Promise((resolve) => setTimeout(resolve, 0))
    expect(order).toEqual(['history', 'chat.message', 'chat.message_update'])
    client.disconnect()
  })
})

describe('RPC endpoint validation', () => {
  it('requires TLS except on loopback and rejects credential-bearing URLs', () => {
    expect(validateRpcEndpoint('ws://127.0.0.1:38765/rpc')).toBe('ws://127.0.0.1:38765/rpc')
    expect(() => validateRpcEndpoint('ws://example.test/rpc')).toThrow(/wss/)
    expect(() => validateRpcEndpoint('wss://example.test/rpc?token=secret')).toThrow(/query/)
  })

  it('reports a synchronous WebSocket construction failure', async () => {
    const client = new RpcClient({ createSocket: () => { throw new Error('blocked') } })
    await expect(client.connect('ws://127.0.0.1:38765/rpc', 'secret')).rejects.toMatchObject({ code: 'connection_failed' })
    expect(client.state).toBe('failed')
  })

  it('fails an initial connection error without retaining retry credentials', async () => {
    const socket = new FakeSocket()
    const client = new RpcClient({ createSocket: () => socket })
    const connection = client.connect('ws://127.0.0.1:38765/rpc', 'secret')
    socket.fail()
    await expect(connection).rejects.toMatchObject({ code: 'connection_failed' })
    await expect(client.retry()).rejects.toMatchObject({ code: 'offline' })
  })

  it('does not let stale cleanup remove a replacement subscription', async () => {
    const socket = new FakeSocket()
    const client = new RpcClient({ createSocket: () => socket })
    const connection = client.connect('ws://127.0.0.1:38765/rpc', 'secret')
    await authenticate(socket, connection)
    const firstCleanup = client.subscribe('audit', 'audit.subscribe', 'audit.unsubscribe', {}, ['audit.event'], () => Promise.resolve(), () => undefined)
    const received: unknown[] = []
    client.subscribe('audit', 'audit.subscribe', 'audit.unsubscribe', {}, ['audit.event'], () => Promise.resolve(), (_method, event) => received.push(event))
    firstCleanup()
    await Promise.resolve()
    socket.succeed(2, { subscribed: true })
    socket.succeed(3, { subscribed: true })
    await new Promise((resolve) => setTimeout(resolve, 0))
    socket.receive({ jsonrpc: '2.0', method: 'audit.event', params: { id: 1 } })
    expect(received).toEqual([{ id: 1 }])
    client.disconnect()
  })
})
