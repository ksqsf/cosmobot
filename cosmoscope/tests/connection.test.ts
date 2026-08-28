import { beforeEach, describe, expect, it, vi } from 'vitest'
import { createPinia, setActivePinia } from 'pinia'
import { useConnectionStore } from '@/stores/connection'

class FakeSocket {
  static readonly instances: FakeSocket[] = []
  readyState = 0
  onopen: ((event: Event) => void) | null = null
  onmessage: ((event: MessageEvent) => void) | null = null
  onclose: ((event: CloseEvent) => void) | null = null
  onerror: ((event: Event) => void) | null = null
  readonly sent: string[] = []

  constructor() { FakeSocket.instances.push(this) }
  send(data: string): void { this.sent.push(data) }
  close(): void { this.readyState = 3 }
  open(): void { this.readyState = 1; this.onopen?.(new Event('open')) }
  succeed(index: number, result: unknown): void {
    const request = JSON.parse(this.sent[index] ?? '') as { id: number }
    this.onmessage?.(new MessageEvent('message', { data: JSON.stringify({ jsonrpc: '2.0', id: request.id, result }) }))
  }
}

async function authenticate(socket: FakeSocket, connection: Promise<unknown>): Promise<void> {
  socket.open()
  socket.succeed(0, { authenticated: true })
  await Promise.resolve()
  socket.succeed(1, { serverVersion: '1', methods: [], topics: [], permissions: [], features: {} })
  await connection
}

describe('saved RPC connection', () => {
  beforeEach(() => {
    localStorage.clear()
    FakeSocket.instances.length = 0
    vi.stubGlobal('WebSocket', FakeSocket)
    setActivePinia(createPinia())
  })

  it('restores after reload and explicit disconnect forgets the token', async () => {
    const first = useConnectionStore()
    const connected = first.connect('ws://127.0.0.1:38765/rpc', 'secret')
    const firstSocket = FakeSocket.instances[0]
    if (firstSocket === undefined) throw new Error('first socket missing')
    await authenticate(firstSocket, connected)
    expect(localStorage.length).toBe(1)
    first.dispose()

    setActivePinia(createPinia())
    const restored = useConnectionStore()
    const restoring = restored.restore()
    const secondSocket = FakeSocket.instances[1]
    if (secondSocket === undefined) throw new Error('second socket missing')
    await authenticate(secondSocket, restoring)
    await expect(restoring).resolves.toBe(true)
    expect(restored.state).toBe('authenticated')

    restored.disconnect()
    expect(localStorage.length).toBe(0)
  })
})
