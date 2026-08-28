import { capabilitiesSchema, rpcNotificationSchema, rpcResponseSchema, RpcCallError, RpcProtocolError, type Capabilities } from './protocol'

export type ConnectionState = 'offline' | 'opening' | 'authenticated' | 'reconnecting' | 'failed'

export interface WebSocketConnection {
  readonly readyState: number
  onopen: ((event: Event) => void) | null
  onmessage: ((event: MessageEvent<unknown>) => void) | null
  onclose: ((event: CloseEvent) => void) | null
  onerror: ((event: Event) => void) | null
  send(data: string): void
  close(code?: number, reason?: string): void
}

interface PendingRequest {
  readonly resolve: (value: unknown) => void
  readonly reject: (error: Error) => void
  readonly timeout: ReturnType<typeof setTimeout>
}

interface Subscription {
  readonly subscribeMethod: string
  readonly unsubscribeMethod: string
  readonly params: unknown
  readonly notificationMethod: string
  readonly refresh: () => Promise<void>
  readonly handler: (params: unknown) => void
  restoring: boolean
  readonly buffered: unknown[]
}

const notificationBufferLimit = 1_000

export interface RpcClientOptions {
  readonly createSocket?: (url: string) => WebSocketConnection
  readonly requestTimeoutMs?: number
  readonly reconnectBaseMs?: number
  readonly reconnectMaxMs?: number
  readonly random?: () => number
}

const browserSocket = (url: string): WebSocketConnection => new WebSocket(url)

export class RpcClient {
  private readonly createSocket: (url: string) => WebSocketConnection
  private readonly requestTimeoutMs: number
  private readonly reconnectBaseMs: number
  private readonly reconnectMaxMs: number
  private readonly random: () => number
  private readonly pending = new Map<number, PendingRequest>()
  private readonly subscriptions = new Map<string, Subscription>()
  private nextId = 1
  private socket: WebSocketConnection | undefined
  private endpoint = ''
  private credential = ''
  private reconnectAttempt = 0
  private reconnectTimer: ReturnType<typeof setTimeout> | undefined
  private intentionalClose = true
  private generation = 0
  private stateListener: (state: ConnectionState, error?: string) => void = () => undefined
  state: ConnectionState = 'offline'

  constructor(options: RpcClientOptions = {}) {
    this.createSocket = options.createSocket ?? browserSocket
    this.requestTimeoutMs = options.requestTimeoutMs ?? 15_000
    this.reconnectBaseMs = options.reconnectBaseMs ?? 500
    this.reconnectMaxMs = options.reconnectMaxMs ?? 15_000
    this.random = options.random ?? Math.random
  }

  onStateChange(listener: (state: ConnectionState, error?: string) => void): void {
    this.stateListener = listener
  }

  async connect(endpoint: string, credential: string): Promise<Capabilities> {
    this.disconnect()
    this.endpoint = validateRpcEndpoint(endpoint)
    this.credential = credential
    this.intentionalClose = false
    this.reconnectAttempt = 0
    return this.openSocket(true)
  }

  disconnect(): void {
    this.intentionalClose = true
    this.generation += 1
    if (this.reconnectTimer !== undefined) clearTimeout(this.reconnectTimer)
    this.reconnectTimer = undefined
    this.rejectPending(new RpcCallError('disconnected', 'RPC connection closed'))
    this.socket?.close(1000, 'client disconnect')
    this.socket = undefined
    this.credential = ''
    this.subscriptions.clear()
    this.setState('offline')
  }

  request(method: string, params: unknown = {}): Promise<unknown> {
    if (this.state !== 'authenticated') return Promise.reject(new RpcCallError('offline', 'RPC is not connected'))
    return this.sendRequest(method, params)
  }

  retry(): Promise<Capabilities> {
    if (this.credential === '') return Promise.reject(new RpcCallError('offline', 'RPC credential is no longer available'))
    if (this.reconnectTimer !== undefined) clearTimeout(this.reconnectTimer)
    this.reconnectTimer = undefined
    this.generation += 1
    this.socket?.close(1000, 'retry')
    this.intentionalClose = false
    return this.openSocket(false)
  }

  subscribe(
    key: string,
    subscribeMethod: string,
    unsubscribeMethod: string,
    params: unknown,
    notificationMethod: string,
    refresh: () => Promise<void>,
    handler: (params: unknown) => void,
  ): () => void {
    const subscription: Subscription = { subscribeMethod, unsubscribeMethod, params, notificationMethod, refresh, handler, restoring: false, buffered: [] }
    this.subscriptions.set(key, subscription)
    if (this.state === 'authenticated') void this.restoreSubscription(key, subscription).catch(() => this.socket?.close(1011, 'subscription restore failed'))
    return () => {
      if (this.subscriptions.get(key) !== subscription) return
      this.subscriptions.delete(key)
      if (this.state === 'authenticated') void this.request(unsubscribeMethod, params).catch(() => undefined)
    }
  }

  private openSocket(initial: boolean): Promise<Capabilities> {
    const generation = ++this.generation
    this.setState(initial ? 'opening' : 'reconnecting')
    return new Promise<Capabilities>((resolve, reject) => {
      let socket: WebSocketConnection
      try {
        socket = this.createSocket(this.endpoint)
      } catch {
        const error = new RpcCallError('connection_failed', 'Could not open the RPC connection')
        if (initial) {
          this.credential = ''
          this.setState('failed', error.message)
        } else if (!this.intentionalClose) {
          this.scheduleReconnect()
        }
        reject(error)
        return
      }
      this.socket = socket
      this.nextId = 1
      let authenticated = false
      socket.onopen = () => {
        void this.authenticate().then((capabilities) => {
          if (generation !== this.generation) return
          authenticated = true
          this.reconnectAttempt = 0
          this.setState('authenticated')
          void this.restoreSubscriptions().catch(() => socket.close(1011, 'subscription restore failed'))
          resolve(capabilities)
        }).catch((error: unknown) => {
          if (generation !== this.generation) return
          this.intentionalClose = true
          this.credential = ''
          const safeError = error instanceof Error ? error : new Error('Authentication failed')
          this.setState('failed', safeError.message)
          socket.close(4001, 'authentication failed')
          reject(safeError)
        })
      }
      socket.onmessage = (event) => { this.handleMessage(event.data) }
      socket.onerror = () => {
        if (!authenticated && initial) {
          this.intentionalClose = true
          this.credential = ''
          this.setState('failed', 'Could not connect to RPC')
          reject(new RpcCallError('connection_failed', 'Could not connect to RPC'))
          socket.close(1001, 'connection failed')
        }
      }
      socket.onclose = () => {
        if (generation !== this.generation) return
        const error = new RpcCallError('disconnected', 'RPC connection closed')
        this.rejectPending(error)
        if (!authenticated) {
          reject(error)
          if (initial) {
            this.intentionalClose = true
            this.credential = ''
            this.setState('failed', 'Could not connect to RPC')
          }
        }
        if (!this.intentionalClose) this.scheduleReconnect()
      }
    })
  }

  private async authenticate(): Promise<Capabilities> {
    await this.sendRequest('admin.authenticate', { token: this.credential })
    return capabilitiesSchema.parse(await this.sendRequest('admin.capabilities', {}))
  }

  private sendRequest(method: string, params: unknown): Promise<unknown> {
    const socket = this.socket
    if (socket?.readyState !== 1) return Promise.reject(new RpcCallError('offline', 'RPC is not connected'))
    const id = this.nextId++
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        this.pending.delete(id)
        reject(new RpcCallError('timeout', 'RPC request timed out'))
      }, this.requestTimeoutMs)
      this.pending.set(id, { resolve, reject, timeout })
      socket.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
    })
  }

  private handleMessage(data: unknown): void {
    try {
      if (typeof data !== 'string') throw new RpcProtocolError('RPC sent a non-text message')
      const value: unknown = JSON.parse(data)
      const response = rpcResponseSchema.safeParse(value)
      if (response.success) {
        const id = response.data.id
        if (typeof id !== 'number') throw new RpcProtocolError('RPC response ID is not numeric')
        const pending = this.pending.get(id)
        if (pending === undefined) return
        clearTimeout(pending.timeout)
        this.pending.delete(id)
        if ('error' in response.data) {
          pending.reject(new RpcCallError(response.data.error.data?.code ?? String(response.data.error.code), response.data.error.message))
        } else {
          pending.resolve(response.data.result)
        }
        return
      }
      const notification = rpcNotificationSchema.parse(value)
      for (const subscription of this.subscriptions.values()) {
        if (subscription.notificationMethod !== notification.method) continue
        if (subscription.restoring && subscription.buffered.length < notificationBufferLimit) subscription.buffered.push(notification.params)
        else if (subscription.restoring) this.socket?.close(1011, 'notification buffer overflow')
        else subscription.handler(notification.params)
      }
    } catch {
      const error = new RpcProtocolError('RPC returned a malformed message')
      this.rejectPending(error)
      this.socket?.close(1002, 'protocol error')
    }
  }

  private rejectPending(error: Error): void {
    for (const pending of this.pending.values()) {
      clearTimeout(pending.timeout)
      pending.reject(error)
    }
    this.pending.clear()
  }

  private scheduleReconnect(): void {
    this.setState('reconnecting')
    const exponential = Math.min(this.reconnectMaxMs, this.reconnectBaseMs * 2 ** this.reconnectAttempt++)
    const delay = Math.round(exponential * (0.75 + this.random() * 0.5))
    this.reconnectTimer = setTimeout(() => { void this.openSocket(false).catch(() => undefined) }, delay)
  }

  private async restoreSubscriptions(): Promise<void> {
    for (const [key, subscription] of this.subscriptions) await this.restoreSubscription(key, subscription)
  }

  private async restoreSubscription(key: string, subscription: Subscription): Promise<void> {
    subscription.restoring = true
    subscription.buffered.length = 0
    try {
      await this.request(subscription.subscribeMethod, subscription.params)
      if (this.subscriptions.get(key) !== subscription) return
      await subscription.refresh()
      if (this.subscriptions.get(key) !== subscription) return
      for (const value of subscription.buffered.splice(0)) subscription.handler(value)
    } finally {
      subscription.restoring = false
    }
  }

  private setState(state: ConnectionState, error?: string): void {
    this.state = state
    this.stateListener(state, error)
  }
}

export function validateRpcEndpoint(value: string): string {
  const url = new URL(value)
  const loopback = url.hostname === 'localhost' || url.hostname === '127.0.0.1' || url.hostname === '[::1]'
  if (url.protocol !== 'wss:' && !(url.protocol === 'ws:' && loopback)) throw new Error('Use wss://, or ws:// for loopback development')
  if (url.username !== '' || url.password !== '' || url.search !== '' || url.hash !== '') throw new Error('RPC endpoint must not contain credentials, a query, or a fragment')
  return url.toString()
}
