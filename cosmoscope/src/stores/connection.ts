import { computed, ref } from 'vue'
import { defineStore } from 'pinia'
import { z } from 'zod'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { setAdminBackend } from '@/backend/runBackend'
import { mockBackend } from '@/backend/mockBackend'
import { RpcClient, type ConnectionState } from '@/rpc/client'

const connectionStorageKey = 'cosmoscope.rpc.connection'
const savedConnectionSchema = z.object({ endpoint: z.string().min(1), token: z.string().min(1) })
type SavedConnection = z.infer<typeof savedConnectionSchema>

function loadSavedConnection(): SavedConnection | undefined {
  try {
    const saved = localStorage.getItem(connectionStorageKey)
    if (saved === null) return undefined
    const parsed = savedConnectionSchema.safeParse(JSON.parse(saved))
    if (parsed.success) return parsed.data
    localStorage.removeItem(connectionStorageKey)
  } catch { /* discard malformed or unavailable browser storage */ }
  return undefined
}

function saveConnection(connection: SavedConnection | undefined): void {
  try {
    if (connection === undefined) localStorage.removeItem(connectionStorageKey)
    else localStorage.setItem(connectionStorageKey, JSON.stringify(connection))
  } catch { /* the live connection still works when browser storage is unavailable */ }
}

export const useConnectionStore = defineStore('connection', () => {
  const client = new RpcClient()
  const savedConnection = loadSavedConnection()
  const state = ref<ConnectionState>(savedConnection === undefined ? 'offline' : 'opening')
  const endpoint = ref(savedConnection?.endpoint ?? '')
  const error = ref('')
  const serverVersion = ref('')
  const methods = ref<ReadonlySet<string>>(new Set())
  const stale = computed(() => methods.value.size > 0 && state.value !== 'authenticated')

  client.onStateChange((nextState, message) => {
    state.value = nextState
    error.value = message ?? ''
  })

  async function connect(url: string, credential: string): Promise<void> {
    error.value = ''
    try {
      const capabilities = await client.connect(url, credential)
      endpoint.value = url
      serverVersion.value = capabilities.serverVersion
      methods.value = new Set(capabilities.methods)
      setAdminBackend(makeRpcBackend(client, methods.value))
      saveConnection({ endpoint: url, token: credential })
    } catch (cause) {
      saveConnection(undefined)
      throw cause
    }
  }

  function disconnect(): void {
    saveConnection(undefined)
    dispose()
  }

  function dispose(): void {
    client.disconnect()
    serverVersion.value = ''
    methods.value = new Set()
    setAdminBackend(mockBackend)
  }

  async function restore(): Promise<boolean> {
    if (savedConnection === undefined) return false
    try {
      await connect(savedConnection.endpoint, savedConnection.token)
      return true
    } catch {
      return false
    }
  }

  async function retry(): Promise<void> {
    try {
      const capabilities = await client.retry()
      serverVersion.value = capabilities.serverVersion
      methods.value = new Set(capabilities.methods)
      setAdminBackend(makeRpcBackend(client, methods.value))
    } catch (cause) {
      error.value = cause instanceof Error ? cause.message : 'Could not reconnect to cosmobot.'
    }
  }

  return { state, endpoint, error, serverVersion, methods, stale, connect, disconnect, dispose, restore, retry }
})
