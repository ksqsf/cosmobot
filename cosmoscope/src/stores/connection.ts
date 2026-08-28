import { computed, ref } from 'vue'
import { defineStore } from 'pinia'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { setAdminBackend } from '@/backend/runBackend'
import { mockBackend } from '@/backend/mockBackend'
import { RpcClient, type ConnectionState } from '@/rpc/client'

export const useConnectionStore = defineStore('connection', () => {
  const client = new RpcClient()
  const state = ref<ConnectionState>('offline')
  const endpoint = ref('')
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
    const capabilities = await client.connect(url, credential)
    endpoint.value = url
    serverVersion.value = capabilities.serverVersion
    methods.value = new Set(capabilities.methods)
    setAdminBackend(makeRpcBackend(client, methods.value))
  }

  function disconnect(): void {
    client.disconnect()
    endpoint.value = ''
    serverVersion.value = ''
    methods.value = new Set()
    setAdminBackend(mockBackend)
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

  return { state, endpoint, error, serverVersion, methods, stale, connect, disconnect, retry }
})
