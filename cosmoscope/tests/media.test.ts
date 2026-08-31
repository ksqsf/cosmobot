import { Effect } from 'effect'
import { flushPromises, shallowMount } from '@vue/test-utils'
import { describe, expect, it, vi } from 'vitest'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { RpcClient } from '@/rpc/client'
import { liveAdminMethods } from '@/rpc/protocol'

const pageMocks = vi.hoisted(() => ({ connection: { state: 'authenticated', methods: new Set<string>(), error: '' }, runBackend: vi.fn() }))
vi.mock('@/backend/runBackend', () => ({ runBackend: pageMocks.runBackend }))
vi.mock('@/stores/connection', () => ({ useConnectionStore: () => pageMocks.connection }))
vi.mock('vue-router', () => ({
  useRoute: () => ({ params: {} }),
  useRouter: () => ({ replace: vi.fn(() => Promise.resolve()) }),
}))
vi.mock('primevue/usetoast', () => ({ useToast: () => ({ add: vi.fn() }) }))
vi.mock('@/overlay', () => ({
  useLayeredConfirm: () => ({ require: vi.fn() }),
  useOverlayLayer: () => ({ isTop: { value: true } }),
}))

const item = {
  mediaId: 'media:file-1', fileId: 'file-1', digest: 'abc', mimeType: 'image/png', sourceName: 'image.png',
  size: 1024, createdAtUnix: 10, lastUsedAtUnix: 20, exists: true,
  sourceRefs: ['telegram:file-1'], platformRefs: [{ platform: 'telegram', scope: 'bot', platformRef: 'photo-1' }], platforms: ['telegram'], sourceKinds: ['chat', 'tool-result'],
}

describe('media backend', () => {
  it('reports malformed RPC payloads without exposing the full Zod issue dump', async () => {
    const client = new RpcClient()
    vi.spyOn(client, 'request').mockResolvedValue({})
    const backend = makeRpcBackend(client, new Set(liveAdminMethods))

    await expect(Effect.runPromise(backend.media.list())).rejects.toMatchObject({
      message: 'Invalid RPC payload at stats (invalid_type).',
    })
  })

  it('maps snapshots and distinguishes configured from force GC', async () => {
    const client = new RpcClient()
    const request = vi.spyOn(client, 'request').mockImplementation((method) => {
      if (method === 'media.stats') return Promise.resolve({
        stats: { files: 1, existingFiles: 1, missingFiles: 0, totalBytes: 1024, sources: 1, platformRefs: 1, platformAssociations: 1, mimeTypes: ['image/png'], platforms: ['telegram'] },
        files: [item], gcSettings: { enabled: true, maxAgeSeconds: 604800, intervalHours: 24 },
      })
      if (method === 'media.get') return Promise.resolve({
        mediaId: item.mediaId, fileId: item.fileId, file: { ...item, ref: item.mediaId },
        sourceRefs: item.sourceRefs, platformRefs: item.platformRefs, platforms: item.platforms, sourceKinds: item.sourceKinds, publicUrl: 'https://media.example/image.png',
      })
      if (method === 'media.search') return Promise.resolve({ files: [item] })
      if (method === 'media.delete') return Promise.resolve({ mediaId: item.mediaId, fileId: item.fileId, deleted: true })
      return Promise.resolve({ deleted: 1, retainedReferencedFiles: 2, maxAgeSeconds: 0 })
    })
    const backend = makeRpcBackend(client, new Set(liveAdminMethods))

    await expect(Effect.runPromise(backend.media.list(50))).resolves.toMatchObject({ files: [item], stats: { totalBytes: 1024 } })
    await expect(Effect.runPromise(backend.media.get(item.mediaId))).resolves.toMatchObject({ ...item, publicUrl: 'https://media.example/image.png' })
    await expect(Effect.runPromise(backend.media.search({ query: 'image', platforms: ['telegram'], withoutPlatform: false, mimeTypes: ['image/png'], sourceKinds: ['chat'] }))).resolves.toEqual([item])
    await expect(Effect.runPromise(backend.media.delete(item.mediaId))).resolves.toBe(true)
    await Effect.runPromise(backend.media.gc())
    await Effect.runPromise(backend.media.gc(0))

    expect(request.mock.calls).toEqual([
      ['media.stats', { limit: 50 }],
      ['media.get', { mediaId: item.mediaId }],
      ['media.search', { query: 'image', platforms: ['telegram'], withoutPlatform: false, mimeTypes: ['image/png'], sourceKinds: ['chat'] }],
      ['media.delete', { mediaId: item.mediaId }],
      ['media.gc', {}],
      ['media.gc', { maxAgeSeconds: 0 }],
    ])
  })

  it('keeps the newest media snapshot when list requests finish out of order', async () => {
    const pending: ((result: unknown) => void)[] = []
    pageMocks.connection.methods = new Set(liveAdminMethods)
    pageMocks.runBackend.mockImplementation(() => new Promise((resolve) => pending.push(resolve)))
    const { default: MediaPage } = await import('@/pages/MediaPage.vue')
    const wrapper = shallowMount(MediaPage)
    await flushPromises()
    void (wrapper.vm as unknown as { refresh: () => Promise<void> }).refresh()
    expect(pending).toHaveLength(2)
    const snapshot = (name: string): unknown => ({
      _tag: 'Success',
      value: { files: [{ ...item, sourceName: name }], stats: { files: 1, existingFiles: 1, missingFiles: 0, totalBytes: 1024, sources: 1, platformRefs: 1, platformAssociations: 1, mimeTypes: ['image/png'], platforms: ['telegram'] }, gcSettings: { enabled: true, maxAgeSeconds: 604800, intervalHours: 24 } },
    })
    pending[1]?.(snapshot('new.png'))
    await flushPromises()
    pending[0]?.(snapshot('old.png'))
    await flushPromises()
    expect((wrapper.vm as unknown as { media: unknown }).media).toMatchObject([{ sourceName: 'new.png' }])
    wrapper.unmount()
  })
})
