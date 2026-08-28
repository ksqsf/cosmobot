import { Effect } from 'effect'
import { describe, expect, it, vi } from 'vitest'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { RpcClient } from '@/rpc/client'
import { liveAdminMethods } from '@/rpc/protocol'

const item = {
  mediaId: 'media:file-1', fileId: 'file-1', digest: 'abc', mimeType: 'image/png', sourceName: 'image.png',
  size: 1024, createdAtUnix: 10, lastUsedAtUnix: 20, exists: true,
  sourceRefs: ['telegram:file-1'], platformRefs: [{ platform: 'telegram', scope: 'bot', platformRef: 'photo-1' }], platforms: ['telegram'], sourceKinds: ['chat', 'tool-result'],
}

describe('media backend', () => {
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
})
