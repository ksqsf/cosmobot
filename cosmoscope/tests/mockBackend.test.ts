import { describe, expect, it } from 'vitest'
import { Effect } from 'effect'
import { listPlugins, listTasks } from '@/backend/AdminBackend'
import { mockBackendLayer } from '@/backend/mockBackend'

describe('demo backend', () => {
  it('keeps fixtures only for pages that are not implemented', async () => {
    const first = await Effect.runPromise(Effect.provide(listPlugins, mockBackendLayer))
    const second = await Effect.runPromise(Effect.provide(listPlugins, mockBackendLayer))
    expect(first).toEqual(second)
    expect(first).not.toBe(second)
  })

  it('never supplies fixture data to implemented pages', async () => {
    await expect(Effect.runPromise(Effect.flip(Effect.provide(listTasks, mockBackendLayer)))).resolves.toMatchObject({ _tag: 'OfflineError' })
  })
})
