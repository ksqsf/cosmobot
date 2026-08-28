import { describe, expect, it } from 'vitest'
import { Effect } from 'effect'
import { listPlugins, listTasks } from '@/backend/AdminBackend'
import { mockBackendLayer } from '@/backend/mockBackend'

describe('demo backend', () => {
  it('never supplies fixture data to implemented pages', async () => {
    await expect(Effect.runPromise(Effect.flip(Effect.provide(listTasks, mockBackendLayer)))).resolves.toMatchObject({ _tag: 'OfflineError' })
    await expect(Effect.runPromise(Effect.flip(Effect.provide(listPlugins, mockBackendLayer)))).resolves.toMatchObject({ _tag: 'OfflineError' })
  })
})
