import { describe, expect, it } from 'vitest'
import { Effect } from 'effect'
import { getOverview, listTasks } from '@/backend/AdminBackend'
import { mockBackendLayer } from '@/backend/mockBackend'

describe('fixture backend', () => {
  it('returns deterministic snapshots without sharing mutable records', async () => {
    const first = await Effect.runPromise(Effect.provide(listTasks, mockBackendLayer))
    const second = await Effect.runPromise(Effect.provide(listTasks, mockBackendLayer))
    const firstTask = first[0]
    const secondTask = second[0]
    expect(firstTask).toBeDefined()
    expect(secondTask).toBeDefined()
    if (firstTask === undefined || secondTask === undefined) throw new Error('Fixture task is missing')
    firstTask.label = 'changed'
    expect(secondTask.label).toBe('agent.run')
  })

  it('provides empty and failure scenarios', async () => {
    await expect(Effect.runPromise(Effect.provide(getOverview('empty'), mockBackendLayer))).resolves.toMatchObject({ tasks: [] })
    await expect(Effect.runPromise(Effect.flip(Effect.provide(getOverview('offline'), mockBackendLayer)))).resolves.toMatchObject({ _tag: 'OfflineError' })
    await expect(Effect.runPromise(Effect.flip(Effect.provide(getOverview('forbidden'), mockBackendLayer)))).resolves.toMatchObject({ _tag: 'ForbiddenError' })
  })
})
