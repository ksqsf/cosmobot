import { afterEach, expect, it } from 'vitest'
import { Effect } from 'effect'
import { countAudit, type AdminBackend } from '@/backend/AdminBackend'
import { mockBackend } from '@/backend/mockBackend'
import { runBackend, setAdminBackend } from '@/backend/runBackend'

afterEach(() => setAdminBackend(mockBackend))

it('resolves the current backend when an existing program runs', async () => {
  const backend: AdminBackend = {
    ...mockBackend,
    audit: { ...mockBackend.audit, count: () => Effect.succeed(7) },
  }
  await expect(runBackend(countAudit)).resolves.toMatchObject({
    _tag: 'Failure', error: { _tag: 'OfflineError' },
  })
  setAdminBackend(backend)
  await expect(runBackend(countAudit)).resolves.toEqual({ _tag: 'Success', value: 7 })
  setAdminBackend(mockBackend)
  await expect(runBackend(countAudit)).resolves.toMatchObject({
    _tag: 'Failure', error: { _tag: 'OfflineError' },
  })
})
