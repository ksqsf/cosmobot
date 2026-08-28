import { Effect, Layer } from 'effect'
import { activity, logs, overviewCounts, platforms, plugins, tasks } from '@/fixtures/data'
import { AdminBackendService, FixtureError, ForbiddenError, OfflineError, type AdminBackend, type BackendEffect } from './AdminBackend'
import type { FixtureScenario, OverviewSnapshot } from '@/types/domain'

const copy = <T>(value: T): T => structuredClone(value)

function overview(scenario: FixtureScenario = 'ready'): BackendEffect<OverviewSnapshot> {
  return Effect.gen(function*() {
    if (scenario === 'loading') yield* Effect.sleep('300 millis')
    if (scenario === 'error') return yield* new FixtureError({ message: 'The fixture request failed.' })
    if (scenario === 'offline') return yield* new OfflineError({ message: 'Cosmobot is offline. Showing the last snapshot.' })
    if (scenario === 'forbidden') return yield* new ForbiddenError({ message: 'Administrator permission is required.' })
    return {
      tasks: scenario === 'empty' ? [] : copy(tasks),
      activity: copy(activity),
      platforms: copy(platforms),
      sessionCount: overviewCounts.sessions,
      resourceCount: overviewCounts.resources,
    }
  })
}

const mockBackend: AdminBackend = {
  system: { overview },
  tasks: { list: () => Effect.succeed(copy(tasks)) },
  audit: {
    recent: () => Effect.succeed([]),
    get: () => Effect.succeed(null),
    thread: () => Effect.succeed([]),
    subscribe: () => Effect.succeed(() => undefined),
  },
  chat: {
    sessionCount: () => Effect.succeed(overviewCounts.sessions),
    list: () => Effect.succeed([]),
    open: () => Effect.fail(new FixtureError({ message: 'Connect to cosmobot to create a chat session.' })),
    history: () => Effect.succeed([]),
    fork: () => Effect.fail(new FixtureError({ message: 'Connect to cosmobot to fork a chat session.' })),
    rename: () => Effect.fail(new FixtureError({ message: 'Connect to cosmobot to rename a chat session.' })),
    delete: () => Effect.fail(new FixtureError({ message: 'Connect to cosmobot to delete a chat session.' })),
    upload: () => Effect.fail(new FixtureError({ message: 'Connect to cosmobot to upload an attachment.' })),
    discardAttachment: () => Effect.fail(new FixtureError({ message: 'Connect to cosmobot to discard an attachment.' })),
    send: () => Effect.fail(new FixtureError({ message: 'Connect to cosmobot to send a message.' })),
    subscribe: () => Effect.succeed(() => undefined),
  },
  resources: { count: () => Effect.succeed(overviewCounts.resources) },
  plugins: { list: () => Effect.succeed(copy(plugins)) },
  logs: { list: () => Effect.succeed(copy(logs)) },
}

export const mockBackendLayer = Layer.succeed(AdminBackendService, mockBackend)
export { mockBackend }
