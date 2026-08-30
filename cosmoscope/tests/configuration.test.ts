import { describe, expect, it } from 'vitest'
import { configChangeSchema, configurationGetSchema } from '@/rpc/schemas'

const option = {
  path: ['rpc', 'token'], label: 'Token', description: 'RPC token', owner: 'Bot.RPC.Config',
  type: { kind: 'secret' }, required: false, default: 'unset', constraints: {}, activation: 'restart',
  source: { present: true, value: 'configured' }, effective: 'configured',
}

const snapshot = {
  schemaVersion: 1, revision: 'current', activeRevision: 'active', sourceState: 'valid', editable: true,
  diagnostics: [], configuration: {
    sections: [{ path: ['rpc'], label: 'RPC', repeatable: false, options: [option] }],
    repeatableSections: [],
  }, backup: null,
}

describe('configuration RPC schemas', () => {
  it('accepts redacted secrets and rejects secret material', () => {
    expect(configurationGetSchema.safeParse(snapshot).success).toBe(true)
    expect(configurationGetSchema.safeParse({
      ...snapshot,
      configuration: { ...snapshot.configuration, sections: [{ ...snapshot.configuration.sections[0], options: [{ ...option, effective: 'actual-secret' }] }] },
    }).success).toBe(false)
  })

  it('rejects dotted paths and unknown change operations', () => {
    expect(configChangeSchema.safeParse({ operation: 'set', path: ['rpc', 'port'], value: 38765 }).success).toBe(true)
    expect(configChangeSchema.safeParse({ operation: 'set', path: 'rpc.port', value: 38765 }).success).toBe(false)
    expect(configChangeSchema.safeParse({ operation: 'patch', path: ['rpc', 'port'], value: 38765 }).success).toBe(false)
  })
})
