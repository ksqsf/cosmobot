import { describe, expect, it } from 'vitest'
import { configChangeSchema, configurationGetSchema } from '@/rpc/schemas'
import { configSectionTitle, groupConfigSections } from '@/configuration/navigation'
import { configListItemKind, configTextInputValue, displayConfigValue } from '@/configuration/values'

const group = (path: string[], label: string): { path: string[], label: string } => ({ path, label })

const option = {
  path: ['rpc', 'token'], label: 'Token', description: 'RPC token', owner: 'Bot.RPC.Config',
  type: { kind: 'secret' }, required: false, default: 'unset', constraints: {}, activation: 'restart',
  source: { present: true, value: 'configured' }, effective: 'configured',
}

const snapshot = {
  schemaVersion: 2, revision: 'current', activeRevision: 'active', sourceState: 'valid', editable: true,
  diagnostics: [], configuration: {
    sections: [{ path: ['rpc'], label: 'RPC', group: group(['interfaces'], 'Interfaces'), optional: false, present: true, repeatable: false, options: [option] }],
    repeatableSections: [],
  }, backup: null,
}

describe('configuration RPC schemas', () => {
  it('accepts redacted secrets and rejects secret material', () => {
    expect(configurationGetSchema.safeParse(snapshot).success).toBe(true)
    expect(configurationGetSchema.safeParse({ ...snapshot, schemaVersion: 1 }).success).toBe(false)
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

  it('rejects incomplete or decorated option type variants', () => {
    expect(configurationGetSchema.safeParse({
      ...snapshot,
      configuration: { ...snapshot.configuration, sections: [{ ...snapshot.configuration.sections[0], options: [{ ...option, type: { kind: 'enum' } }] }] },
    }).success).toBe(false)
    expect(configurationGetSchema.safeParse({
      ...snapshot,
      configuration: { ...snapshot.configuration, sections: [{ ...snapshot.configuration.sections[0], options: [{ ...option, type: { kind: 'text', values: ['integer'] } }] }] },
    }).success).toBe(false)
    expect(configurationGetSchema.safeParse({
      ...snapshot,
      configuration: { ...snapshot.configuration, sections: [{ ...snapshot.configuration.sections[0], options: [{
        ...option, type: { kind: 'list', values: ['text'] }, default: [], source: { present: true, value: [true] }, effective: [true],
      }] }] },
    }).success).toBe(false)
  })

  it('uses schema-owned section and group labels', () => {
    const configuration = {
      sections: [
        { path: ['rpc'], label: 'RPC', group: group(['interfaces'], 'Interfaces'), optional: false, present: true, repeatable: false, options: [] },
        { path: ['driver', 'qq'], label: 'QQ', group: group(['drivers'], 'Chat drivers'), optional: true, present: true, repeatable: false, options: [] },
        { path: ['llm'], label: 'General', group: group(['llm'], 'LLM'), optional: false, present: true, repeatable: false, options: [] },
        { path: ['llm', 'chat_provider', 'openrouter'], label: 'openrouter', group: group(['llm'], 'LLM'), optional: false, present: true, repeatable: true, options: [] },
        { path: ['llm', 'image_provider', 'images'], label: 'images', group: group(['llm'], 'LLM'), optional: false, present: true, repeatable: true, options: [] },
      ],
      repeatableSections: [
        { path: ['llm', 'chat_provider'], label: 'Chat providers', group: group(['llm'], 'LLM'), options: [] },
        { path: ['llm', 'image_provider'], label: 'Image providers', group: group(['llm'], 'LLM'), options: [] },
        { path: ['llm', 'audio_provider'], label: 'Audio providers', group: group(['llm'], 'LLM'), options: [] },
      ],
    }
    const parsed = configurationGetSchema.parse({ ...snapshot, configuration }).configuration
    const groups = groupConfigSections(parsed)
    expect(groups.map(({ label }) => label)).toEqual(['Interfaces', 'Chat drivers', 'LLM'])
    const llm = groups.find(({ label }) => label === 'LLM')
    expect(llm?.clusters.map(({ label }) => label)).toEqual(['', 'Chat providers', 'Image providers', 'Audio providers'])
    expect(llm?.clusters.find(({ label }) => label === 'Chat providers')?.sections.map(configSectionTitle)).toEqual(['openrouter'])
    expect(parsed.sections.map(configSectionTitle).slice(0, 2)).toEqual(['RPC', 'QQ'])
  })

  it('maps schema types to unquoted scalar and structured-list values', () => {
    expect(configTextInputValue('plain text', 'text')).toBe('plain text')
    expect(configTextInputValue('configured', 'secret')).toBe('')
    expect(displayConfigValue('plain text')).toBe('plain text')
    const parsedOption = configurationGetSchema.parse(snapshot).configuration.sections.flatMap(({ options }) => options).at(0)
    if (parsedOption === undefined) throw new Error('missing configuration option fixture')
    expect(configListItemKind({ ...parsedOption, type: { kind: 'list', values: ['integer'] } })).toBe('integer')
    expect(configListItemKind({ ...parsedOption, type: { kind: 'identity_list' } })).toBe('identity')
  })
})
