// @vitest-environment jsdom

import { flushPromises, mount } from '@vue/test-utils'
import { defineComponent, h, reactive, nextTick } from 'vue'
import { describe, expect, it, vi } from 'vitest'
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

  it('keeps drafts when an update from an old connection completes after reconnect', async () => {
    vi.resetModules()
    const connection = reactive<{ state: string, methods: Set<string> }>({
      state: 'authenticated',
      methods: new Set(['config.get', 'config.validate', 'config.update']),
    })
    const addToast = vi.fn()
    let resolveUpdate!: (value: unknown) => void
    vi.doMock('primevue/usetoast', () => ({ useToast: () => ({ add: addToast }) }))
    vi.doMock('@/stores/connection', () => ({ useConnectionStore: () => connection }))
    vi.doMock('@/backend/AdminBackend', () => ({
      getConfiguration: { tag: 'get' },
      validateConfiguration: () => ({ tag: 'validate' }),
      updateConfiguration: () => ({ tag: 'update' }),
      rollbackConfiguration: () => ({ tag: 'rollback' }),
    }))
    vi.doMock('@/backend/runBackend', () => ({
      runBackend: (operation: { tag: string }): Promise<unknown> => {
        if (operation.tag === 'get') return Promise.resolve({ _tag: 'Success', value: configurationGetSchema.parse(snapshot) })
        if (operation.tag === 'validate') return Promise.resolve({
          _tag: 'Success',
          value: { valid: true, revision: 'current', diagnostics: [], diff: [], restartRequired: true },
        })
        if (operation.tag === 'update') return new Promise((resolve) => { resolveUpdate = resolve })
        return Promise.resolve({ _tag: 'Failure', error: new Error('unexpected operation') })
      },
    }))
    const { useConfigurationDraft } = await import('@/composables/useConfigurationDraft')
    let draft!: ReturnType<typeof useConfigurationDraft>
    const wrapper = mount(defineComponent({
      setup() { draft = useConfigurationDraft(); return () => h('div') },
    }))
    await flushPromises()
    const parsedOption = draft.snapshot.value?.configuration.sections[0]?.options[0]
    if (parsedOption === undefined) throw new Error('missing configuration option fixture')
    draft.replaceSecret(parsedOption, 'replacement')
    await draft.validate()
    expect(draft.applyReady.value).toBe(true)

    const applying = draft.apply()
    expect(draft.loading.value).toBe(true)
    connection.state = 'reconnecting'
    await nextTick()
    expect(draft.loading.value).toBe(false)
    expect(draft.changes.value).toHaveLength(1)
    connection.state = 'authenticated'
    connection.methods = new Set(['config.get', 'config.validate', 'config.update'])
    await nextTick()
    resolveUpdate({ _tag: 'Success', value: { updated: true } })
    await applying

    expect(draft.changes.value).toHaveLength(1)
    expect(draft.validation.value).toBeUndefined()
    expect(addToast).not.toHaveBeenCalled()
    wrapper.unmount()
    vi.resetModules()
    vi.doUnmock('primevue/usetoast')
    vi.doUnmock('@/stores/connection')
    vi.doUnmock('@/backend/AdminBackend')
    vi.doUnmock('@/backend/runBackend')
  })
})
