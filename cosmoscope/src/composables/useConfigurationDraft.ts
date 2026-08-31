import { computed, onMounted, ref, watch, type ComputedRef, type Ref } from 'vue'
import { useToast } from 'primevue/usetoast'
import {
  getConfiguration, rollbackConfiguration, updateConfiguration, validateConfiguration,
} from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useLatest } from '@/async'
import { groupConfigSections } from '@/configuration/navigation'
import { useConnectionStore } from '@/stores/connection'
import type {
  ConfigChange, ConfigOption, ConfigSection, ConfigurationSnapshot, ConfigurationValidation,
} from '@/rpc/schemas'

export function pathKey(path: readonly string[]): string { return JSON.stringify(path) }

type RepeatableSection = ConfigurationSnapshot['configuration']['repeatableSections'][number]

export interface ConfigurationDraft {
  snapshot: Ref<ConfigurationSnapshot | undefined>
  selectedPath: Ref<string>
  drafts: Ref<Record<string, ConfigChange>>
  validation: Ref<ConfigurationValidation | undefined>
  providerNames: Ref<Record<string, string>>
  loading: Ref<boolean>
  validating: Ref<boolean>
  error: Ref<string>
  sections: ComputedRef<ConfigSection[]>
  navigationGroups: ComputedRef<ReturnType<typeof groupConfigSections>>
  selected: ComputedRef<ConfigSection | undefined>
  changes: ComputedRef<ConfigChange[]>
  selectedEnabled: ComputedRef<boolean>
  busy: ComputedRef<boolean>
  supportsConfigGet: ComputedRef<boolean>
  canManage: ComputedRef<boolean>
  controlsDisabled: ComputedRef<boolean>
  managementDisabled: ComputedRef<boolean>
  canRollback: ComputedRef<boolean>
  canRestart: ComputedRef<boolean>
  applyReady: ComputedRef<boolean>
  draftValue: (option: ConfigOption) => unknown
  setOption: (option: ConfigOption, value: unknown) => void
  replaceSecret: (option: ConfigOption, value: string | undefined) => void
  removeOption: (option: ConfigOption) => void
  resetOption: (option: ConfigOption) => void
  addProvider: (template: RepeatableSection) => void
  removeProvider: (section: ConfigSection) => void
  addOptionalSection: (section: ConfigSection) => void
  removeOptionalSection: (section: ConfigSection) => void
  clearDrafts: () => void
  refresh: (discardDrafts?: boolean) => Promise<void>
  validate: () => Promise<void>
  apply: () => Promise<void>
  rollback: (backupRevision: string) => Promise<void>
}

export function useConfigurationDraft(): ConfigurationDraft {
  const connection = useConnectionStore()
  const toast = useToast()
  const refreshLatest = useLatest()
  const validationLatest = useLatest()
  const mutationLatest = useLatest()
  const snapshot = ref<ConfigurationSnapshot>()
  const selectedPath = ref('')
  const drafts = ref<Record<string, ConfigChange>>({})
  const sectionChanges = ref<ConfigChange[]>([])
  const draftSections = ref<ConfigSection[]>([])
  const validation = ref<ConfigurationValidation>()
  const validatedFingerprint = ref('')
  const providerNames = ref<Record<string, string>>({})
  const loading = ref(false)
  const validating = ref(false)
  const error = ref('')

  const sections = computed(() => {
    const removed = new Set(sectionChanges.value.flatMap((change) => change.operation === 'remove_section' ? [pathKey(change.path)] : []))
    return [...(snapshot.value?.configuration.sections ?? []), ...draftSections.value]
      .filter((section) => section.optional || !removed.has(pathKey(section.path)))
  })
  const navigationGroups = computed(() => snapshot.value === undefined ? [] : groupConfigSections({
    ...snapshot.value.configuration,
    sections: sections.value,
  }))
  const selected = computed(() => sections.value.find((section) => pathKey(section.path) === selectedPath.value) ?? sections.value[0])
  const changes = computed<ConfigChange[]>(() => [...sectionChanges.value, ...Object.values(drafts.value)])
  const fingerprint = computed(() => JSON.stringify(changes.value))
  const selectedEnabled = computed(() => {
    const section = selected.value
    if (section === undefined) return true
    const operation = sectionChanges.value.find((change) => pathKey(change.path) === pathKey(section.path))?.operation
    if (section.repeatable && !section.present) return operation === 'add_section'
    if (!section.optional) return true
    return operation === 'add_section' || (section.present && operation !== 'remove_section')
  })
  const busy = computed(() => loading.value || validating.value)
  const supportsConfigGet = computed(() => connection.state === 'authenticated' && connection.methods.has('config.get'))
  const canManage = computed(() => connection.state === 'authenticated' && snapshot.value?.editable === true &&
    ['config.validate', 'config.update'].every((method) => connection.methods.has(method)))
  const controlsDisabled = computed(() => !canManage.value || busy.value || !selectedEnabled.value)
  const managementDisabled = computed(() => !canManage.value || busy.value)
  const canRollback = computed(() => canManage.value && !busy.value && changes.value.length === 0 &&
    snapshot.value?.backup != null && connection.methods.has('config.rollback'))
  const canRestart = computed(() => connection.state === 'authenticated' && !busy.value &&
    snapshot.value !== undefined && connection.methods.has('admin.restart'))
  const applyReady = computed(() => changes.value.length > 0 && validation.value?.valid === true &&
    validatedFingerprint.value === fingerprint.value)

  function invalidateValidation(): void {
    validationLatest.invalidate()
    validation.value = undefined
    validatedFingerprint.value = ''
  }

  function baseValue(option: ConfigOption): unknown {
    const owner = sections.value.find((section) => section.options.some((candidate) => pathKey(candidate.path) === pathKey(option.path)))
    const readded = owner !== undefined && !owner.present && sectionChanges.value.some((change) =>
      change.operation === 'add_section' && pathKey(change.path) === pathKey(owner.path))
    if (readded) return option.default
    return option.source.present ? option.source.value : option.effective ?? option.default
  }

  function draftValue(option: ConfigOption): unknown {
    const draft = drafts.value[pathKey(option.path)]
    if (draft?.operation === 'set' || draft?.operation === 'replace_secret') return draft.value
    if (draft?.operation === 'clear_secret') return null
    if (draft?.operation === 'remove') return option.default
    return baseValue(option)
  }

  function setOption(option: ConfigOption, value: unknown): void {
    const key = pathKey(option.path)
    drafts.value = JSON.stringify(value) === JSON.stringify(baseValue(option))
      ? Object.fromEntries(Object.entries(drafts.value).filter(([path]) => path !== key))
      : { ...drafts.value, [key]: { operation: 'set', path: option.path, value } }
    invalidateValidation()
  }

  function replaceSecret(option: ConfigOption, value: string | undefined): void {
    const next = value ?? ''
    const key = pathKey(option.path)
    if (next === '') drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([path]) => path !== key))
    else drafts.value = { ...drafts.value, [key]: { operation: 'replace_secret', path: option.path, value: next } }
    invalidateValidation()
  }

  function removeOption(option: ConfigOption): void {
    drafts.value = { ...drafts.value, [pathKey(option.path)]: {
      operation: option.type.kind === 'secret' ? 'clear_secret' : 'remove', path: option.path,
    } }
    invalidateValidation()
  }

  function resetOption(option: ConfigOption): void {
    const key = pathKey(option.path)
    drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([path]) => path !== key))
    invalidateValidation()
  }

  function addProvider(template: ConfigurationSnapshot['configuration']['repeatableSections'][number]): void {
    const family = pathKey(template.path)
    const name = providerNames.value[family]?.trim()
    if (!name) return
    const providerPath = [...template.path, name]
    const key = pathKey(providerPath)
    const existing = snapshot.value?.configuration.sections.find((section) => pathKey(section.path) === key)
    const pendingRemoval = sectionChanges.value.some((change) => change.operation === 'remove_section' && pathKey(change.path) === key)
    if (existing !== undefined && (pendingRemoval || !existing.present)) {
      sectionChanges.value = [...sectionChanges.value.filter((change) => pathKey(change.path) !== key),
        ...(!existing.present ? [{ operation: 'add_section' as const, path: providerPath }] : [])]
      providerNames.value = { ...providerNames.value, [family]: '' }
      selectedPath.value = key
      error.value = ''
      invalidateValidation()
      return
    }
    if (sections.value.some((section) => pathKey(section.path) === key)) {
      error.value = `A provider named “${name}” already exists in ${template.label.toLowerCase()}.`
      return
    }
    const section: ConfigSection = {
      path: providerPath, label: name, group: template.group, optional: false, present: false, repeatable: true,
      options: template.options.map((option) => ({
        ...option,
        path: option.path.map((segment) => segment === '*' ? name : segment),
        source: { present: false, value: null },
        effective: option.default,
      })),
    }
    sectionChanges.value = [...sectionChanges.value, { operation: 'add_section', path: providerPath }]
    draftSections.value = [...draftSections.value, section]
    providerNames.value = { ...providerNames.value, [family]: '' }
    selectedPath.value = key
    error.value = ''
    invalidateValidation()
  }

  function removeProvider(section: ConfigSection): void {
    const key = pathKey(section.path)
    const wasAdded = draftSections.value.some((candidate) => pathKey(candidate.path) === key) ||
      sectionChanges.value.some((change) => change.operation === 'add_section' && pathKey(change.path) === key)
    draftSections.value = draftSections.value.filter((candidate) => pathKey(candidate.path) !== key)
    sectionChanges.value = wasAdded
      ? sectionChanges.value.filter((change) => !(change.operation === 'add_section' && pathKey(change.path) === key))
      : [...sectionChanges.value.filter((change) => pathKey(change.path) !== key), { operation: 'remove_section', path: section.path }]
    drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([, change]) =>
      !section.path.every((segment, index) => change.path[index] === segment)))
    selectedPath.value = pathKey(sections.value[0]?.path ?? [])
    invalidateValidation()
  }

  function addOptionalSection(section: ConfigSection): void {
    sectionChanges.value = [...sectionChanges.value.filter((change) => pathKey(change.path) !== pathKey(section.path)),
      ...(section.present ? [] : [{ operation: 'add_section' as const, path: section.path }])]
    invalidateValidation()
  }

  function removeOptionalSection(section: ConfigSection): void {
    sectionChanges.value = [...sectionChanges.value.filter((change) => pathKey(change.path) !== pathKey(section.path)),
      ...(section.present ? [{ operation: 'remove_section' as const, path: section.path }] : [])]
    drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([, change]) =>
      !section.path.every((segment, index) => change.path[index] === segment)))
    invalidateValidation()
  }

  function clearDrafts(): void {
    drafts.value = {}
    sectionChanges.value = []
    draftSections.value = []
    invalidateValidation()
    if (!sections.value.some((section) => pathKey(section.path) === selectedPath.value))
      selectedPath.value = pathKey(sections.value[0]?.path ?? [])
  }

  async function refresh(discardDrafts = false): Promise<void> {
    const token = refreshLatest.begin()
    if (connection.state !== 'authenticated' || !connection.methods.has('config.get')) {
      if (snapshot.value === undefined) {
        selectedPath.value = ''
        error.value = connection.state === 'authenticated'
          ? 'This cosmobot server does not support configuration inspection.'
          : 'Connect to cosmobot to inspect configuration.'
      }
      return
    }
    if (!discardDrafts && changes.value.length > 0) return
    loading.value = true
    const result = await runBackend(getConfiguration)
    if (!refreshLatest.current(token)) return
    loading.value = false
    if (result._tag === 'Failure') { error.value = result.error.message; return }
    error.value = ''
    snapshot.value = result.value
    if (discardDrafts) clearDrafts()
    if (!sections.value.some((section) => pathKey(section.path) === selectedPath.value))
      selectedPath.value = pathKey(sections.value[0]?.path ?? [])
  }

  async function validate(): Promise<void> {
    if (!snapshot.value || busy.value || changes.value.length === 0) return
    const token = validationLatest.begin()
    const expectedFingerprint = fingerprint.value
    const revision = snapshot.value.revision
    validating.value = true
    const result = await runBackend(validateConfiguration(revision, changes.value))
    if (!validationLatest.current(token)) return
    validating.value = false
    if (expectedFingerprint !== fingerprint.value || revision !== snapshot.value.revision) return
    if (result._tag === 'Failure') { error.value = result.error.message; return }
    validation.value = result.value
    validatedFingerprint.value = expectedFingerprint
    error.value = ''
  }

  async function apply(): Promise<void> {
    if (!applyReady.value || !snapshot.value || busy.value) return
    const token = mutationLatest.begin()
    loading.value = true
    const result = await runBackend(updateConfiguration(snapshot.value.revision, changes.value))
    if (!mutationLatest.current(token)) return
    loading.value = false
    if (result._tag === 'Failure') { error.value = result.error.message; invalidateValidation(); return }
    clearDrafts()
    toast.add({ severity: 'success', summary: 'Configuration updated', detail: 'Restart to activate the changes.', life: 3500 })
    await refresh()
  }

  async function rollback(backupRevision: string): Promise<void> {
    if (!snapshot.value || busy.value || changes.value.length > 0) return
    const token = mutationLatest.begin()
    loading.value = true
    const result = await runBackend(rollbackConfiguration(snapshot.value.revision, backupRevision))
    if (!mutationLatest.current(token)) return
    loading.value = false
    if (result._tag === 'Failure') { error.value = result.error.message; return }
    clearDrafts()
    toast.add({ severity: 'success', summary: 'Configuration rolled back', detail: 'Restart to activate it.', life: 3500 })
    await refresh()
  }

  watch([() => connection.state, () => connection.methods], () => {
    refreshLatest.invalidate()
    invalidateValidation()
    mutationLatest.invalidate()
    loading.value = false
    validating.value = false
    if (supportsConfigGet.value) void refresh()
  })
  onMounted(() => { void refresh() })

  return {
    snapshot, selectedPath, drafts, validation, providerNames, loading, validating, error,
    sections, navigationGroups, selected, changes, selectedEnabled, busy, supportsConfigGet,
    canManage, controlsDisabled, managementDisabled, canRollback, canRestart, applyReady,
    draftValue, setOption, replaceSecret, removeOption, resetOption,
    addProvider, removeProvider, addOptionalSection, removeOptionalSection,
    clearDrafts, refresh, validate, apply, rollback,
  }
}
