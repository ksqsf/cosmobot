<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Checkbox from 'primevue/checkbox'
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import PageHeading from '@/components/PageHeading.vue'
import ConfigListInput from '@/components/configuration/ConfigListInput.vue'
import ConfigIdentityInput from '@/components/configuration/ConfigIdentityInput.vue'
import { configSectionTitle, groupConfigSections } from '@/configuration/navigation'
import { configListItemKind, configTextInputValue, displayConfigValue } from '@/configuration/values'
import {
  getConfiguration, restartCosmobot, rollbackConfiguration,
  updateConfiguration, validateConfiguration,
} from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useLayeredConfirm } from '@/overlay'
import { useConnectionStore } from '@/stores/connection'
import type {
  ConfigChange, ConfigOption, ConfigSection, ConfigurationSnapshot,
  ConfigurationValidation,
} from '@/rpc/schemas'

const connection = useConnectionStore()
const confirm = useLayeredConfirm()
const toast = useToast()
const loading = ref(false)
const error = ref('')
const snapshot = ref<ConfigurationSnapshot>()
const selectedPath = ref('')
const drafts = ref<Record<string, ConfigChange>>({})
const sectionChanges = ref<ConfigChange[]>([])
const draftSections = ref<ConfigSection[]>([])
const validation = ref<ConfigurationValidation>()
const validating = ref(false)
const validatedFingerprint = ref('')
const providerNames = ref<Record<string, string>>({})
let draftGeneration = 0
let refreshGeneration = 0

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
const selectedEnabled = computed(() => {
  const section = selected.value
  if (section === undefined) return true
  const operation = sectionChanges.value.find((change) => pathKey(change.path) === pathKey(section.path))?.operation
  if (section.repeatable && !section.present) return operation === 'add_section'
  if (!section.optional) return true
  return operation === 'add_section' || (section.present && operation !== 'remove_section')
})
const changesFingerprint = computed(() => JSON.stringify(changes.value))
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
  validatedFingerprint.value === changesFingerprint.value)

function pathKey(path: readonly string[]): string { return JSON.stringify(path) }
function displayPath(path: readonly string[]): string { return path.join('.') }

function baseValue(option: ConfigOption): unknown {
  const ownerSection = sections.value.find((section) => section.options.some((candidate) => pathKey(candidate.path) === pathKey(option.path)))
  const readded = ownerSection !== undefined && !ownerSection.present && sectionChanges.value.some((change) =>
    change.operation === 'add_section' && pathKey(change.path) === pathKey(ownerSection.path))
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

function displayValue(value: unknown): string {
  return displayConfigValue(value)
}

function inputValue(option: ConfigOption): string {
  return configTextInputValue(draftValue(option), option.type.kind)
}

function numericValue(option: ConfigOption): number | null {
  const value = draftValue(option)
  return typeof value === 'number' ? value : null
}

function listValue(option: ConfigOption): unknown[] {
  const value = draftValue(option)
  return Array.isArray(value) ? value : []
}

function identityValue(option: ConfigOption): string | number | null {
  const value = draftValue(option)
  return typeof value === 'string' || typeof value === 'number' ? value : null
}

function numericConstraint(option: ConfigOption, key: 'minimum' | 'maximum'): number | undefined {
  if (typeof option.constraints !== 'object' || option.constraints === null) return undefined
  const value = (option.constraints as Record<string, unknown>)[key]
  return typeof value === 'number' ? value : undefined
}

function markDraftChanged(): void {
  draftGeneration += 1
  refreshGeneration += 1
  validation.value = undefined
  validatedFingerprint.value = ''
}

function setOption(option: ConfigOption, value: unknown): void {
  const key = pathKey(option.path)
  drafts.value = JSON.stringify(value) === JSON.stringify(baseValue(option))
    ? Object.fromEntries(Object.entries(drafts.value).filter(([path]) => path !== key))
    : { ...drafts.value, [key]: { operation: 'set', path: option.path, value } }
  markDraftChanged()
}

function setText(option: ConfigOption, value: string | undefined): void {
  setOption(option, value ?? '')
}

function setEnum(option: ConfigOption, event: Event): void {
  setOption(option, (event.target as HTMLSelectElement).value)
}

function replaceSecret(option: ConfigOption, value: string | undefined): void {
  const next = value ?? ''
  const key = pathKey(option.path)
  const copy = { ...drafts.value }
  if (next === '') drafts.value = Object.fromEntries(Object.entries(copy).filter(([path]) => path !== key))
  else copy[key] = { operation: 'replace_secret', path: option.path, value: next }
  if (next !== '') drafts.value = copy
  markDraftChanged()
}

function removeOption(option: ConfigOption): void {
  drafts.value = { ...drafts.value, [pathKey(option.path)]: {
    operation: option.type.kind === 'secret' ? 'clear_secret' : 'remove', path: option.path,
  } }
  markDraftChanged()
}

function resetOption(option: ConfigOption): void {
  const key = pathKey(option.path)
  drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([path]) => path !== key))
  markDraftChanged()
}

function addProvider(template: ConfigurationSnapshot['configuration']['repeatableSections'][number]): void {
  const path = template.path
  const family = pathKey(path)
  const name = providerNames.value[family]?.trim()
  if (!name) return
  const providerPath = [...path, name]
  const existing = snapshot.value?.configuration.sections.find((section) => pathKey(section.path) === pathKey(providerPath))
  const pendingRemoval = sectionChanges.value.some((change) => change.operation === 'remove_section' && pathKey(change.path) === pathKey(providerPath))
  if (existing !== undefined && pendingRemoval) {
    sectionChanges.value = sectionChanges.value.filter((change) => !(change.operation === 'remove_section' && pathKey(change.path) === pathKey(providerPath)))
    providerNames.value = { ...providerNames.value, [family]: '' }
    selectedPath.value = pathKey(providerPath)
    error.value = ''
    markDraftChanged()
    return
  }
  if (existing !== undefined && !existing.present) {
    sectionChanges.value = [...sectionChanges.value.filter((change) => pathKey(change.path) !== pathKey(providerPath)), { operation: 'add_section', path: providerPath }]
    providerNames.value = { ...providerNames.value, [family]: '' }
    selectedPath.value = pathKey(providerPath)
    error.value = ''
    markDraftChanged()
    return
  }
  if (sections.value.some((section) => pathKey(section.path) === pathKey(providerPath))) {
    error.value = `A provider named “${name}” already exists in ${template.label.toLowerCase()}.`
    return
  }
  const section: ConfigSection = {
    path: providerPath,
    label: name,
    group: template.group,
    optional: false,
    present: false,
    repeatable: true,
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
  selectedPath.value = pathKey(providerPath)
  error.value = ''
  markDraftChanged()
}

function removeProvider(section: ConfigSection): void {
  const key = pathKey(section.path)
  const wasAdded = draftSections.value.some((candidate) => pathKey(candidate.path) === key) ||
    sectionChanges.value.some((change) => change.operation === 'add_section' && pathKey(change.path) === key)
  draftSections.value = draftSections.value.filter((candidate) => pathKey(candidate.path) !== key)
  sectionChanges.value = wasAdded
    ? sectionChanges.value.filter((change) => !(change.operation === 'add_section' && pathKey(change.path) === key))
    : [...sectionChanges.value.filter((change) => pathKey(change.path) !== key), { operation: 'remove_section', path: section.path }]
  drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([, change]) => !section.path.every((segment, index) => change.path[index] === segment)))
  selectedPath.value = pathKey(sections.value[0]?.path ?? [])
  markDraftChanged()
}

function addOptionalSection(section: ConfigSection): void {
  sectionChanges.value = [
    ...sectionChanges.value.filter((change) => pathKey(change.path) !== pathKey(section.path)),
    ...(section.present ? [] : [{ operation: 'add_section' as const, path: section.path }]),
  ]
  markDraftChanged()
}

function removeOptionalSection(section: ConfigSection): void {
  sectionChanges.value = [
    ...sectionChanges.value.filter((change) => pathKey(change.path) !== pathKey(section.path)),
    ...(section.present ? [{ operation: 'remove_section' as const, path: section.path }] : []),
  ]
  drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([, change]) =>
    !section.path.every((segment, index) => change.path[index] === segment)))
  markDraftChanged()
}

function clearDrafts(): void {
  drafts.value = {}
  sectionChanges.value = []
  draftSections.value = []
  markDraftChanged()
  if (!sections.value.some((section) => pathKey(section.path) === selectedPath.value))
    selectedPath.value = pathKey(sections.value[0]?.path ?? [])
}

async function refresh(discardDrafts = false): Promise<void> {
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
  const generation = ++refreshGeneration
  loading.value = true
  const result = await runBackend(getConfiguration)
  loading.value = false
  if (generation !== refreshGeneration) return
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  snapshot.value = result.value
  if (!sections.value.some((section) => pathKey(section.path) === selectedPath.value))
    selectedPath.value = pathKey(sections.value[0]?.path ?? [])
  if (discardDrafts) clearDrafts()
}

function requestRefresh(): void {
  if (changes.value.length === 0) { void refresh(true); return }
  confirm.require({
    header: 'Discard drafts and refresh?',
    message: 'Refreshing loads the current file and discards every unapplied configuration change.',
    rejectLabel: 'Keep drafts', acceptLabel: 'Discard and refresh', acceptClass: 'p-button-danger',
    accept: () => { void refresh(true) },
  })
}

async function validate(): Promise<void> {
  if (!snapshot.value || busy.value || changes.value.length === 0) return
  const generation = draftGeneration
  const fingerprint = changesFingerprint.value
  const revision = snapshot.value.revision
  validating.value = true
  const result = await runBackend(validateConfiguration(revision, changes.value))
  validating.value = false
  if (generation !== draftGeneration || fingerprint !== changesFingerprint.value || revision !== snapshot.value.revision) return
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  validation.value = result.value
  validatedFingerprint.value = fingerprint
  error.value = ''
}

async function apply(): Promise<void> {
  if (!applyReady.value || !snapshot.value || busy.value) return
  loading.value = true
  const result = await runBackend(updateConfiguration(snapshot.value.revision, changes.value))
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; validation.value = undefined; validatedFingerprint.value = ''; return }
  clearDrafts()
  toast.add({ severity: 'success', summary: 'Configuration updated', detail: 'Restart to activate the changes.', life: 3500 })
  await refresh()
}

function requestRollback(): void {
  const backup = snapshot.value?.backup
  if (!backup) return
  confirm.require({
    header: 'Roll back configuration?',
    message: 'Swap the current file with the previous valid configuration? A restart is still required.',
    rejectLabel: 'Cancel', acceptLabel: 'Roll back', acceptClass: 'p-button-danger',
    accept: () => { void rollback(backup.revision) },
  })
}

async function rollback(backupRevision: string): Promise<void> {
  if (!snapshot.value || busy.value || changes.value.length > 0) return
  loading.value = true
  const result = await runBackend(rollbackConfiguration(snapshot.value.revision, backupRevision))
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  clearDrafts()
  toast.add({ severity: 'success', summary: 'Configuration rolled back', detail: 'Restart to activate it.', life: 3500 })
  await refresh()
}

function requestRestart(): void {
  confirm.require({
    header: 'Restart cosmobot?', message: 'Active conversations may be interrupted while the service reconnects.',
    rejectLabel: 'Cancel', acceptLabel: 'Restart', acceptClass: 'p-button-danger',
    accept: () => { void restart() },
  })
}

async function restart(): Promise<void> {
  if (busy.value || connection.state !== 'authenticated') return
  loading.value = true
  const result = await runBackend(restartCosmobot)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  toast.add({ severity: 'info', summary: 'Restart requested', detail: 'The server acknowledged the request.', life: 3500 })
}

onMounted(() => { void refresh() })
watch(supportsConfigGet, (supported) => { if (supported) void refresh() })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Settings"
      title="Configuration"
      description="Inspect typed source and effective settings. Every change activates after restart."
    >
      <div class="action-row">
        <Button
          label="Refresh"
          icon="pi pi-refresh"
          severity="secondary"
          :loading="loading"
          :disabled="busy"
          @click="requestRefresh"
        />
        <Button
          v-if="canRollback"
          label="Roll back"
          severity="secondary"
          @click="requestRollback"
        />
        <Button
          v-if="canRestart"
          label="Restart"
          severity="danger"
          outlined
          @click="requestRestart"
        />
      </div>
    </PageHeading>
    <Message
      v-if="error"
      severity="error"
      closable
      @close="error = ''"
    >
      {{ error }}
    </Message>
    <Message
      v-if="snapshot?.sourceState === 'invalid'"
      severity="error"
      :closable="false"
    >
      The current TOML is invalid. Showing the active startup snapshot; editing is disabled.
      <ul>
        <li
          v-for="diagnostic in snapshot.diagnostics"
          :key="`${diagnostic.line}:${diagnostic.column}:${diagnostic.code}`"
        >
          {{ diagnostic.message }}<template v-if="diagnostic.line !== null">
            (line {{ diagnostic.line }}, column {{ diagnostic.column }})
          </template>
        </li>
      </ul>
    </Message>
    <div class="config-layout panel">
      <aside class="config-nav">
        <div
          v-for="group in navigationGroups"
          :key="group.key"
          class="config-nav-group"
        >
          <p class="nav-label">
            {{ group.label }}
          </p>
          <div
            v-for="cluster in group.clusters"
            :key="cluster.key"
            class="config-nav-cluster"
          >
            <small v-if="cluster.label">{{ cluster.label }}</small>
            <button
              v-for="section in cluster.sections"
              :key="pathKey(section.path)"
              class="config-nav-item"
              :class="{ active: pathKey(section.path) === selectedPath }"
              @click="selectedPath = pathKey(section.path)"
            >
              {{ configSectionTitle(section) }}
            </button>
            <div
              v-if="canManage && cluster.repeatable"
              class="provider-add"
            >
              <InputText
                v-model="providerNames[pathKey(cluster.repeatable.path)]"
                :aria-label="`New ${cluster.repeatable.label.toLowerCase()} name`"
                placeholder="Provider name"
                :disabled="managementDisabled"
                fluid
              />
              <Button
                label="Add provider"
                size="small"
                severity="secondary"
                :disabled="managementDisabled"
                @click="addProvider(cluster.repeatable)"
              />
            </div>
          </div>
        </div>
      </aside>
      <section
        v-if="selected"
        class="config-form stack"
      >
        <div class="config-section-heading">
          <div><h2>{{ configSectionTitle(selected) }}</h2><p><code>{{ displayPath(selected.path) }}</code></p></div>
          <Button
            v-if="canManage && selected.optional && selectedEnabled"
            :label="`Remove ${selected.label}`"
            severity="danger"
            text
            :disabled="busy"
            @click="removeOptionalSection(selected)"
          />
          <Button
            v-else-if="canManage && selected.optional"
            :label="`Add ${selected.label}`"
            severity="secondary"
            :disabled="busy"
            @click="addOptionalSection(selected)"
          />
          <Button
            v-else-if="canManage && selected.repeatable && selectedEnabled"
            :label="selected.present ? 'Remove provider' : 'Cancel provider add'"
            severity="danger"
            text
            :disabled="controlsDisabled"
            @click="removeProvider(selected)"
          />
        </div>
        <Message
          v-if="selected.optional && !selectedEnabled"
          severity="info"
          :closable="false"
        >
          Add this optional section before editing its settings.
        </Message>
        <Message
          v-else-if="selected.repeatable && !selectedEnabled"
          severity="info"
          :closable="false"
        >
          This provider is active until restart but is no longer present in the source. Add it again to edit it.
        </Message>
        <Message
          v-else
          severity="warn"
          :closable="false"
        >
          Changes in this section require a cosmobot restart.
        </Message>
        <fieldset
          v-for="option in selected.options"
          :key="pathKey(option.path)"
          class="config-option"
        >
          <legend>{{ option.label }}</legend>
          <p>{{ option.description }}</p>
          <label
            v-if="option.type.kind === 'boolean'"
            class="checkbox-row"
          >
            <Checkbox
              :model-value="Boolean(draftValue(option))"
              binary
              :disabled="controlsDisabled"
              @update:model-value="setOption(option, $event)"
            /> Enabled
          </label>
          <InputNumber
            v-else-if="option.type.kind === 'integer' || option.type.kind === 'number'"
            :model-value="numericValue(option)"
            :aria-label="option.label"
            :use-grouping="false"
            :min="numericConstraint(option, 'minimum')"
            :max="numericConstraint(option, 'maximum')"
            :max-fraction-digits="option.type.kind === 'integer' ? 0 : undefined"
            :disabled="controlsDisabled"
            fluid
            @update:model-value="setOption(option, $event)"
          />
          <select
            v-else-if="option.type.kind === 'enum'"
            class="config-select"
            :value="String(draftValue(option) ?? '')"
            :aria-label="option.label"
            :disabled="controlsDisabled"
            @change="setEnum(option, $event)"
          >
            <option
              v-for="choice in option.type.values"
              :key="choice"
              :value="choice"
            >
              {{ choice }}
            </option>
          </select>
          <ConfigIdentityInput
            v-else-if="option.type.kind === 'identity'"
            :model-value="identityValue(option)"
            :label="option.label"
            :disabled="controlsDisabled"
            @update:model-value="setOption(option, $event)"
          />
          <InputText
            v-else-if="option.type.kind === 'secret'"
            :model-value="inputValue(option)"
            :aria-label="option.label"
            type="password"
            autocomplete="new-password"
            placeholder="Leave blank to preserve"
            :disabled="controlsDisabled"
            fluid
            @update:model-value="replaceSecret(option, $event)"
          />
          <ConfigListInput
            v-else-if="option.type.kind === 'list' || option.type.kind === 'identity_list'"
            :model-value="listValue(option)"
            :item-kind="configListItemKind(option)"
            :label="option.label"
            :disabled="controlsDisabled"
            @update:model-value="setOption(option, $event)"
          />
          <InputText
            v-else
            :model-value="inputValue(option)"
            :aria-label="option.label"
            :disabled="controlsDisabled"
            fluid
            @update:model-value="setText(option, $event)"
          />
          <div class="config-values">
            <small>Source: <code>{{ displayValue(option.source.value) }}</code></small>
            <small>Effective: <code>{{ displayValue(option.effective) }}</code></small>
            <small>Default: <code>{{ displayValue(option.default) }}</code></small>
          </div>
          <div
            v-if="canManage"
            class="action-row"
          >
            <Button
              v-if="!option.required && (option.source.present || option.type.kind === 'secret')"
              :label="option.type.kind === 'secret' ? 'Clear secret' : 'Restore default'"
              size="small"
              severity="secondary"
              text
              :disabled="controlsDisabled"
              @click="removeOption(option)"
            />
            <Button
              v-if="drafts[pathKey(option.path)]"
              label="Undo draft"
              size="small"
              severity="secondary"
              text
              :disabled="controlsDisabled"
              @click="resetOption(option)"
            />
          </div>
        </fieldset>
        <Message
          v-if="validation"
          :severity="validation.valid ? 'success' : 'error'"
          :closable="false"
        >
          {{ validation.valid ? `${validation.diff.length} semantic change(s) validated.` : 'Validation failed.' }}
          <ul v-if="validation.diagnostics.length">
            <li
              v-for="diagnostic in validation.diagnostics"
              :key="`${pathKey(diagnostic.path)}:${diagnostic.code}`"
            >
              {{ displayPath(diagnostic.path) }}: {{ diagnostic.message }}
            </li>
          </ul>
          <ul v-else-if="validation.diff.length">
            <li
              v-for="change in validation.diff"
              :key="pathKey(change.path)"
            >
              <code>{{ displayPath(change.path) }}</code>: {{ displayValue(change.before) }} → {{ displayValue(change.after) }}
            </li>
          </ul>
        </Message>
        <div
          v-if="canManage"
          class="action-row"
        >
          <Button
            label="Validate"
            severity="secondary"
            :loading="validating"
            :disabled="changes.length === 0 || busy"
            @click="validate"
          />
          <Button
            label="Apply"
            :disabled="!applyReady || busy"
            :loading="loading"
            @click="apply"
          />
          <Button
            label="Discard drafts"
            severity="secondary"
            text
            :disabled="changes.length === 0 || busy"
            @click="clearDrafts"
          />
        </div>
      </section>
      <aside
        v-if="selected"
        class="config-meta"
      >
        <h2>Section details</h2>
        <dl class="detail-list">
          <div><dt>Revision</dt><dd><code>{{ snapshot?.revision.slice(0, 12) }}</code></dd></div>
          <div><dt>Active revision</dt><dd><code>{{ snapshot?.activeRevision.slice(0, 12) }}</code></dd></div>
          <div><dt>Activation</dt><dd>Restart</dd></div>
          <div><dt>Draft changes</dt><dd>{{ changes.length }}</dd></div>
        </dl>
      </aside>
    </div>
  </section>
</template>
