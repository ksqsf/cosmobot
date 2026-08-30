<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Checkbox from 'primevue/checkbox'
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import PageHeading from '@/components/PageHeading.vue'
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
const demo = ref(false)
const snapshot = ref<ConfigurationSnapshot>(demoSnapshot())
const selectedPath = ref('driver/telegram')
const drafts = ref<Record<string, ConfigChange>>({})
const sectionChanges = ref<ConfigChange[]>([])
const validation = ref<ConfigurationValidation>()
const providerNames = ref<Record<string, string>>({})

const sections = computed(() => snapshot.value.configuration.sections)
const selected = computed(() => sections.value.find((section) => pathKey(section.path) === selectedPath.value) ?? sections.value[0])
const changes = computed<ConfigChange[]>(() => [...Object.values(drafts.value), ...sectionChanges.value])
const canManage = computed(() => !demo.value && snapshot.value.editable &&
  ['config.validate', 'config.update'].every((method) => connection.methods.has(method)))
const canRollback = computed(() => canManage.value && snapshot.value.backup !== null && connection.methods.has('config.rollback'))
const canRestart = computed(() => !demo.value && connection.methods.has('admin.restart'))
const applyReady = computed(() => changes.value.length > 0 && validation.value?.valid === true)

function pathKey(path: readonly string[]): string { return path.join('/') }
function displayPath(path: readonly string[]): string { return path.join('.') }

function draftValue(option: ConfigOption): unknown {
  const draft = drafts.value[pathKey(option.path)]
  if (draft?.operation === 'set' || draft?.operation === 'replace_secret') return draft.value
  return option.source.present ? option.source.value : option.effective ?? option.default
}

function displayValue(value: unknown): string {
  if (value === null || value === undefined) return 'unset'
  if (Array.isArray(value)) return value.join(', ')
  if (typeof value === 'object') return JSON.stringify(value)
  return JSON.stringify(value)
}

function inputValue(option: ConfigOption): string {
  if (option.type.kind === 'secret') return ''
  return displayValue(draftValue(option)).replace(/^unset$/, '')
}

function setOption(option: ConfigOption, value: unknown): void {
  drafts.value = { ...drafts.value, [pathKey(option.path)]: { operation: 'set', path: option.path, value } }
  validation.value = undefined
}

function setText(option: ConfigOption, value: string | undefined): void {
  const text = value ?? ''
  const parsed = option.type.kind === 'list'
    ? text.split(',').map((part) => part.trim()).filter(Boolean)
    : option.type.kind === 'identity_list'
      ? text.split(',').map((part) => part.trim()).filter(Boolean).map(parseIdentity)
      : option.type.kind === 'identity' ? parseIdentity(text) : text
  setOption(option, parsed)
}

function parseIdentity(value: string): string | number {
  return /^-?\d+$/.test(value) ? Number(value) : value
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
  validation.value = undefined
}

function removeOption(option: ConfigOption): void {
  drafts.value = { ...drafts.value, [pathKey(option.path)]: {
    operation: option.type.kind === 'secret' ? 'clear_secret' : 'remove', path: option.path,
  } }
  validation.value = undefined
}

function resetOption(option: ConfigOption): void {
  const key = pathKey(option.path)
  drafts.value = Object.fromEntries(Object.entries(drafts.value).filter(([path]) => path !== key))
  validation.value = undefined
}

function addProvider(path: readonly string[]): void {
  const family = pathKey(path)
  const name = providerNames.value[family]?.trim()
  if (!name) return
  sectionChanges.value = [...sectionChanges.value, { operation: 'add_section', path: [...path, name] }]
  providerNames.value = { ...providerNames.value, [family]: '' }
  validation.value = undefined
}

function removeProvider(section: ConfigSection): void {
  sectionChanges.value = [...sectionChanges.value, { operation: 'remove_section', path: section.path }]
  validation.value = undefined
}

function clearDrafts(): void {
  drafts.value = {}
  sectionChanges.value = []
  validation.value = undefined
}

async function refresh(): Promise<void> {
  if (connection.state !== 'authenticated' || !connection.methods.has('config.get')) {
    demo.value = true
    snapshot.value = demoSnapshot()
    selectedPath.value = pathKey(snapshot.value.configuration.sections[0]?.path ?? [])
    clearDrafts()
    return
  }
  loading.value = true
  const result = await runBackend(getConfiguration)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  demo.value = false
  snapshot.value = result.value
  if (!sections.value.some((section) => pathKey(section.path) === selectedPath.value))
    selectedPath.value = pathKey(sections.value[0]?.path ?? [])
  clearDrafts()
}

async function validate(): Promise<void> {
  const result = await runBackend(validateConfiguration(snapshot.value.revision, changes.value))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  validation.value = result.value
  error.value = ''
}

async function apply(): Promise<void> {
  if (!applyReady.value) return
  loading.value = true
  const result = await runBackend(updateConfiguration(snapshot.value.revision, changes.value))
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; validation.value = undefined; return }
  snapshot.value = result.value
  clearDrafts()
  toast.add({ severity: 'success', summary: 'Configuration updated', detail: 'Restart to activate the changes.', life: 3500 })
}

function requestRollback(): void {
  const backup = snapshot.value.backup
  if (backup === null) return
  confirm.require({
    header: 'Roll back configuration?',
    message: 'Swap the current file with the previous valid configuration? A restart is still required.',
    rejectLabel: 'Cancel', acceptLabel: 'Roll back', acceptClass: 'p-button-danger',
    accept: () => { void rollback(backup.revision) },
  })
}

async function rollback(backupRevision: string): Promise<void> {
  const result = await runBackend(rollbackConfiguration(snapshot.value.revision, backupRevision))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  snapshot.value = result.value
  clearDrafts()
  toast.add({ severity: 'success', summary: 'Configuration rolled back', detail: 'Restart to activate it.', life: 3500 })
}

function requestRestart(): void {
  confirm.require({
    header: 'Restart cosmobot?', message: 'Active conversations may be interrupted while the service reconnects.',
    rejectLabel: 'Cancel', acceptLabel: 'Restart', acceptClass: 'p-button-danger',
    accept: () => { void restart() },
  })
}

async function restart(): Promise<void> {
  const result = await runBackend(restartCosmobot)
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  toast.add({ severity: 'info', summary: 'Restart requested', detail: 'The server acknowledged the request.', life: 3500 })
}

function demoSnapshot(): ConfigurationSnapshot {
  const options: ConfigOption[] = [
    {
      path: ['driver', 'telegram', 'enabled'], label: 'Enabled', description: 'Start the Telegram driver.',
      owner: 'Bot.Chat.Driver.Telegram.Config', type: { kind: 'boolean' }, required: false,
      default: true, constraints: {}, activation: 'restart', source: { present: true, value: true }, effective: true,
    },
    {
      path: ['driver', 'telegram', 'token'], label: 'Bot token', description: 'Telegram bot credential.',
      owner: 'Bot.Chat.Driver.Telegram.Config', type: { kind: 'secret' }, required: true,
      default: null, constraints: {}, activation: 'restart', source: { present: true, value: 'configured' }, effective: 'configured',
    },
    {
      path: ['driver', 'telegram', 'poll_timeout'], label: 'Polling timeout', description: 'Long-poll timeout in seconds.',
      owner: 'Bot.Chat.Driver.Telegram.Config', type: { kind: 'integer' }, required: false,
      default: 20, constraints: { minimum: 1 }, activation: 'restart', source: { present: true, value: 25 }, effective: 25,
    },
  ]
  return {
    schemaVersion: 1, revision: 'demo', activeRevision: 'demo', sourceState: 'valid', editable: false,
    diagnostics: [], configuration: { sections: [{ path: ['driver', 'telegram'], label: 'Driver / Telegram', repeatable: false, options }], repeatableSections: [] }, backup: null,
  }
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
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
          @click="refresh"
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
      v-if="demo"
      severity="info"
      :closable="false"
    >
      Demo configuration — connect to a server that advertises <code>config.get</code> to inspect live settings.
    </Message>
    <Message
      v-if="error"
      severity="error"
      closable
      @close="error = ''"
    >
      {{ error }}
    </Message>
    <Message
      v-if="snapshot.sourceState === 'invalid'"
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
        <p class="nav-label">
          {{ demo ? 'Demo sections' : 'Live sections' }}
        </p>
        <button
          v-for="section in sections"
          :key="pathKey(section.path)"
          class="config-nav-item"
          :class="{ active: pathKey(section.path) === selectedPath }"
          @click="selectedPath = pathKey(section.path)"
        >
          {{ section.label }}
        </button>
        <template v-if="canManage">
          <div
            v-for="repeatable in snapshot.configuration.repeatableSections"
            :key="pathKey(repeatable.path)"
            class="provider-add"
          >
            <small>{{ repeatable.label }}</small>
            <InputText
              v-model="providerNames[pathKey(repeatable.path)]"
              placeholder="Provider name"
              fluid
            />
            <Button
              label="Add"
              size="small"
              severity="secondary"
              @click="addProvider(repeatable.path)"
            />
          </div>
        </template>
      </aside>
      <section
        v-if="selected"
        class="config-form stack"
      >
        <div class="config-section-heading">
          <div><h2>{{ selected.label }}</h2><p><code>{{ displayPath(selected.path) }}</code></p></div>
          <Button
            v-if="canManage && selected.repeatable"
            label="Remove provider"
            severity="danger"
            text
            @click="removeProvider(selected)"
          />
        </div>
        <Message
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
              :disabled="!canManage"
              @update:model-value="setOption(option, $event)"
            /> Enabled
          </label>
          <InputNumber
            v-else-if="option.type.kind === 'integer' || option.type.kind === 'number'"
            :model-value="Number(draftValue(option))"
            :use-grouping="false"
            :disabled="!canManage"
            fluid
            @update:model-value="setOption(option, $event)"
          />
          <select
            v-else-if="option.type.kind === 'enum'"
            class="config-select"
            :value="String(draftValue(option) ?? '')"
            :disabled="!canManage"
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
          <InputText
            v-else-if="option.type.kind === 'secret'"
            :model-value="inputValue(option)"
            type="password"
            autocomplete="new-password"
            placeholder="Leave blank to preserve"
            :disabled="!canManage"
            fluid
            @update:model-value="replaceSecret(option, $event)"
          />
          <InputText
            v-else
            :model-value="inputValue(option)"
            :disabled="!canManage"
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
              @click="removeOption(option)"
            />
            <Button
              v-if="drafts[pathKey(option.path)]"
              label="Undo draft"
              size="small"
              severity="secondary"
              text
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
        </Message>
        <div
          v-if="canManage"
          class="action-row"
        >
          <Button
            label="Validate"
            severity="secondary"
            :disabled="changes.length === 0"
            @click="validate"
          />
          <Button
            label="Apply"
            :disabled="!applyReady"
            :loading="loading"
            @click="apply"
          />
          <Button
            label="Discard drafts"
            severity="secondary"
            text
            :disabled="changes.length === 0"
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
          <div><dt>Revision</dt><dd><code>{{ snapshot.revision.slice(0, 12) }}</code></dd></div>
          <div><dt>Active revision</dt><dd><code>{{ snapshot.activeRevision.slice(0, 12) }}</code></dd></div>
          <div><dt>Activation</dt><dd>Restart</dd></div>
          <div><dt>Draft changes</dt><dd>{{ changes.length }}</dd></div>
        </dl>
      </aside>
    </div>
  </section>
</template>
