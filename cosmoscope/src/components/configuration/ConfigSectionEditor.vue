<script setup lang="ts">
import Button from 'primevue/button'
import Message from 'primevue/message'
import ConfigChangeSummary from './ConfigChangeSummary.vue'
import ConfigOptionEditor from './ConfigOptionEditor.vue'
import { configSectionTitle } from '@/configuration/navigation'
import { pathKey } from '@/composables/useConfigurationDraft'
import type { ConfigOption, ConfigSection, ConfigurationValidation } from '@/rpc/schemas'

defineProps<{
  section: ConfigSection
  enabled: boolean
  canManage: boolean
  busy: boolean
  controlsDisabled: boolean
  changesCount: number
  applyReady: boolean
  loading: boolean
  validating: boolean
  validation: ConfigurationValidation | undefined
  drafts: Record<string, unknown>
  value: (option: ConfigOption) => unknown
}>()
const emit = defineEmits<{
  addOptional: []
  removeOptional: []
  removeProvider: []
  updateOption: [option: ConfigOption, value: unknown]
  replaceSecret: [option: ConfigOption, value: string | undefined]
  removeOption: [option: ConfigOption]
  resetOption: [option: ConfigOption]
  validate: []
  apply: []
  clear: []
}>()
const displayPath = (path: readonly string[]): string => path.join('.')
</script>

<template>
  <section class="config-form stack">
    <div class="config-section-heading">
      <div><h2>{{ configSectionTitle(section) }}</h2><p><code>{{ displayPath(section.path) }}</code></p></div>
      <Button
        v-if="canManage && section.optional && enabled"
        :label="`Remove ${section.label}`"
        severity="danger"
        text
        :disabled="busy"
        @click="emit('removeOptional')"
      />
      <Button
        v-else-if="canManage && section.optional"
        :label="`Add ${section.label}`"
        severity="secondary"
        :disabled="busy"
        @click="emit('addOptional')"
      />
      <Button
        v-else-if="canManage && section.repeatable && enabled"
        :label="section.present ? 'Remove provider' : 'Cancel provider add'"
        severity="danger"
        text
        :disabled="controlsDisabled"
        @click="emit('removeProvider')"
      />
    </div>
    <Message
      v-if="section.optional && !enabled"
      severity="info"
      :closable="false"
    >
      Add this optional section before editing its settings.
    </Message>
    <Message
      v-else-if="section.repeatable && !enabled"
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
    <ConfigOptionEditor
      v-for="option in section.options"
      :key="pathKey(option.path)"
      :option="option"
      :value="value(option)"
      :disabled="controlsDisabled"
      :can-manage="canManage"
      :has-draft="drafts[pathKey(option.path)] !== undefined"
      @update="emit('updateOption', option, $event)"
      @replace-secret="emit('replaceSecret', option, $event)"
      @remove="emit('removeOption', option)"
      @reset="emit('resetOption', option)"
    />
    <ConfigChangeSummary :validation="validation" />
    <div
      v-if="canManage"
      class="action-row"
    >
      <Button
        label="Validate"
        severity="secondary"
        :loading="validating"
        :disabled="changesCount === 0 || busy"
        @click="emit('validate')"
      />
      <Button
        label="Apply"
        :disabled="!applyReady || busy"
        :loading="loading"
        @click="emit('apply')"
      />
      <Button
        label="Discard drafts"
        severity="secondary"
        text
        :disabled="changesCount === 0 || busy"
        @click="emit('clear')"
      />
    </div>
  </section>
</template>
