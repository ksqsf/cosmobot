<script setup lang="ts">
import Button from 'primevue/button'
import Checkbox from 'primevue/checkbox'
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'
import ConfigIdentityInput from './ConfigIdentityInput.vue'
import ConfigListInput from './ConfigListInput.vue'
import { configListItemKind, configTextInputValue, displayConfigValue } from '@/configuration/values'
import type { ConfigOption } from '@/rpc/schemas'

const props = defineProps<{
  option: ConfigOption
  value: unknown
  disabled: boolean
  canManage: boolean
  hasDraft: boolean
}>()
const emit = defineEmits<{
  update: [value: unknown]
  replaceSecret: [value: string | undefined]
  remove: []
  reset: []
}>()

function numericValue(): number | null { return typeof props.value === 'number' ? props.value : null }
function listValue(): unknown[] { return Array.isArray(props.value) ? props.value : [] }
function identityValue(): string | number | null {
  return typeof props.value === 'string' || typeof props.value === 'number' ? props.value : null
}
function numericConstraint(key: 'minimum' | 'maximum'): number | undefined {
  if (typeof props.option.constraints !== 'object' || props.option.constraints === null) return undefined
  const value = (props.option.constraints as Record<string, unknown>)[key]
  return typeof value === 'number' ? value : undefined
}
</script>

<template>
  <fieldset class="config-option">
    <legend>{{ option.label }}</legend>
    <p>{{ option.description }}</p>
    <label
      v-if="option.type.kind === 'boolean'"
      class="checkbox-row"
    >
      <Checkbox
        :model-value="Boolean(value)"
        binary
        :disabled="disabled"
        @update:model-value="emit('update', $event)"
      /> Enabled
    </label>
    <InputNumber
      v-else-if="option.type.kind === 'integer' || option.type.kind === 'number'"
      :model-value="numericValue()"
      :aria-label="option.label"
      :use-grouping="false"
      :min="numericConstraint('minimum')"
      :max="numericConstraint('maximum')"
      :max-fraction-digits="option.type.kind === 'integer' ? 0 : undefined"
      :disabled="disabled"
      fluid
      @update:model-value="emit('update', $event)"
    />
    <select
      v-else-if="option.type.kind === 'enum'"
      class="config-select"
      :value="String(value ?? '')"
      :aria-label="option.label"
      :disabled="disabled"
      @change="emit('update', ($event.target as HTMLSelectElement).value)"
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
      :model-value="identityValue()"
      :label="option.label"
      :disabled="disabled"
      @update:model-value="emit('update', $event)"
    />
    <InputText
      v-else-if="option.type.kind === 'secret'"
      :model-value="configTextInputValue(value, option.type.kind)"
      :aria-label="option.label"
      type="password"
      autocomplete="new-password"
      placeholder="Leave blank to preserve"
      :disabled="disabled"
      fluid
      @update:model-value="emit('replaceSecret', $event)"
    />
    <ConfigListInput
      v-else-if="option.type.kind === 'list' || option.type.kind === 'identity_list'"
      :model-value="listValue()"
      :item-kind="configListItemKind(option)"
      :label="option.label"
      :disabled="disabled"
      @update:model-value="emit('update', $event)"
    />
    <InputText
      v-else
      :model-value="configTextInputValue(value, option.type.kind)"
      :aria-label="option.label"
      :disabled="disabled"
      fluid
      @update:model-value="emit('update', $event ?? '')"
    />
    <div class="config-values">
      <small>Source: <code>{{ displayConfigValue(option.source.value) }}</code></small>
      <small>Effective: <code>{{ displayConfigValue(option.effective) }}</code></small>
      <small>Default: <code>{{ displayConfigValue(option.default) }}</code></small>
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
        :disabled="disabled"
        @click="emit('remove')"
      />
      <Button
        v-if="hasDraft"
        label="Undo draft"
        size="small"
        severity="secondary"
        text
        :disabled="disabled"
        @click="emit('reset')"
      />
    </div>
  </fieldset>
</template>
