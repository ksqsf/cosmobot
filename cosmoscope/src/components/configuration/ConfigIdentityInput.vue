<script setup lang="ts">
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'

const props = defineProps<{
  modelValue: string | number | null
  label: string
  disabled: boolean
}>()
const emit = defineEmits<{ 'update:modelValue': [value: string | number | null] }>()

function changeType(event: Event): void {
  const numeric = (event.target as HTMLSelectElement).value === 'number'
  if (numeric) emit('update:modelValue', typeof props.modelValue === 'number' ? props.modelValue : Number(props.modelValue) || 0)
  else emit('update:modelValue', props.modelValue === null ? '' : String(props.modelValue))
}
</script>

<template>
  <div class="config-identity-editor">
    <select
      class="config-select"
      :value="typeof modelValue === 'number' ? 'number' : 'text'"
      :aria-label="`${label} type`"
      :disabled="disabled"
      @change="changeType"
    >
      <option value="text">
        Text
      </option>
      <option value="number">
        Number
      </option>
    </select>
    <InputNumber
      v-if="typeof modelValue === 'number'"
      :model-value="modelValue"
      :aria-label="label"
      :use-grouping="false"
      :max-fraction-digits="0"
      :disabled="disabled"
      fluid
      @update:model-value="emit('update:modelValue', $event)"
    />
    <InputText
      v-else
      :model-value="modelValue ?? ''"
      :aria-label="label"
      :disabled="disabled"
      fluid
      @update:model-value="emit('update:modelValue', $event ?? '')"
    />
  </div>
</template>
