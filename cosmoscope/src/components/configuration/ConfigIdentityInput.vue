<script setup lang="ts">
import { ref, watch } from 'vue'
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'

const props = defineProps<{
  modelValue: string | number | null
  label: string
  disabled: boolean
}>()
const emit = defineEmits<{ 'update:modelValue': [value: string | number | null] }>()
const identityKind = ref<'text' | 'number'>(typeof props.modelValue === 'number' ? 'number' : 'text')

watch(() => props.modelValue, (value) => {
  if (value !== null) identityKind.value = typeof value === 'number' ? 'number' : 'text'
})

function changeType(event: Event): void {
  const numeric = (event.target as HTMLSelectElement).value === 'number'
  identityKind.value = numeric ? 'number' : 'text'
  const converted = typeof props.modelValue === 'string' && props.modelValue.trim() !== '' ? Number(props.modelValue) : props.modelValue
  if (numeric) emit('update:modelValue', typeof converted === 'number' && Number.isInteger(converted) ? converted : null)
  else emit('update:modelValue', props.modelValue === null ? '' : String(props.modelValue))
}
</script>

<template>
  <div class="config-identity-editor">
    <select
      class="config-select"
      :value="identityKind"
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
      v-if="identityKind === 'number'"
      :model-value="typeof modelValue === 'number' ? modelValue : null"
      :aria-label="label"
      :use-grouping="false"
      :max-fraction-digits="0"
      :disabled="disabled"
      fluid
      @update:model-value="emit('update:modelValue', $event)"
    />
    <InputText
      v-else
      :model-value="modelValue === null ? '' : String(modelValue)"
      :aria-label="label"
      :disabled="disabled"
      fluid
      @update:model-value="emit('update:modelValue', $event ?? '')"
    />
  </div>
</template>
