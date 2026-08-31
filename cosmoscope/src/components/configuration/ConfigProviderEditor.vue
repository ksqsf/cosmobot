<script setup lang="ts">
import Button from 'primevue/button'
import InputText from 'primevue/inputtext'
import type { ConfigurationSnapshot } from '@/rpc/schemas'

type RepeatableSection = ConfigurationSnapshot['configuration']['repeatableSections'][number]
defineProps<{ template: RepeatableSection, modelValue: string | undefined, disabled: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [value: string | undefined], add: [] }>()
</script>

<template>
  <div class="provider-add">
    <InputText
      :model-value="modelValue"
      :aria-label="`New ${template.label.toLowerCase()} name`"
      placeholder="Provider name"
      :disabled="disabled"
      fluid
      @update:model-value="emit('update:modelValue', $event)"
    />
    <Button
      label="Add provider"
      size="small"
      severity="secondary"
      :disabled="disabled"
      @click="emit('add')"
    />
  </div>
</template>
