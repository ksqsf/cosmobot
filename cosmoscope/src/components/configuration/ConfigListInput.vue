<script setup lang="ts">
import Button from 'primevue/button'
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'

const props = defineProps<{
  modelValue: unknown
  itemKind: 'text' | 'integer' | 'identity'
  label: string
  disabled: boolean
}>()
const emit = defineEmits<{ 'update:modelValue': [value: unknown[]] }>()

function items(): unknown[] {
  return Array.isArray(props.modelValue) ? props.modelValue : []
}

function update(index: number, value: unknown): void {
  const next = [...items()]
  next[index] = value
  emit('update:modelValue', next)
}

function changeIdentityType(index: number, event: Event): void {
  const current = items()[index]
  const numeric = (event.target as HTMLSelectElement).value === 'number'
  const text = typeof current === 'string' || typeof current === 'number' ? String(current) : ''
  update(index, numeric ? (typeof current === 'number' ? current : Number(text) || 0) : text)
}

function remove(index: number): void {
  emit('update:modelValue', items().filter((_, itemIndex) => itemIndex !== index))
}

function add(): void {
  emit('update:modelValue', [...items(), null])
}
</script>

<template>
  <div class="config-list-editor">
    <div
      v-for="(item, index) in items()"
      :key="index"
      class="config-list-row"
    >
      <InputNumber
        v-if="itemKind === 'integer'"
        :model-value="typeof item === 'number' ? item : null"
        :aria-label="`${label} item ${index + 1}`"
        :use-grouping="false"
        :max-fraction-digits="0"
        :disabled="disabled"
        fluid
        @update:model-value="update(index, $event)"
      />
      <div
        v-else-if="itemKind === 'identity'"
        class="config-identity-editor"
      >
        <select
          class="config-select"
          :value="typeof item === 'number' ? 'number' : 'text'"
          :aria-label="`${label} item ${index + 1} type`"
          :disabled="disabled"
          @change="changeIdentityType(index, $event)"
        >
          <option value="text">
            Text
          </option>
          <option value="number">
            Number
          </option>
        </select>
        <InputNumber
          v-if="typeof item === 'number'"
          :model-value="item"
          :aria-label="`${label} item ${index + 1}`"
          :use-grouping="false"
          :max-fraction-digits="0"
          :disabled="disabled"
          fluid
          @update:model-value="update(index, $event)"
        />
        <InputText
          v-else
          :model-value="typeof item === 'string' ? item : ''"
          :aria-label="`${label} item ${index + 1}`"
          :disabled="disabled"
          fluid
          @update:model-value="update(index, $event ?? '')"
        />
      </div>
      <InputText
        v-else
        :model-value="String(item ?? '')"
        :aria-label="`${label} item ${index + 1}`"
        :disabled="disabled"
        fluid
        @update:model-value="update(index, $event)"
      />
      <Button
        icon="pi pi-times"
        :aria-label="`Remove ${label} item ${index + 1}`"
        severity="secondary"
        text
        :disabled="disabled"
        @click="remove(index)"
      />
    </div>
    <small v-if="items().length === 0">No entries.</small>
    <Button
      label="Add entry"
      icon="pi pi-plus"
      size="small"
      severity="secondary"
      outlined
      :disabled="disabled"
      @click="add"
    />
  </div>
</template>
