<script setup lang="ts">
import { ref, watch } from 'vue'
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
const identityKinds = ref<('text' | 'number' | undefined)[]>([])

watch(() => props.modelValue, () => {
  const next = identityKinds.value.slice(0, items().length)
  items().forEach((item, index) => {
    if (item !== null && item !== undefined) next[index] = typeof item === 'number' ? 'number' : 'text'
  })
  identityKinds.value = next
}, { deep: true })

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
  identityKinds.value[index] = numeric ? 'number' : 'text'
  const converted = text.trim() === '' ? Number.NaN : Number(text)
  update(index, numeric ? (Number.isInteger(converted) ? converted : null) : text)
}

function identityKind(index: number, item: unknown): 'text' | 'number' {
  return identityKinds.value[index] ?? (typeof item === 'number' ? 'number' : 'text')
}

function remove(index: number): void {
  identityKinds.value.splice(index, 1)
  emit('update:modelValue', items().filter((_, itemIndex) => itemIndex !== index))
}

function add(): void {
  identityKinds.value.push(props.itemKind === 'identity' ? 'text' : undefined)
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
          :value="identityKind(index, item)"
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
          v-if="identityKind(index, item) === 'number'"
          :model-value="typeof item === 'number' ? item : null"
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
