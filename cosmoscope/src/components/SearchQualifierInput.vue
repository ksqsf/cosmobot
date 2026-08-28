<script lang="ts">
export interface SearchQualifier {
  readonly prefix: string
  readonly icon: string
  readonly title: string
  readonly description: string
}
</script>

<script setup lang="ts">
import { ref } from 'vue'
import AutoComplete from 'primevue/autocomplete'

const props = defineProps<{
  readonly modelValue: string
  readonly qualifiers: readonly [SearchQualifier, ...SearchQualifier[]]
  readonly placeholder: string
  readonly inputLabel: string
}>()
const emit = defineEmits<{
  'update:modelValue': [value: string]
  'submit': [value: string]
}>()
const suggestions = ref<string[]>([])

function complete({ query }: { readonly query: string }): void {
  const separator = query.lastIndexOf(' ') + 1
  const before = query.slice(0, separator)
  const token = query.slice(separator).toLowerCase()
  suggestions.value = props.qualifiers
    .filter(({ prefix }) => prefix.toLowerCase().startsWith(token))
    .map(({ prefix }) => `${before}${prefix}`)
}

function qualifierFor(option: string): SearchQualifier {
  const token = option.slice(option.lastIndexOf(' ') + 1)
  return props.qualifiers.find(({ prefix }) => prefix === token) ?? props.qualifiers[0]
}
</script>

<template>
  <AutoComplete
    :model-value="props.modelValue"
    :suggestions="suggestions"
    :placeholder="props.placeholder"
    :aria-label="props.inputLabel"
    fluid
    complete-on-focus
    :min-length="0"
    @update:model-value="emit('update:modelValue', String($event))"
    @complete="complete"
    @keyup.enter="emit('submit', props.modelValue)"
  >
    <template #option="{ option }">
      <div class="qualifier-option">
        <span class="qualifier-icon"><i :class="qualifierFor(option).icon" /></span>
        <span><strong>{{ qualifierFor(option).prefix }}</strong><small>{{ qualifierFor(option).description }}</small></span>
        <kbd>{{ qualifierFor(option).title }}</kbd>
      </div>
    </template>
  </AutoComplete>
</template>

<style scoped>
.qualifier-option {
  display: grid;
  grid-template-columns: auto minmax(0, 1fr) auto;
  align-items: center;
  gap: .75rem;
  min-width: min(30rem, 72vw);
}

.qualifier-icon {
  display: grid;
  width: 2rem;
  height: 2rem;
  place-items: center;
  border: 1px solid var(--p-content-border-color);
  border-radius: .5rem;
  color: var(--p-primary-color);
  background: var(--p-content-background);
}

.qualifier-option span:nth-child(2) {
  display: grid;
  gap: .15rem;
}

.qualifier-option strong {
  font-family: monospace;
}

.qualifier-option small {
  color: var(--p-text-muted-color);
}

.qualifier-option kbd {
  padding: .15rem .4rem;
  border: 1px solid var(--p-content-border-color);
  border-radius: .35rem;
  color: var(--p-text-muted-color);
  background: var(--p-content-background);
  font: inherit;
  font-size: .75rem;
}
</style>
