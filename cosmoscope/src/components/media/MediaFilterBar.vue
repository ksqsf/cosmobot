<script setup lang="ts">
import Button from 'primevue/button'
import FloatLabel from 'primevue/floatlabel'
import InputText from 'primevue/inputtext'
import MultiSelect from 'primevue/multiselect'
import type { MediaItem, MediaSourceKind } from '@/types/domain'

defineProps<{
  platformOptions: { label: string; value: string }[]
  mimeTypeOptions: string[]
  sourceKindOptions: { label: string; value: MediaSourceKind }[]
  selection: readonly MediaItem[]
  disabled: boolean
}>()
const emit = defineEmits<{ delete: [] }>()
const query = defineModel<string>('query', { required: true })
const sourceKinds = defineModel<MediaSourceKind[]>('sourceKinds', { required: true })
const platforms = defineModel<string[]>('platforms', { required: true })
const mimeTypes = defineModel<string[]>('mimeTypes', { required: true })
</script>

<template>
  <div class="table-toolbar resource-toolbar">
    <InputText
      v-model="query"
      placeholder="Search all media"
      aria-label="Search all media"
    />
    <div>
      <FloatLabel variant="on">
        <MultiSelect
          v-model="sourceKinds"
          :options="sourceKindOptions"
          input-id="media-source-filter"
          option-label="label"
          option-value="value"
          display="chip"
          placeholder="All"
        />
        <label for="media-source-filter">Source</label>
      </FloatLabel>
      <FloatLabel variant="on">
        <MultiSelect
          v-model="platforms"
          :options="platformOptions"
          input-id="media-platform-filter"
          option-label="label"
          option-value="value"
          display="chip"
          placeholder="All"
        />
        <label for="media-platform-filter">Platform</label>
      </FloatLabel>
      <FloatLabel variant="on">
        <MultiSelect
          v-model="mimeTypes"
          :options="mimeTypeOptions"
          input-id="media-type-filter"
          display="chip"
          placeholder="All"
        />
        <label for="media-type-filter">Type</label>
      </FloatLabel>
      <Button
        label="Delete selected"
        icon="pi pi-trash"
        severity="danger"
        outlined
        :disabled="selection.length === 0 || disabled"
        @click="emit('delete')"
      />
    </div>
  </div>
</template>
