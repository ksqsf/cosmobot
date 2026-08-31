<script setup lang="ts">
import FloatLabel from 'primevue/floatlabel'
import MultiSelect from 'primevue/multiselect'
import SearchQualifierInput from '@/components/SearchQualifierInput.vue'
import {
  auditEventTypeOptions, auditPlatformOptions, type AuditEventFilter, type AuditPlatformFilter,
} from '@/composables/useAuditStream'

defineProps<{ query: string, platforms: AuditPlatformFilter[], eventTypes: AuditEventFilter[] }>()
const emit = defineEmits<{
  'update:query': [value: string]
  'update:platforms': [value: AuditPlatformFilter[]]
  'update:eventTypes': [value: AuditEventFilter[]]
  submit: [value: string]
}>()
const qualifiers = [
  { prefix: 'thread:', icon: 'pi pi-sitemap', title: 'Thread', description: 'Show every run linked to a thread' },
  { prefix: 'run:', icon: 'pi pi-sparkles', title: 'Agent run', description: 'Show events from one agent run' },
] as const
</script>

<template>
  <div class="filter-bar panel">
    <SearchQualifierInput
      :model-value="query"
      :qualifiers="qualifiers"
      placeholder="Search or use thread:42"
      input-label="Search audit events"
      @update:model-value="emit('update:query', $event)"
      @submit="emit('submit', $event)"
    />
    <FloatLabel variant="on">
      <MultiSelect
        :model-value="platforms"
        :options="[...auditPlatformOptions]"
        input-id="audit-platform-filter"
        option-label="label"
        option-value="value"
        display="chip"
        placeholder="All"
        @update:model-value="emit('update:platforms', $event)"
      />
      <label for="audit-platform-filter">Platform</label>
    </FloatLabel>
    <FloatLabel variant="on">
      <MultiSelect
        :model-value="eventTypes"
        :options="[...auditEventTypeOptions]"
        input-id="audit-event-filter"
        option-label="label"
        option-value="value"
        display="chip"
        placeholder="All"
        @update:model-value="emit('update:eventTypes', $event)"
      />
      <label for="audit-event-filter">Event type</label>
    </FloatLabel>
  </div>
</template>
