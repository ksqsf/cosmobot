<script setup lang="ts">
import Select from 'primevue/select'
import SearchQualifierInput from '@/components/SearchQualifierInput.vue'
import {
  auditEventTypeOptions, auditPlatformOptions, type AuditEventFilter, type AuditPlatformFilter,
} from '@/composables/useAuditStream'

defineProps<{ query: string, platform: AuditPlatformFilter, eventType: AuditEventFilter }>()
const emit = defineEmits<{
  'update:query': [value: string]
  'update:platform': [value: AuditPlatformFilter]
  'update:eventType': [value: AuditEventFilter]
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
    <Select
      :model-value="platform"
      :options="[...auditPlatformOptions]"
      option-label="label"
      option-value="value"
      aria-label="Platform"
      size="small"
      @update:model-value="emit('update:platform', $event)"
    />
    <Select
      :model-value="eventType"
      :options="[...auditEventTypeOptions]"
      option-label="label"
      option-value="value"
      aria-label="Event type"
      size="small"
      @update:model-value="emit('update:eventType', $event)"
    />
  </div>
</template>
