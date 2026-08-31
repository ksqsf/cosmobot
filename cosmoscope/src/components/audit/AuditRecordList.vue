<script setup lang="ts">
import Listbox from 'primevue/listbox'
import Tag from 'primevue/tag'
import RunIdLink from '@/components/RunIdLink.vue'
import { auditPresentation } from '@/backend/audit'
import type { AuditRecord } from '@/types/domain'

defineProps<{
  records: readonly AuditRecord[]
  selectedId: number | undefined
  total: number
  paused: boolean
  buffered: number
  platformLabel: (record: AuditRecord) => string
}>()
const emit = defineEmits<{ select: [id: number] }>()
const eventTime = (record: AuditRecord): string =>
  new Date(record.occurredAt).toLocaleTimeString(undefined, { hour12: false })
</script>

<template>
  <section
    class="panel audit-stream"
    aria-label="Audit events"
  >
    <div class="stream-heading">
      <span><i class="pulse" />{{ paused ? 'Rendering paused' : 'Receiving events' }}</span>
      <small>{{ records.length }} of {{ total }} events<span v-if="paused"> · {{ buffered }} buffered</span></small>
    </div>
    <Listbox
      :model-value="selectedId"
      :options="[...records]"
      option-value="id"
      data-key="id"
      aria-label="Audit events"
      class="audit-list"
      scroll-height="min(60vh, 600px)"
      @update:model-value="typeof $event === 'number' && emit('select', $event)"
    >
      <template #option="{ option }">
        <div class="audit-option">
          <time :datetime="option.occurredAt">{{ eventTime(option) }}</time>
          <Tag
            class="audit-type"
            :value="auditPresentation(option.event).kind"
            :severity="auditPresentation(option.event).tone"
          />
          <span>
            <strong>{{ auditPresentation(option.event).summary }}</strong>
            <small><RunIdLink :run-id="option.event.runId" /> · {{ platformLabel(option) }}</small>
          </span>
        </div>
      </template>
      <template #empty>
        No audit events match these filters.
      </template>
    </Listbox>
  </section>
</template>
