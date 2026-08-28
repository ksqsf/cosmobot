<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import Card from 'primevue/card'
import InputText from 'primevue/inputtext'
import Listbox from 'primevue/listbox'
import Message from 'primevue/message'
import Select from 'primevue/select'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'

const route = useRoute()
const query = ref('')
type PlatformFilter = 'all' | 'Telegram' | 'Discord'
type EventFilter = 'all' | 'tool' | 'model' | 'failure'
const platform = ref<PlatformFilter>('all')
const eventType = ref<EventFilter>('all')
interface AuditEvent {
  readonly id: string
  readonly time: string
  readonly type: string
  readonly text: string
  readonly meta: string
  readonly tone: 'info' | 'success' | 'secondary' | 'danger'
  readonly platform: Exclude<PlatformFilter, 'all'>
  readonly category: Exclude<EventFilter, 'all' | 'failure'>
}
const platformOptions = [
  { label: 'All platforms', value: 'all' },
  { label: 'Telegram', value: 'Telegram' },
  { label: 'Discord', value: 'Discord' },
] satisfies readonly { label: string; value: PlatformFilter }[]
const eventTypeOptions = [
  { label: 'All events', value: 'all' },
  { label: 'Tool calls', value: 'tool' },
  { label: 'Model turns', value: 'model' },
  { label: 'Failures', value: 'failure' },
] satisfies readonly { label: string; value: EventFilter }[]
const initialEvent: AuditEvent = { id: '84291', time: '14:32:08', type: 'tool.call', text: 'Agent called query_chat_log', meta: 'run_b41d · turn 3 · Telegram', tone: 'info', platform: 'Telegram', category: 'tool' }
const events: readonly AuditEvent[] = [
  initialEvent,
  { id: '84292', time: '14:32:08', type: 'tool.result', text: 'Returned 18 chat messages', meta: 'run_b41d · call_7a2 · 6.4 KB', tone: 'success', platform: 'Telegram', category: 'tool' },
  { id: '84293', time: '14:31:54', type: 'model.request', text: 'Sent 24 transcript messages', meta: 'run_b41d · turn 3 · gpt-5.4', tone: 'secondary', platform: 'Telegram', category: 'model' },
  { id: '84294', time: '14:30:11', type: 'tool.failure', text: 'web_fetch exceeded its timeout', meta: 'run_2e09 · call_d88 · Discord', tone: 'danger', platform: 'Discord', category: 'tool' },
]
const eventFilters = {
  all: () => true,
  tool: (event: AuditEvent) => event.category === 'tool',
  model: (event: AuditEvent) => event.category === 'model',
  failure: (event: AuditEvent) => event.tone === 'danger',
} satisfies Record<EventFilter, (event: AuditEvent) => boolean>
const requestedAuditId = computed(() => typeof route.params['auditId'] === 'string' ? route.params['auditId'] : undefined)
const selected = ref<AuditEvent>(events.find(({ id }) => id === requestedAuditId.value) ?? initialEvent)
const missingAuditDetail = computed(() => requestedAuditId.value !== undefined && !events.some(({ id }) => id === requestedAuditId.value))
const filteredEvents = computed(() => events.filter((item) =>
  `${item.type} ${item.text}`.toLowerCase().includes(query.value.toLowerCase())
  && (platform.value === 'all' || item.platform === platform.value)
  && eventFilters[eventType.value](item),
))
watch(requestedAuditId, (id) => {
  if (id !== undefined) selected.value = events.find((event) => event.id === id) ?? selected.value
})
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Observability"
      title="Audit timeline"
      description="Follow agent decisions and tool activity as they happen."
    /><div class="filter-bar panel">
      <InputText
        v-model="query"
        placeholder="Search events, runs, tools, or messages"
        fluid
      /><Select
        v-model="platform"
        :options="platformOptions"
        option-label="label"
        option-value="value"
        aria-label="Platform"
        size="small"
      /><Select
        v-model="eventType"
        :options="eventTypeOptions"
        option-label="label"
        option-value="value"
        aria-label="Event type"
        size="small"
      />
    </div><div class="audit-layout">
      <Listbox
        v-model="selected"
        :options="filteredEvents"
        option-label="text"
        data-key="id"
        aria-label="Audit events"
        class="audit-list"
      >
        <template #option="{ option }">
          <div class="audit-option">
            <time>{{ option.time }}</time><Tag
              class="audit-type"
              :value="option.type"
              :severity="option.tone"
            /><span><strong>{{ option.text }}</strong><small>{{ option.meta }}</small></span>
          </div>
        </template>
      </Listbox><Message
        v-if="missingAuditDetail"
        severity="secondary"
        :closable="false"
      >
        Live audit detail is not available until Phase 5.
      </Message><Card
        v-else
        class="inspector"
      >
        <template #title>
          Audit event #{{ selected.id }}
        </template><template #content>
          <div class="stack stack-tight">
            <dl class="detail-list">
              <div><dt>Recorded</dt><dd>{{ selected.time }}</dd></div><div><dt>Type</dt><dd><code>{{ selected.type }}</code></dd></div><div><dt>Platform</dt><dd>{{ selected.platform }}</dd></div><div><dt>Context</dt><dd>{{ selected.meta }}</dd></div><div><dt>Summary</dt><dd>{{ selected.text }}</dd></div><div>
                <dt>Tone</dt><dd>
                  <Tag
                    :value="selected.tone"
                    :severity="selected.tone"
                  />
                </dd>
              </div>
            </dl>
          </div>
        </template>
      </Card>
    </div>
  </section>
</template>
