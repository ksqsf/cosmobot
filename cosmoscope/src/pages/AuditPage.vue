<script setup lang="ts">
import { computed, ref } from 'vue'
import Button from 'primevue/button'
import Card from 'primevue/card'
import InputText from 'primevue/inputtext'
import Listbox from 'primevue/listbox'
import Select from 'primevue/select'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'

const paused = ref(false)
const query = ref('')
const platform = ref('All platforms')
const eventType = ref('All events')
const timeRange = ref('1 hour')
interface AuditEvent {
  readonly id: string
  readonly time: string
  readonly type: string
  readonly text: string
  readonly meta: string
  readonly tone: 'info' | 'success' | 'secondary' | 'danger'
}
const initialEvent: AuditEvent = { id: '84291', time: '14:32:08', type: 'tool.call', text: 'Agent called query_chat_log', meta: 'run_b41d · turn 3 · Telegram', tone: 'info' }
const events: readonly AuditEvent[] = [
  initialEvent,
  { id: '84292', time: '14:32:08', type: 'tool.result', text: 'Returned 18 chat messages', meta: 'run_b41d · call_7a2 · 6.4 KB', tone: 'success' },
  { id: '84293', time: '14:31:54', type: 'model.request', text: 'Sent 24 transcript messages', meta: 'run_b41d · turn 3 · gpt-5.4', tone: 'secondary' },
  { id: '84294', time: '14:30:11', type: 'tool.failure', text: 'web_fetch exceeded its timeout', meta: 'run_2e09 · call_d88 · Discord', tone: 'danger' },
]
const selected = ref<AuditEvent>(initialEvent)
const filteredEvents = computed(() => events.filter((item) =>
  `${item.type} ${item.text}`.toLowerCase().includes(query.value.toLowerCase()),
))
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Observability"
      title="Audit timeline"
      description="Follow agent decisions and tool activity as they happen."
    >
      <Button
        :label="paused ? 'Resume' : 'Pause'"
        :icon="paused ? 'pi pi-play' : 'pi pi-pause'"
        severity="secondary"
        @click="paused = !paused"
      /><Button label="Export view" />
    </PageHeading><div class="filter-bar panel">
      <InputText
        v-model="query"
        placeholder="Search events, runs, tools, or messages"
        fluid
      /><Select
        v-model="platform"
        :options="['All platforms', 'RPC', 'Telegram', 'Discord']"
        aria-label="Platform"
        size="small"
      /><Select
        v-model="eventType"
        :options="['All events', 'Tool calls', 'Model turns', 'Failures']"
        aria-label="Event type"
        size="small"
      /><Select
        v-model="timeRange"
        :options="['15 minutes', '1 hour', '24 hours']"
        aria-label="Time range"
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
      </Listbox><Card class="inspector">
        <template #title>
          Audit event #{{ selected.id }}
        </template><template #content>
          <div class="stack stack-tight">
            <dl class="detail-list">
              <div><dt>Recorded</dt><dd>{{ selected.time }}</dd></div><div><dt>Type</dt><dd><code>{{ selected.type }}</code></dd></div><div><dt>Run</dt><dd><code>run_b41d</code></dd></div>
            </dl><h3>Arguments</h3><pre>{ "scope": "current_chat", "limit": 20 }</pre><h3>Result</h3><Tag
              value="Success"
              severity="success"
            />
          </div>
        </template>
      </Card>
    </div>
  </section>
</template>
