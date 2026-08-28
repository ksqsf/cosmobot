<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import Button from 'primevue/button'
import Card from 'primevue/card'
import InputText from 'primevue/inputtext'
import Listbox from 'primevue/listbox'
import Message from 'primevue/message'
import Select from 'primevue/select'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import { getAudit, getAuditThread, recentAudit, subscribeAudit } from '@/backend/AdminBackend'
import { auditArguments, auditDetailFields, auditPlatform, auditPresentation, auditResult, boundedStructuredText, isAuditFailure, linkedThread, mergeAuditRecords } from '@/backend/audit'
import { runBackend } from '@/backend/runBackend'
import { useConnectionStore } from '@/stores/connection'
import type { AuditEvent, AuditPlatform, AuditRecord } from '@/types/domain'

type EventFilter = 'all' | 'tool' | 'model' | 'failure'
type PlatformFilter = 'all' | AuditPlatform | 'unlinked'
type PageState = 'loading' | 'ready' | 'unavailable' | 'error'

const loadedLimit = 200
const bufferedLimit = 100
const route = useRoute()
const router = useRouter()
const connection = useConnectionStore()
const query = ref('')
const platform = ref<PlatformFilter>('all')
const eventType = ref<EventFilter>('all')
const state = ref<PageState>('loading')
const error = ref('')
const events = ref<AuditRecord[]>([])
const buffered = ref<AuditRecord[]>([])
const paused = ref(false)
const selectedId = ref<number>()
const selectedDetail = ref<AuditRecord>()
const related = ref<AuditRecord[]>([])
const detailError = ref('')
const threadError = ref('')
let stopSubscription: (() => void) | undefined
let subscriptionGeneration = 0
let detailGeneration = 0

const platformOptions = [
  { label: 'All platforms', value: 'all' },
  { label: 'QQ', value: 'PlatformQQ' },
  { label: 'Telegram', value: 'PlatformTelegram' },
  { label: 'Matrix', value: 'PlatformMatrix' },
  { label: 'Discord', value: 'PlatformDiscord' },
  { label: 'RPC', value: 'PlatformRPC' },
  { label: 'ACP', value: 'PlatformACP' },
  { label: 'Unlinked', value: 'unlinked' },
] satisfies readonly { readonly label: string; readonly value: PlatformFilter }[]
const eventTypeOptions = [
  { label: 'All events', value: 'all' },
  { label: 'Tool calls', value: 'tool' },
  { label: 'Model turns', value: 'model' },
  { label: 'Failures', value: 'failure' },
] satisfies readonly { readonly label: string; readonly value: EventFilter }[]
const eventFilters = {
  all: () => true,
  tool: (event: AuditEvent) => auditPresentation(event).category === 'tool',
  model: (event: AuditEvent) => auditPresentation(event).category === 'model',
  failure: isAuditFailure,
} satisfies Record<EventFilter, (event: AuditEvent) => boolean>
const allKnownEvents = computed(() => mergeAuditRecords(events.value, buffered.value, loadedLimit + bufferedLimit))
const filteredEvents = computed(() => [...events.value].reverse().filter((record) => {
  const presentation = auditPresentation(record.event)
  const search = `${presentation.kind} ${presentation.summary} ${record.event.runId}`.toLowerCase()
  const eventPlatform = auditPlatform(allKnownEvents.value, record.event.runId)
  const platformMatches = platform.value === 'all'
    || platform.value === 'unlinked' && eventPlatform === undefined
    || platform.value === eventPlatform
  return search.includes(query.value.trim().toLowerCase()) && platformMatches && eventFilters[eventType.value](record.event)
}))
const selected = computed(() => selectedDetail.value ?? events.value.find(({ id }) => id === selectedId.value))
const selectedPresentation = computed(() => selected.value === undefined ? undefined : auditPresentation(selected.value.event))
const selectedArguments = computed(() => {
  const value = selected.value && auditArguments(selected.value.event)
  return value === undefined ? undefined : boundedStructuredText(value)
})
const selectedResult = computed(() => {
  const value = selected.value && auditResult(selected.value.event)
  return value === undefined ? undefined : boundedStructuredText(value)
})
const selectedFields = computed(() => selected.value === undefined ? [] : auditDetailFields(selected.value.event))
const requiredMethods = ['audit.recent', 'audit.get', 'audit.thread', 'audit.subscribe'] as const
const supportsAudit = computed(() => requiredMethods.every((method) => connection.methods.has(method)))

function eventTime(record: AuditRecord): string {
  return new Date(record.occurredAt).toLocaleTimeString(undefined, { hour12: false })
}

function platformLabel(record: AuditRecord): string {
  const value = auditPlatform(allKnownEvents.value, record.event.runId)
  return platformOptions.find((option) => option.value === value)?.label ?? 'Unlinked'
}

async function loadSnapshot(): Promise<void> {
  const result = await runBackend(recentAudit(loadedLimit))
  if (result._tag === 'Failure') {
    error.value = result.error.message
    state.value = 'error'
    return
  }
  error.value = ''
  if (paused.value) {
    const unseen = result.value.filter(({ id }) => !events.value.some((record) => record.id === id))
    buffered.value = mergeAuditRecords(buffered.value, unseen, bufferedLimit)
  } else {
    events.value = mergeAuditRecords([], result.value, loadedLimit)
  }
  state.value = 'ready'
  const requested = requestedAuditId()
  if (requested !== undefined) await loadSelection(requested)
  else if (selectedId.value === undefined && events.value.length > 0) selectAuditId(events.value.at(-1)?.id)
}

function receive(record: AuditRecord): void {
  if (paused.value) buffered.value = mergeAuditRecords(buffered.value, [record], bufferedLimit)
  else events.value = mergeAuditRecords(events.value, [record], loadedLimit)
}

async function installSubscription(): Promise<void> {
  const generation = ++subscriptionGeneration
  stopSubscription?.()
  stopSubscription = undefined
  if (connection.state === 'opening' || connection.state === 'reconnecting') {
    state.value = events.value.length === 0 ? 'loading' : 'ready'
    return
  }
  if (connection.state !== 'authenticated' || !supportsAudit.value) {
    error.value = connection.state === 'authenticated'
      ? 'The server does not provide every Audit RPC method required by this page.'
      : connection.error || 'Connect to cosmobot to load audit events.'
    state.value = events.value.length === 0 ? 'unavailable' : 'ready'
    return
  }
  state.value = events.value.length === 0 ? 'loading' : 'ready'
  const result = await runBackend(subscribeAudit(loadSnapshot, receive))
  if (generation !== subscriptionGeneration) {
    if (result._tag === 'Success') result.value()
    return
  }
  if (result._tag === 'Failure') {
    error.value = result.error.message
    state.value = 'error'
  } else {
    stopSubscription = result.value
  }
}

function requestedAuditId(): number | undefined {
  const raw = route.params['auditId']
  if (typeof raw !== 'string') return undefined
  const id = Number(raw)
  return Number.isSafeInteger(id) && id > 0 ? id : undefined
}

function selectAuditId(id: number | null | undefined): void {
  if (typeof id !== 'number' || !Number.isSafeInteger(id) || id < 1) return
  void loadSelection(id)
  void router.replace({ name: 'audit', params: { auditId: String(id) } })
}

async function loadSelection(id: number): Promise<void> {
  selectedId.value = id
  selectedDetail.value = undefined
  related.value = []
  detailError.value = ''
  threadError.value = ''
  const generation = ++detailGeneration
  const result = await runBackend(getAudit(id))
  if (generation !== detailGeneration) return
  if (result._tag === 'Failure') {
    detailError.value = result.error.message
    return
  }
  if (result.value === null) {
    detailError.value = `Audit event #${String(id)} was not found.`
    return
  }
  selectedDetail.value = result.value
  const key = linkedThread([...events.value, result.value], result.value.event.runId)
  if (key === undefined) {
    related.value = events.value.filter(({ event }) => event.runId === result.value?.event.runId)
    return
  }
  const threadResult = await runBackend(getAuditThread(key))
  if (generation !== detailGeneration) return
  if (threadResult._tag === 'Success') related.value = [...threadResult.value]
  else threadError.value = threadResult.error.message
}

function resume(): void {
  events.value = mergeAuditRecords(events.value, buffered.value, loadedLimit)
  buffered.value = []
  paused.value = false
}

function discard(): void {
  buffered.value = []
  paused.value = false
}

watch(() => route.params['auditId'], () => {
  const id = requestedAuditId()
  if (id !== undefined && id !== selectedId.value) void loadSelection(id)
})
watch([() => connection.state, () => connection.methods], () => { void installSubscription() })
onMounted(() => { void installSubscription() })
onUnmounted(() => { subscriptionGeneration += 1; stopSubscription?.(); detailGeneration += 1 })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Observability"
      title="Audit timeline"
      description="Follow agent decisions and tool activity as they happen."
    >
      <Button
        v-if="state === 'ready' && !paused"
        label="Pause"
        icon="pi pi-pause"
        severity="secondary"
        @click="paused = true"
      />
      <template v-else-if="state === 'ready' && paused">
        <Button
          :label="`Resume (${String(buffered.length)})`"
          icon="pi pi-play"
          @click="resume"
        />
        <Button
          label="Discard"
          severity="secondary"
          @click="discard"
        />
      </template>
    </PageHeading>

    <Message
      v-if="state === 'unavailable'"
      severity="error"
      :closable="false"
    >
      {{ error }}
    </Message>
    <Message
      v-else-if="state === 'error'"
      severity="error"
      :closable="false"
    >
      {{ error }}
      <Button
        label="Retry"
        size="small"
        text
        @click="installSubscription"
      />
    </Message>
    <article
      v-else-if="state === 'loading'"
      class="panel manager-loading"
      aria-label="Loading audit events"
    >
      <Skeleton height="3rem" /><Skeleton height="22rem" />
    </article>
    <template v-else>
      <p
        class="sr-only"
        aria-live="polite"
      >
        {{ paused ? `${String(buffered.length)} audit events buffered` : 'Audit stream receiving events' }}
      </p>
      <div class="filter-bar panel">
        <InputText
          v-model="query"
          placeholder="Search events, runs, or tools"
          aria-label="Search audit events"
          fluid
        />
        <Select
          v-model="platform"
          :options="platformOptions"
          option-label="label"
          option-value="value"
          aria-label="Platform"
          size="small"
        />
        <Select
          v-model="eventType"
          :options="eventTypeOptions"
          option-label="label"
          option-value="value"
          aria-label="Event type"
          size="small"
        />
      </div>
      <Message
        v-if="connection.state !== 'authenticated'"
        severity="warn"
        :closable="false"
      >
        Connection lost. Showing the last audit snapshot while cosmobot reconnects.
      </Message>
      <div class="audit-layout">
        <section
          class="panel audit-stream"
          aria-label="Audit events"
        >
          <div class="stream-heading">
            <span><i class="pulse" />{{ paused ? 'Rendering paused' : 'Receiving events' }}</span>
            <small>{{ filteredEvents.length }} of {{ events.length }} events<span v-if="paused"> · {{ buffered.length }} buffered</span></small>
          </div>
          <Listbox
            v-model="selectedId"
            :options="filteredEvents"
            option-value="id"
            data-key="id"
            aria-label="Audit events"
            class="audit-list"
            scroll-height="min(60vh, 600px)"
            @change="selectAuditId(selectedId)"
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
                  <small>{{ option.event.runId }} · {{ platformLabel(option) }}</small>
                </span>
              </div>
            </template>
            <template #empty>
              No audit events match these filters.
            </template>
          </Listbox>
        </section>

        <Message
          v-if="detailError"
          severity="error"
          :closable="false"
        >
          {{ detailError }}
        </Message>
        <Card
          v-else-if="selected && selectedPresentation"
          class="inspector audit-inspector"
        >
          <template #title>
            {{ selectedPresentation.kind }}
          </template>
          <template #subtitle>
            Audit event #{{ selected.id }}
          </template>
          <template #content>
            <dl class="detail-list">
              <div><dt>Recorded</dt><dd>{{ new Date(selected.occurredAt).toLocaleString() }}</dd></div>
              <div><dt>Type</dt><dd><code>{{ selected.event.tag }}</code></dd></div>
              <div
                v-for="field in selectedFields"
                :key="field.label"
              >
                <dt>{{ field.label }}</dt><dd>{{ field.value }}</dd>
              </div>
            </dl>
            <template v-if="selectedArguments">
              <h3>Arguments</h3>
              <pre><code>{{ selectedArguments.text }}</code></pre>
              <small
                v-if="selectedArguments.truncated"
                class="bounded-note"
              >Preview limited to 12,000 characters.</small>
            </template>
            <template v-if="selectedResult">
              <h3>Result</h3>
              <pre><code>{{ selectedResult.text }}</code></pre>
              <small
                v-if="selectedResult.truncated"
                class="bounded-note"
              >Preview limited to 12,000 characters.</small>
            </template>
            <template v-if="related.length > 1">
              <h3>Related thread</h3>
              <ol class="mini-timeline audit-related">
                <li
                  v-for="record in related"
                  :key="record.id"
                >
                  <strong>{{ auditPresentation(record.event).kind }}</strong>
                  <small>{{ eventTime(record) }} · #{{ record.id }}</small>
                </li>
              </ol>
            </template>
            <Message
              v-if="threadError"
              severity="warn"
              :closable="false"
            >
              {{ threadError }}
            </Message>
          </template>
        </Card>
        <Card
          v-else
          class="inspector audit-inspector"
        >
          <template #content>
            Select an audit event to inspect its structured details.
          </template>
        </Card>
      </div>
    </template>
  </section>
</template>
