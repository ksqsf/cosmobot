<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import Button from 'primevue/button'
import Card from 'primevue/card'
import Listbox from 'primevue/listbox'
import Message from 'primevue/message'
import Select from 'primevue/select'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import ChatLogMessageLink from '@/components/ChatLogMessageLink.vue'
import RunIdLink from '@/components/RunIdLink.vue'
import SearchQualifierInput from '@/components/SearchQualifierInput.vue'
import { getAudit, getRunAudit, getThreadAudit, recentAudit, resolveThreadRun, RpcBackendError, searchAudit, subscribeAudit } from '@/backend/AdminBackend'
import type { BackendError } from '@/backend/AdminBackend'
import { auditArguments, auditDetailFields, auditPlatform, auditPresentation, auditResult, boundedStructuredText, isAuditFailure, mergeAuditRecords, parseAuditSearch } from '@/backend/audit'
import { runBackend } from '@/backend/runBackend'
import type { BackendResult } from '@/backend/runBackend'
import { useLatest, useLatestSubscription } from '@/async'
import { mediaRefFromClick, renderMarkdown } from '@/markdown'
import { useConnectionStore } from '@/stores/connection'
import type { AuditEvent, AuditPlatform, AuditRecord, ThreadMessageKey } from '@/types/domain'

type EventFilter = 'all' | 'tool' | 'model' | 'failure'
type PlatformFilter = 'all' | AuditPlatform | 'unlinked'
type PageState = 'loading' | 'ready' | 'unavailable' | 'error'

const loadedLimit = 200
const bufferedLimit = 100
const route = useRoute()
const router = useRouter()
const connection = useConnectionStore()
const query = ref('')
const submittedQuery = ref('')
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
const snapshotLatest = useLatest()
const detailLatest = useLatest()
const subscription = useLatestSubscription()

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
const searchQualifiers = [
  { prefix: 'thread:', icon: 'pi pi-sitemap', title: 'Thread', description: 'Show every run linked to a thread' },
  { prefix: 'run:', icon: 'pi pi-sparkles', title: 'Agent run', description: 'Show events from one agent run' },
] as const
const eventFilters = {
  all: () => true,
  tool: (event: AuditEvent) => auditPresentation(event).category === 'tool',
  model: (event: AuditEvent) => auditPresentation(event).category === 'model',
  failure: isAuditFailure,
} satisfies Record<EventFilter, (event: AuditEvent) => boolean>
const allKnownEvents = computed(() => mergeAuditRecords(events.value, buffered.value, loadedLimit + bufferedLimit))
const search = computed(() => parseAuditSearch(query.value))
const submittedSearch = computed(() => parseAuditSearch(submittedQuery.value))
const scopeKey = computed(() => {
  const scope = submittedSearch.value.scope
  return scope === undefined ? '' : `${scope.kind}:${String(scope.value)}`
})
const filteredEvents = computed(() => [...events.value].reverse().filter((record) => {
  const presentation = auditPresentation(record.event)
  const searchable = `${presentation.kind} ${presentation.summary} ${record.event.runId}`.toLowerCase()
  const eventPlatform = auditPlatform(allKnownEvents.value, record.event.runId)
  const platformMatches = platform.value === 'all'
    || platform.value === 'unlinked' && eventPlatform === undefined
    || platform.value === eventPlatform
  return searchable.includes(submittedSearch.value.text.trim().toLowerCase()) && platformMatches && eventFilters[eventType.value](record.event)
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
const requiredMethods = ['audit.recent', 'audit.search', 'audit.get', 'audit.thread', 'audit.subscribe'] as const
const supportsAudit = computed(() => requiredMethods.every((method) => connection.methods.has(method)))

function messageFieldKey(label: string): ThreadMessageKey | undefined {
  const event = selected.value?.event
  if (event?.tag !== 'AgentThreadLinked' || event.linkedMessageKey === null) return undefined
  if (label === 'Message') return event.linkedMessageKey
  return label === 'Parent message' && event.parentMessageId !== null
    ? { ...event.linkedMessageKey, messageId: event.parentMessageId }
    : undefined
}

function eventTime(record: AuditRecord): string {
  return new Date(record.occurredAt).toLocaleTimeString(undefined, { hour12: false })
}

function platformLabel(record: AuditRecord): string {
  const value = auditPlatform(allKnownEvents.value, record.event.runId)
  return platformOptions.find((option) => option.value === value)?.label ?? 'Unlinked'
}

async function loadSnapshot(): Promise<void> {
  const token = snapshotLatest.begin()
  if (!snapshotLatest.current(token)) return
  const result = await loadRequestedAudit()
  if (!snapshotLatest.current(token)) return
  if (result._tag === 'Failure') {
    error.value = result.error.message
    if (state.value !== 'ready') state.value = 'error'
    return
  }
  error.value = ''
  if (paused.value) {
    const unseen = result.value.filter(({ id }) => !events.value.some((record) => record.id === id))
    buffered.value = mergeAuditRecords(buffered.value, unseen, bufferedLimit)
  } else {
    const completeResult = requestedRunId() !== undefined
      || requestedThreadId() !== undefined
      || submittedSearch.value.text.trim() !== ''
    events.value = completeResult ? [...result.value] : mergeAuditRecords([], result.value, loadedLimit)
  }
  state.value = 'ready'
  const requested = requestedAuditId()
  if (requested !== undefined) await loadSelection(requested)
  else if (selectedId.value === undefined && events.value.length > 0) selectAuditId(events.value.at(-1)?.id)
}

async function loadRequestedAudit(): Promise<BackendResult<readonly AuditRecord[], BackendError>> {
  const runId = requestedRunId()
  if (runId !== undefined) return runBackend(getRunAudit(runId))
  const threadId = requestedThreadId()
  if (threadId === undefined) {
    const searchText = submittedSearch.value.text.trim()
    return runBackend(searchText === '' ? recentAudit(loadedLimit) : searchAudit(searchText))
  }
  return loadThreadAudit(threadId)
}

async function loadThreadAudit(threadId: number): Promise<BackendResult<readonly AuditRecord[], BackendError>> {
  const result = await runBackend(getThreadAudit(threadId))
  if (result._tag === 'Failure') return result
  return result.value === null
    ? { _tag: 'Failure', error: new RpcBackendError({ message: `Thread #${String(threadId)} was not found.` }) }
    : { _tag: 'Success', value: result.value }
}

function receive(record: AuditRecord): void {
  const runId = requestedRunId()
  if (runId !== undefined && record.event.runId !== runId) return
  if (requestedThreadId() !== undefined && !events.value.some(({ event }) => event.runId === record.event.runId)) return
  if (paused.value) buffered.value = mergeAuditRecords(buffered.value, [record], bufferedLimit)
  else events.value = mergeAuditRecords(events.value, [record], loadedLimit)
}

async function installSubscription(): Promise<void> {
  const token = subscription.begin()
  if (!subscription.current(token)) return
  snapshotLatest.invalidate()
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
  const result = await runBackend(subscribeAudit(
    () => subscription.current(token) ? loadSnapshot() : Promise.resolve(),
    (record) => { if (subscription.current(token)) receive(record) },
  ))
  if (!subscription.current(token)) {
    if (result._tag === 'Success') subscription.own(token, result.value)
    return
  }
  if (result._tag === 'Failure') {
    error.value = result.error.message
    state.value = events.value.length === 0 ? 'error' : 'ready'
  } else {
    subscription.own(token, result.value)
  }
}

function requestedAuditId(): number | undefined {
  const raw = route.params['auditId']
  if (typeof raw !== 'string') return undefined
  const id = Number(raw)
  return Number.isSafeInteger(id) && id > 0 ? id : undefined
}

function requestedRunId(): string | undefined {
  const scope = submittedSearch.value.scope
  if (scope !== undefined) return scope.kind === 'run' ? scope.value : undefined
  const routeRunId = route.query['run']
  return typeof routeRunId === 'string' && routeRunId.trim() !== '' ? routeRunId.trim() : undefined
}

function requestedThreadId(): number | undefined {
  const scope = submittedSearch.value.scope
  if (scope !== undefined) return scope.kind === 'thread' ? scope.value : undefined
  const routeThreadId = route.query['thread']
  if (typeof routeThreadId !== 'string') return undefined
  const value = Number(routeThreadId)
  return Number.isSafeInteger(value) && value > 0 ? value : undefined
}

function syncScopeFromRoute(): void {
  const runId = route.query['run']
  const threadId = route.query['thread']
  const token = typeof runId === 'string' && runId.trim() !== ''
    ? `run:${runId.trim()}`
    : typeof threadId === 'string' && Number.isSafeInteger(Number(threadId)) && Number(threadId) > 0
      ? `thread:${threadId}`
      : undefined
  if (token === undefined) {
    if (submittedSearch.value.scope !== undefined) {
      query.value = search.value.text
      submittedQuery.value = query.value
    }
  } else if (token !== scopeKey.value) {
    query.value = `${token} ${search.value.text}`.trim()
    submittedQuery.value = query.value
  }
}

function routeScopeChanged(): void {
  const previous = scopeKey.value
  syncScopeFromRoute()
  if (scopeKey.value !== previous) void loadSnapshot()
}

async function updateScopeRoute(): Promise<void> {
  const scope = submittedSearch.value.scope
  const key = scopeKey.value
  const routeKey = typeof route.query['run'] === 'string'
    ? `run:${route.query['run']}`
    : typeof route.query['thread'] === 'string' ? `thread:${route.query['thread']}` : ''
  if (key !== routeKey) {
    if (scope === undefined) await router.replace({ name: 'audit' })
    else await router.replace({ name: 'audit', query: { [scope.kind]: String(scope.value) } })
  }
  await loadSnapshot()
}

function submitSearch(value: string): void {
  submittedQuery.value = value.trim()
  void updateScopeRoute()
}

function selectAuditId(id: number | null | undefined): void {
  if (typeof id !== 'number' || !Number.isSafeInteger(id) || id < 1) return
  void loadSelection(id)
  void router.replace({ name: 'audit', params: { auditId: String(id) } })
}

function openSelectedThread(): void {
  if (selected.value !== undefined) void router.push({ name: 'threads', query: { run: selected.value.event.runId } })
}

function openMediaRef(event: MouseEvent): void {
  const mediaRef = mediaRefFromClick(event)
  if (mediaRef === undefined) return
  event.preventDefault()
  void router.push({ name: 'media', params: { mediaId: mediaRef } })
}

async function loadSelection(id: number): Promise<void> {
  selectedId.value = id
  selectedDetail.value = undefined
  related.value = []
  detailError.value = ''
  threadError.value = ''
  const token = detailLatest.begin()
  if (!detailLatest.current(token)) return
  const result = await runBackend(getAudit(id))
  if (!detailLatest.current(token)) return
  if (result._tag === 'Failure') {
    detailError.value = result.error.message
    return
  }
  if (result.value === null) {
    detailError.value = `Audit event #${String(id)} was not found.`
    return
  }
  selectedDetail.value = result.value
  const target = await runBackend(resolveThreadRun(result.value.event.runId))
  if (!detailLatest.current(token)) return
  if (target._tag === 'Failure' || target.value.threadId === null) {
    related.value = events.value.filter(({ event }) => event.runId === result.value?.event.runId)
    return
  }
  const threadResult = await loadThreadAudit(target.value.threadId)
  if (!detailLatest.current(token)) return
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
watch([() => route.query['run'], () => route.query['thread']], routeScopeChanged)
watch([() => connection.state, () => connection.methods], () => { void installSubscription() })
onMounted(() => { syncScopeFromRoute(); void installSubscription() })
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
        <SearchQualifierInput
          v-model="query"
          :qualifiers="searchQualifiers"
          placeholder="Search or use thread:42"
          input-label="Search audit events"
          @submit="submitSearch"
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
        v-if="error"
        severity="error"
        :closable="false"
      >
        {{ error }}
      </Message>
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
                  <small><RunIdLink :run-id="option.event.runId" /> · {{ platformLabel(option) }}</small>
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
                <dt>{{ field.label }}</dt><dd>
                  <RunIdLink
                    v-if="field.kind === 'run'"
                    :run-id="field.value"
                  /><ChatLogMessageLink
                    v-else-if="messageFieldKey(field.label)"
                    :message-key="messageFieldKey(field.label)!"
                  /><template v-else>
                    {{ field.value }}
                  </template>
                </dd>
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
              <div
                class="markdown-body audit-result"
                :innerHTML="renderMarkdown(selectedResult.text)"
                @click="openMediaRef"
              />
              <small
                v-if="selectedResult.truncated"
                class="bounded-note"
              >Preview limited to 12,000 characters.</small>
            </template>
            <template v-if="related.length > 0">
              <div class="inspector-section-heading">
                <h3>Related thread</h3>
                <Button
                  label="Open thread"
                  icon="pi pi-arrow-up-right"
                  severity="secondary"
                  size="small"
                  @click="openSelectedThread"
                />
              </div>
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
