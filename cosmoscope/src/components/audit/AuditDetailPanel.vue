<script setup lang="ts">
import { computed } from 'vue'
import Button from 'primevue/button'
import Card from 'primevue/card'
import Message from 'primevue/message'
import ChatLogMessageLink from '@/components/ChatLogMessageLink.vue'
import RunIdLink from '@/components/RunIdLink.vue'
import {
  auditArguments, auditDetailFields, auditPresentation, auditResult, boundedStructuredText,
} from '@/backend/audit'
import { mediaRefFromClick, renderMarkdown } from '@/markdown'
import type { AuditRecord, ThreadMessageKey } from '@/types/domain'

const props = defineProps<{
  selected: AuditRecord | undefined
  related: readonly AuditRecord[]
  detailError: string
  threadError: string
}>()
const emit = defineEmits<{ openThread: [runId: string], openMedia: [mediaId: string] }>()
const presentation = computed(() => props.selected === undefined ? undefined : auditPresentation(props.selected.event))
const fields = computed(() => props.selected === undefined ? [] : auditDetailFields(props.selected.event))
const argumentsText = computed(() => {
  const value = props.selected && auditArguments(props.selected.event)
  return value === undefined ? undefined : boundedStructuredText(value)
})
const resultText = computed(() => {
  const value = props.selected && auditResult(props.selected.event)
  return value === undefined ? undefined : boundedStructuredText(value)
})
const eventTime = (record: AuditRecord): string =>
  new Date(record.occurredAt).toLocaleTimeString(undefined, { hour12: false })

function messageFieldKey(label: string): ThreadMessageKey | undefined {
  const event = props.selected?.event
  if (event?.tag !== 'AgentThreadLinked' || event.linkedMessageKey === null) return undefined
  if (label === 'Message') return event.linkedMessageKey
  return label === 'Parent message' && event.parentMessageId !== null
    ? { ...event.linkedMessageKey, messageId: event.parentMessageId }
    : undefined
}

function openMediaRef(event: MouseEvent): void {
  const mediaRef = mediaRefFromClick(event)
  if (mediaRef === undefined) return
  event.preventDefault()
  emit('openMedia', mediaRef)
}
</script>

<template>
  <Message
    v-if="detailError"
    severity="error"
    :closable="false"
  >
    {{ detailError }}
  </Message>
  <Card
    v-else-if="selected && presentation"
    class="inspector audit-inspector"
  >
    <template #title>
      {{ presentation.kind }}
    </template>
    <template #subtitle>
      Audit event #{{ selected.id }}
    </template>
    <template #content>
      <dl class="detail-list">
        <div><dt>Recorded</dt><dd>{{ new Date(selected.occurredAt).toLocaleString() }}</dd></div>
        <div><dt>Type</dt><dd><code>{{ selected.event.tag }}</code></dd></div>
        <div
          v-for="field in fields"
          :key="field.label"
        >
          <dt>{{ field.label }}</dt><dd>
            <RunIdLink
              v-if="field.kind === 'run'"
              :run-id="field.value"
            />
            <ChatLogMessageLink
              v-else-if="messageFieldKey(field.label)"
              :message-key="messageFieldKey(field.label)!"
            />
            <template v-else>
              {{ field.value }}
            </template>
          </dd>
        </div>
      </dl>
      <template v-if="argumentsText">
        <h3>Arguments</h3>
        <pre><code>{{ argumentsText.text }}</code></pre>
        <small
          v-if="argumentsText.truncated"
          class="bounded-note"
        >Preview limited to 12,000 characters.</small>
      </template>
      <template v-if="resultText">
        <h3>Result</h3>
        <div
          class="markdown-body audit-result"
          :innerHTML="renderMarkdown(resultText.text)"
          @click="openMediaRef"
        />
        <small
          v-if="resultText.truncated"
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
            @click="emit('openThread', selected.event.runId)"
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
</template>
