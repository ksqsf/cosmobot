<script setup lang="ts">
import Button from 'primevue/button'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import PageHeading from '@/components/PageHeading.vue'
import AuditDetailPanel from '@/components/audit/AuditDetailPanel.vue'
import AuditRecordList from '@/components/audit/AuditRecordList.vue'
import AuditScopeBar from '@/components/audit/AuditScopeBar.vue'
import { useAuditScopeRoute } from '@/composables/useAuditScopeRoute'
import { useAuditStream } from '@/composables/useAuditStream'
import { useConnectionStore } from '@/stores/connection'

const connection = useConnectionStore()
function reload(): void { void stream.loadSnapshot() }
function select(id: number): void {
  if (id !== stream.selectedId.value) void stream.loadSelection(id)
}
const scope = useAuditScopeRoute(reload, select)
const stream = useAuditStream(scope)
const {
  state, error, events, buffered, paused, selectedId, selected, related, detailError, threadError,
  platforms, eventTypes, filteredEvents, installSubscription,
} = stream
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
          @click="stream.resume"
        />
        <Button
          label="Discard"
          severity="secondary"
          @click="stream.discard"
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
      <AuditScopeBar
        v-model:query="scope.query.value"
        v-model:platforms="platforms"
        v-model:event-types="eventTypes"
        @submit="scope.submitSearch"
      />
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
        <AuditRecordList
          :records="filteredEvents"
          :selected-id="selectedId"
          :total="events.length"
          :paused="paused"
          :buffered="buffered.length"
          :platform-label="stream.platformLabel"
          @select="scope.selectAuditId"
        />
        <AuditDetailPanel
          :selected="selected"
          :related="related"
          :detail-error="detailError"
          :thread-error="threadError"
          @open-thread="scope.openThread"
          @open-media="scope.openMedia"
        />
      </div>
    </template>
  </section>
</template>
