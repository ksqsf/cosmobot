<script setup lang="ts">
import { computed, watch } from 'vue'
import { RouterLink } from 'vue-router'
import Button from 'primevue/button'
import Message from 'primevue/message'
import Tag from 'primevue/tag'
import ThreadStatsPanel from './ThreadStatsPanel.vue'
import ThreadTranscript from './ThreadTranscript.vue'
import { useTranscriptMedia } from '@/composables/useTranscriptMedia'
import { useLatest } from '@/async'
import { useSubagentInspector } from '@/composables/useSubagentInspector'
import { subagentRuns, subagentStatus } from '@/domain/subagents'
import { threadStats } from '@/backend/threadStats'
import type { AuditRecord } from '@/types/domain'

const props = defineProps<{ records: readonly AuditRecord[] }>()
const emit = defineEmits<{ previewImage: [url: string] }>()
const child = useSubagentInspector()
const media = useTranscriptMedia()
const mediaLatest = useLatest()
watch(() => child.selected.value?.childRunId, () => { mediaLatest.invalidate(); media.reset() })
watch(child.messages, (messages) => {
  const token = mediaLatest.begin()
  if (messages !== null) void media.loadMediaForMessages(messages, () => mediaLatest.current(token))
})
const parentWaiting = computed(() => subagentStatus(props.records) === 'Waiting for subagents')
const roots = computed(() => subagentRuns(props.records))
const children = computed(() => subagentRuns(child.selected.value ? child.records.value : props.records)
  .filter(({ childRunId }) => !child.trail.value.some((ancestor) => ancestor.childRunId === childRunId)))
const stats = computed(() => threadStats(child.records.value))
const status = computed(() => subagentStatus(child.records.value))

watch(roots, (runs) => {
  const root = child.trail.value[0]
  if (root && !runs.some(({ childRunId }) => childRunId === root.childRunId)) child.back(0)
})
</script>

<template>
  <section
    class="subagents-panel"
    aria-label="Subagents"
  >
    <h3>Subagents</h3>
    <p
      v-if="parentWaiting"
      role="status"
    >
      Parent is waiting for subagent results.
    </p>
    <nav
      v-if="child.selected.value"
      aria-label="Subagent ancestry"
      class="tag-list"
    >
      <Button
        label="All subagents"
        severity="secondary"
        size="small"
        @click="child.back(0)"
      />
      <Button
        v-for="(ancestor, index) in child.trail.value"
        :key="ancestor.childRunId"
        :label="ancestor.subagentId"
        :disabled="index === child.trail.value.length - 1"
        severity="secondary"
        size="small"
        @click="child.back(index + 1)"
      />
    </nav>
    <template v-if="child.selected.value">
      <header class="tag-list">
        <h4>{{ child.selected.value.subagentId }}</h4>
        <code>{{ child.selected.value.childRunId }}</code>
        <Tag
          :value="status"
          severity="secondary"
        />
        <RouterLink :to="{ name: 'audit', query: { run: child.selected.value.runId } }">
          View parent run
        </RouterLink>
        <RouterLink :to="{ name: 'audit', query: { run: child.selected.value.childRunId } }">
          View subagent audit
        </RouterLink>
        <RouterLink
          v-if="child.hasThread.value"
          :to="{ name: 'threads', params: { threadId: '' }, query: { run: child.selected.value.childRunId } }"
        >
          Open subagent thread
        </RouterLink>
        <Button
          label="Refresh subagent"
          severity="secondary"
          size="small"
          @click="child.reload"
        />
      </header>
      <Message
        v-if="child.error.value"
        severity="error"
        :closable="false"
      >
        {{ child.error.value }}
      </Message>
      <Message
        v-if="child.linkError.value"
        severity="warn"
        :closable="false"
      >
        {{ child.linkError.value }}
      </Message>
      <Message
        v-if="child.liveError.value"
        severity="warn"
        :closable="false"
      >
        {{ child.liveError.value }}
      </Message>
      <p
        v-if="child.loading.value"
        role="status"
      >
        Loading subagent…
      </p>
      <template v-else-if="!child.error.value">
        <p v-if="child.records.value.length === 0">
          No audit records available for this subagent.
        </p>
        <ThreadStatsPanel
          v-else
          :stats="stats"
          :audit-event-count="child.records.value.length"
        />
      </template>
      <Message
        v-if="child.transcriptError.value"
        severity="warn"
        :closable="false"
      >
        {{ child.transcriptError.value }}
      </Message>
      <p
        v-else-if="child.transcriptLoading.value"
        role="status"
      >
        Loading conversation…
      </p>
      <p v-else-if="child.messages.value === null">
        Conversation unavailable. Older runs may not have a saved transcript.
      </p>
      <p v-else-if="child.messages.value.length === 0">
        No conversation messages recorded.
      </p>
      <ThreadTranscript
        v-else
        :messages="child.messages.value"
        :image-urls="media.imageUrls"
        :attachments="media.messageAttachments"
        @preview-image="emit('previewImage', $event)"
      />
      <h4>Child subagents</h4>
    </template>
    <ul v-if="children.length">
      <li
        v-for="run in children"
        :key="run.childRunId"
      >
        <Button
          :label="run.subagentId"
          severity="secondary"
          size="small"
          @click="child.select(run)"
        />
        <code>{{ run.childRunId }}</code>
        <span>Status unknown · inspect to load</span>
      </li>
    </ul>
    <p v-else-if="!child.loading.value">
      No subagents recorded.
    </p>
  </section>
</template>

<style scoped>
.subagents-panel { display: grid; gap: 0.75rem; min-width: 0; }
ul { list-style: none; padding: 0; margin: 0; display: grid; gap: 0.5rem; }
li { display: flex; align-items: center; flex-wrap: wrap; gap: 0.75rem; }
code { overflow-wrap: anywhere; }
</style>
