<script setup lang="ts">
import Button from 'primevue/button'
import Message from 'primevue/message'
import RunIdLink from '@/components/RunIdLink.vue'
import type { ActiveThread } from '@/types/domain'

defineProps<{
  threads: readonly ActiveThread[]
  error: string
  haltingTaskId?: number | undefined
}>()
const emit = defineEmits<{
  open: [thread: ActiveThread]
  halt: [thread: ActiveThread]
}>()
</script>

<template>
  <article class="panel active-thread-panel">
    <header class="stream-heading">
      <span><i class="pi pi-circle-fill active-thread-pulse" />Active threads</span><small>Live · refreshes every second</small>
    </header>
    <Message
      v-if="error"
      severity="error"
      :closable="false"
    >
      {{ error }}
    </Message>
    <p
      v-else-if="threads.length === 0"
      class="active-thread-empty"
    >
      No agent threads are currently running.
    </p>
    <ul
      v-else
      class="active-thread-list"
    >
      <li
        v-for="active in threads"
        :key="active.taskId"
      >
        <span class="manager-type-icon"><i class="pi pi-sparkles" /></span>
        <span><strong>{{ active.prompt || 'Untitled prompt' }}</strong><small>Task #{{ active.taskId }} · Run <RunIdLink :run-id="active.runId" /> · {{ active.messages.length }} messages<span v-if="active.pendingSteers"> · {{ active.pendingSteers }} pending steer</span></small></span>
        <Button
          label="Open thread"
          icon="pi pi-eye"
          severity="secondary"
          size="small"
          @click="emit('open', active)"
        />
        <Button
          label="Halt"
          icon="pi pi-stop-circle"
          severity="danger"
          size="small"
          :loading="haltingTaskId === active.taskId"
          @click="emit('halt', active)"
        />
      </li>
    </ul>
  </article>
</template>
