<script setup lang="ts">
import { computed, ref } from 'vue'
import Button from 'primevue/button'
import IconField from 'primevue/iconfield'
import InputIcon from 'primevue/inputicon'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import { chatSessionName } from '@/domain/chat'
import type { ChatSession } from '@/types/domain'

const props = defineProps<{
  sessions: readonly ChatSession[]
  selectedId?: string | undefined
}>()
const emit = defineEmits<{ select: [sessionId: string] }>()
const query = ref('')
const filteredSessions = computed(() => props.sessions.filter((session) => chatSessionName(session).toLowerCase().includes(query.value.toLowerCase())))
</script>

<template>
  <aside
    class="conversation-list"
    aria-label="Chat sessions"
  >
    <IconField class="conversation-search">
      <InputIcon class="pi pi-search" />
      <InputText
        v-model="query"
        placeholder="Find a session"
        aria-label="Find a session"
        size="small"
        fluid
      />
    </IconField>
    <Button
      v-for="session in filteredSessions"
      :key="session.sessionId"
      class="conversation"
      :class="{ active: selectedId === session.sessionId }"
      unstyled
      @click="emit('select', session.sessionId)"
    >
      <span class="platform-icon">R</span>
      <span><strong>{{ chatSessionName(session) }}</strong><small><code>{{ session.sessionId }}</code></small></span>
    </Button>
    <Message
      v-if="sessions.length === 0"
      severity="secondary"
      :closable="false"
    >
      No RPC sessions yet.
    </Message>
  </aside>
</template>
