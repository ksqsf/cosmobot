<script setup lang="ts">
import { nextTick, ref, useTemplateRef, watch } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import ContextMenu, { type ContextMenuMethods } from 'primevue/contextmenu'
import Message from 'primevue/message'
import type { MenuItem } from 'primevue/menuitem'
import ChatMessageItem from '@/components/chat/ChatMessage.vue'
import { chatSessionName } from '@/domain/chat'
import { useOverlayLayer } from '@/overlay'
import type { ChatMessage, ChatSession } from '@/types/domain'

const props = defineProps<{
  session?: ChatSession | undefined
  messages: readonly ChatMessage[]
  streamingMessageIds: ReadonlySet<string>
  loading: boolean
  loadingOlder: boolean
  hasOlder: boolean
  loadOlder: () => Promise<void>
}>()
const emit = defineEmits<{
  rename: []
  delete: []
  fork: [message: ChatMessage]
  previewImage: [url: string]
  error: [message: string]
}>()
const toast = useToast()
const pane = useTemplateRef<HTMLElement>('pane')
const messageMenu = useTemplateRef<ContextMenuMethods>('messageMenu')
const contextMessage = ref<ChatMessage>()
const messageMenuLayer = useOverlayLayer()
const messageMenuItems: MenuItem[] = [
  { label: 'Copy text', icon: 'pi pi-copy', command: () => { void copyContextText() } },
  { separator: true },
  { label: 'Fork conversation here', icon: 'pi pi-share-alt', command: () => { if (contextMessage.value !== undefined) emit('fork', contextMessage.value) } },
]

async function copyContextText(): Promise<void> {
  const text = contextMessage.value?.text
  if (!text) return
  try {
    await navigator.clipboard.writeText(text)
    toast.add({ severity: 'success', summary: 'Copied', detail: 'Message text copied.', life: 1800 })
  } catch {
    emit('error', 'Could not copy the message text.')
  }
}

function showMessageMenu(event: MouseEvent, message: ChatMessage): void {
  contextMessage.value = message
  messageMenu.value?.show(event)
}

function isCurrentSession(sessionId: string): boolean {
  return props.session?.sessionId === sessionId
}

async function requestOlder(): Promise<void> {
  const sessionId = props.session?.sessionId
  if (sessionId === undefined) return
  const previousHeight = pane.value?.scrollHeight ?? 0
  await props.loadOlder()
  if (!isCurrentSession(sessionId)) return
  await nextTick()
  if (!isCurrentSession(sessionId)) return
  if (pane.value !== null) pane.value.scrollTop += pane.value.scrollHeight - previousHeight
}

function loadOlderAtTop(event: Event): void {
  const target = event.currentTarget
  if (target instanceof HTMLElement && target.scrollTop < 120) void requestOlder()
}

watch(() => props.loading, async (loading, wasLoading) => {
  if (loading || !wasLoading) return
  await nextTick()
  pane.value?.querySelector('.message:last-of-type')?.scrollIntoView({ block: 'start' })
})
</script>

<template>
  <section
    class="transcript"
    aria-label="Chat transcript"
  >
    <header class="transcript-header">
      <div><strong>{{ session ? chatSessionName(session) : 'Select a session' }}</strong><small v-if="session"><code>{{ session.sessionId }}</code></small></div>
      <div v-if="session">
        <Button
          icon="pi pi-pencil"
          text
          rounded
          aria-label="Rename session"
          @click="emit('rename')"
        /><Button
          icon="pi pi-trash"
          severity="danger"
          text
          rounded
          aria-label="Delete session"
          @click="emit('delete')"
        />
      </div>
    </header>
    <ContextMenu
      ref="messageMenu"
      :model="messageMenuItems"
      @show="messageMenuLayer.show"
      @hide="messageMenuLayer.hide(); contextMessage = undefined"
    />
    <div
      ref="pane"
      class="messages"
      @scroll="loadOlderAtTop"
    >
      <Button
        v-if="hasOlder"
        label="Load older messages"
        severity="secondary"
        text
        size="small"
        :loading="loadingOlder"
        @click="requestOlder"
      />
      <Message
        v-if="loading"
        severity="secondary"
        :closable="false"
      >
        Loading transcript…
      </Message>
      <Message
        v-else-if="session && messages.length === 0"
        severity="secondary"
        :closable="false"
      >
        This session has no messages yet.
      </Message>
      <ChatMessageItem
        v-for="message in messages"
        :key="message.messageId"
        :message
        :streaming="streamingMessageIds.has(message.messageId)"
        :selected="contextMessage?.messageId === message.messageId"
        @menu="showMessageMenu"
        @preview-image="emit('previewImage', $event)"
      />
    </div>
    <slot />
  </section>
</template>
