<script setup lang="ts">
import { computed } from 'vue'
import ProgressSpinner from 'primevue/progressspinner'
import MessageContent from '@/components/MessageContent.vue'
import { safeDownloadUrl } from '@/backend/chat'
import { chatMessageAttachments, chatMessageImages } from '@/domain/chat'
import type { ChatMessage } from '@/types/domain'

const props = defineProps<{
  message: ChatMessage
  streaming: boolean
  selected: boolean
}>()
const emit = defineEmits<{
  menu: [event: MouseEvent, message: ChatMessage]
  previewImage: [url: string]
}>()
const images = computed(() => chatMessageImages(props.message, window.location.href))
const attachments = computed(() => chatMessageAttachments(props.message, window.location.href))
const unsafeAttachments = computed(() => props.message.attachments.filter(({ kind, url }) => kind !== 'image' && safeDownloadUrl(url, window.location.href) === undefined))
</script>

<template>
  <article
    class="message"
    :class="[message.sender === 'user' ? 'user' : 'bot', { 'context-selected': selected }]"
    tabindex="0"
    @contextmenu.prevent="emit('menu', $event, message)"
  >
    <header class="message-meta">
      <span class="avatar">{{ message.sender === 'user' ? 'Y' : 'C' }}</span><strong>{{ message.sender === 'user' ? 'You' : 'Cosmobot' }}</strong><ProgressSpinner
        v-if="streaming"
        aria-label="Streaming response"
        class="chat-streaming"
      />
    </header>
    <div class="message-body">
      <MessageContent
        :text="message.text"
        :images
        :attachments
        @preview-image="emit('previewImage', $event)"
      />
      <div
        v-for="attachment in unsafeAttachments"
        :key="attachment.attachmentId"
        class="chat-file-card"
      >
        <i class="pi pi-file" /><span><strong>{{ attachment.name }}</strong><small>Unsafe download URL rejected</small></span>
      </div>
    </div>
  </article>
</template>
