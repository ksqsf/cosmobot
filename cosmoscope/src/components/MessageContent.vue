<script setup lang="ts">
import { RouterLink, useRouter } from 'vue-router'
import { mediaRefFromClick, renderMarkdown } from '@/markdown'
import type { MessageContentAttachment } from '@/components/messageContent'

defineProps<{
  text: string
  images: readonly string[]
  attachments: readonly MessageContentAttachment[]
}>()

const emit = defineEmits<{ previewImage: [url: string] }>()
const router = useRouter()

function openMediaRef(event: MouseEvent): void {
  const mediaRef = mediaRefFromClick(event)
  if (mediaRef === undefined) return
  event.preventDefault()
  void router.push({ name: 'media', params: { mediaId: mediaRef } })
}
</script>

<template>
  <div
    v-if="images.length"
    class="chat-images thread-media"
  >
    <button
      v-for="url in images"
      :key="url"
      type="button"
      class="chat-image-button"
      aria-label="Zoom image"
      @click="emit('previewImage', url)"
    >
      <img
        :src="url"
        alt="Message image"
        loading="lazy"
      />
    </button>
  </div>
  <div
    v-if="attachments.length"
    class="thread-attachments"
  >
    <template
      v-for="attachment in attachments"
      :key="attachment.key"
    >
      <video
        v-if="attachment.url && attachment.mimeType.startsWith('video/')"
        class="object-preview"
        controls
        preload="metadata"
        :src="attachment.url"
      />
      <audio
        v-else-if="attachment.url && attachment.mimeType.startsWith('audio/')"
        controls
        preload="metadata"
        :src="attachment.url"
      />
      <RouterLink
        v-else-if="attachment.mediaId"
        class="chat-file-card"
        :to="{ name: 'media', params: { mediaId: attachment.mediaId } }"
      >
        <i class="pi pi-file" /><span><strong>{{ attachment.name }}</strong><small>{{ attachment.detail }}</small></span><i class="pi pi-arrow-right" />
      </RouterLink>
      <a
        v-else-if="attachment.url"
        class="chat-file-card"
        :href="attachment.url"
        target="_blank"
        rel="noopener noreferrer"
        download
      ><i class="pi pi-file" /><span><strong>{{ attachment.name }}</strong><small>{{ attachment.detail }}</small></span><i class="pi pi-download" /></a>
    </template>
  </div>
  <div
    v-if="text"
    class="markdown-body"
    :innerHTML="renderMarkdown(text)"
    @click="openMediaRef"
  />
</template>
