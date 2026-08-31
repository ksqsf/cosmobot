<script setup lang="ts">
import Button from 'primevue/button'
import Tag from 'primevue/tag'
import type { ChatAttachment } from '@/types/domain'

defineProps<{ attachments: readonly ChatAttachment[] }>()
const emit = defineEmits<{ remove: [attachment: ChatAttachment] }>()
</script>

<template>
  <div
    v-if="attachments.length > 0"
    class="tag-list"
  >
    <span
      v-for="attachment in attachments"
      :key="attachment.attachmentId"
      class="pending-attachment"
    ><Tag
      :value="attachment.name"
      severity="secondary"
    /><Button
      type="button"
      icon="pi pi-times"
      text
      rounded
      size="small"
      :aria-label="`Remove ${attachment.name}`"
      @click="emit('remove', attachment)"
    /></span>
  </div>
</template>
