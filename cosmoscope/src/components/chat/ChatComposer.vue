<script setup lang="ts">
import { useTemplateRef } from 'vue'
import Button from 'primevue/button'
import Textarea from 'primevue/textarea'
import AttachmentTray from '@/components/chat/AttachmentTray.vue'
import type { ChatAttachment } from '@/types/domain'

defineProps<{
  attachments: readonly ChatAttachment[]
  disabled: boolean
  sending: boolean
  uploading: boolean
}>()
const emit = defineEmits<{
  attach: [files: readonly File[]]
  remove: [attachment: ChatAttachment]
  send: []
}>()
const draft = defineModel<string>({ required: true })
const attachmentInput = useTemplateRef<HTMLInputElement>('attachmentInput')

function attachFiles(event: Event): void {
  const input = event.currentTarget
  if (!(input instanceof HTMLInputElement) || input.files === null) return
  const files = [...input.files]
  input.value = ''
  emit('attach', files)
}
</script>

<template>
  <form
    class="composer"
    @submit.prevent="emit('send')"
  >
    <AttachmentTray
      :attachments
      @remove="emit('remove', $event)"
    />
    <Textarea
      v-model="draft"
      rows="3"
      placeholder="Message cosmobot…"
      aria-label="Message cosmobot"
      class="composer-input"
      :disabled
      fluid
      @keydown.ctrl.enter.prevent="emit('send')"
    />
    <div class="composer-actions">
      <div>
        <Button
          type="button"
          label="Attach"
          icon="pi pi-paperclip"
          size="small"
          severity="secondary"
          text
          :loading="uploading"
          :disabled="disabled || uploading"
          @click="attachmentInput?.click()"
        /><input
          ref="attachmentInput"
          type="file"
          multiple
          hidden
          @change="attachFiles"
        />
      </div>
      <Button
        type="submit"
        icon="pi pi-arrow-up"
        aria-label="Send message"
        :loading="sending"
        :disabled="disabled || uploading || sending || (draft.trim() === '' && attachments.length === 0)"
      />
    </div>
  </form>
</template>
