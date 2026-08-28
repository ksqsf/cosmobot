<script setup lang="ts">
import { computed } from 'vue'
import { RouterLink } from 'vue-router'
import type { ThreadMessageKey } from '@/types/domain'

const props = defineProps<{ messageKey: ThreadMessageKey }>()
const query = computed(() => ({
  view: 'logs',
  platform: props.messageKey.platform,
  ...(props.messageKey.chatId === null ? {} : { chat: props.messageKey.chatId }),
  message: props.messageKey.messageId,
}))
</script>

<template>
  <RouterLink
    :to="{ name: 'chat', query }"
    :title="`Open message ${messageKey.messageId} in chat logs`"
  >
    <code>{{ messageKey.messageId }}</code>
  </RouterLink>
</template>
