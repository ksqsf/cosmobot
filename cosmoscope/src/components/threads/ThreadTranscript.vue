<script setup lang="ts">
import { computed, nextTick, ref, useTemplateRef } from 'vue'
import { useRouter, type RouteLocationRaw } from 'vue-router'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import ContextMenu, { type ContextMenuMethods } from 'primevue/contextmenu'
import Tag from 'primevue/tag'
import type { MenuItem } from 'primevue/menuitem'
import ChatLogMessageLink from '@/components/ChatLogMessageLink.vue'
import MessageContent from '@/components/MessageContent.vue'
import type { MessageContentAttachment } from '@/components/messageContent'
import { threadMessageChatKey, type ThreadToolActivity } from '@/backend/thread'
import { highlightCode } from '@/markdown'
import { useOverlayLayer } from '@/overlay'
import { messageText, readableMessageText, roleLabel, type ThreadTranscriptEntry } from '@/domain/threadTranscript'
import type { ActiveThread, StoredThreadMessage, ThreadMessageKey } from '@/types/domain'

defineProps<{
  active?: ActiveThread | undefined
  activeMessages: readonly StoredThreadMessage[]
  activeTools: readonly ThreadToolActivity[]
  transcript: readonly ThreadTranscriptEntry[]
  selectedMessageKey?: ThreadMessageKey | undefined
  imageUrls: (message: StoredThreadMessage) => string[]
  attachments: (message: StoredThreadMessage) => MessageContentAttachment[]
}>()
const emit = defineEmits<{ previewImage: [url: string] }>()
const router = useRouter()
const toast = useToast()
const list = ref<HTMLOListElement>()
const menu = useTemplateRef<ContextMenuMethods>('menu')
const contextEntry = ref<ThreadTranscriptEntry>()
const menuLayer = useOverlayLayer()
const menuItems = computed<MenuItem[]>(() => {
  const entry = contextEntry.value
  if (entry === undefined) return []
  const messageKey = transcriptMessageKey(entry)
  return [
    { label: 'Copy text', icon: 'pi pi-copy', disabled: readableMessageText(entry.message) === '', command: () => { void copyText(entry.message) } },
    ...(messageKey === undefined ? [] : [
      { separator: true },
      { label: 'Open in chat logs', icon: 'pi pi-comments', command: () => { void router.push(chatLogLocation(messageKey)) } },
      { label: 'Copy message link', icon: 'pi pi-link', command: () => { void copyChatLogLink(messageKey) } },
    ]),
  ]
})

function transcriptMessageKey(entry: ThreadTranscriptEntry): ThreadMessageKey | undefined {
  return threadMessageChatKey(entry.node, entry.messageIndex)
}

function chatLogLocation(messageKey: ThreadMessageKey): RouteLocationRaw {
  return {
    name: 'chat' as const,
    query: {
      view: 'logs',
      platform: messageKey.platform,
      ...(messageKey.chatId === null ? {} : { chat: messageKey.chatId }),
      message: messageKey.messageId,
    },
  }
}

function showMenu(event: Event, entry: ThreadTranscriptEntry): void {
  contextEntry.value = entry
  menu.value?.show(event)
}

async function copyText(message: StoredThreadMessage): Promise<void> {
  try { await navigator.clipboard.writeText(readableMessageText(message)) } catch {
    toast.add({ severity: 'error', summary: 'Could not copy the message text.', life: 3000 })
  }
}

async function copyChatLogLink(messageKey: ThreadMessageKey): Promise<void> {
  const href = router.resolve(chatLogLocation(messageKey)).href
  try { await navigator.clipboard.writeText(new URL(href, globalThis.location.href).href) } catch {
    toast.add({ severity: 'error', summary: 'Could not copy the message link.', life: 3000 })
  }
}

function toolStatusSeverity(status: string): 'success' | 'info' | 'danger' {
  if (status === 'running') return 'info'
  return /^(?:ok|success|succeeded)$/i.test(status) ? 'success' : 'danger'
}

function toolResultMessage(tool: ThreadToolActivity): StoredThreadMessage {
  return { role: 'tool', content: tool.result ?? '' }
}

function isPinned(): boolean {
  const element = list.value
  return element === undefined || element.scrollHeight - element.scrollTop - element.clientHeight < 80
}

async function scrollToEnd(): Promise<void> {
  await nextTick()
  if (list.value !== undefined) list.value.scrollTop = list.value.scrollHeight
}

defineExpose({ isPinned, scrollToEnd })
</script>

<template>
  <section class="thread-transcript-panel">
    <ContextMenu
      ref="menu"
      :model="menuItems"
      @show="menuLayer.show"
      @hide="menuLayer.hide(); contextEntry = undefined"
    />
    <header>
      <span>{{ active ? 'Live context' : 'Context at node' }}</span><small><template v-if="active">{{ activeMessages.length }} messages</template><ChatLogMessageLink
        v-else-if="selectedMessageKey"
        :message-key="selectedMessageKey"
      /><template v-else>No node selected</template></small>
    </header>
    <div
      v-if="active && activeTools.length"
      class="active-tool-stream"
    >
      <details
        v-for="tool in activeTools"
        :key="tool.id"
        class="thread-tool-call"
        :open="tool.status === 'running'"
      >
        <summary>
          <Tag
            :value="tool.name"
            severity="secondary"
          /><span>Turn {{ tool.turn }}</span><Tag
            :value="tool.status"
            :severity="toolStatusSeverity(tool.status)"
          />
        </summary>
        <div class="active-tool-detail">
          <small>Arguments</small><pre><code
class="hljs language-json"
                                             :innerHTML="highlightCode(tool.arguments || '{}', 'json')"
          /></pre>
          <template v-if="tool.result !== undefined">
            <small>Result</small><MessageContent
              :text="tool.result"
              :images="imageUrls(toolResultMessage(tool))"
              :attachments="attachments(toolResultMessage(tool))"
              @preview-image="emit('previewImage', $event)"
            />
          </template>
        </div>
      </details>
    </div>
    <div
      v-if="active === undefined && transcript.length === 0"
      class="thread-empty"
    >
      No stored messages for this node.
    </div>
    <ol
      v-if="active"
      ref="list"
      class="thread-transcript"
    >
      <li
        v-for="(message, index) in activeMessages"
        :key="index"
        :class="`role-${message.role}`"
      >
        <div><strong>{{ roleLabel(message.role) }}</strong><code v-if="message.tool_call_id">{{ message.tool_call_id }}</code></div>
        <MessageContent
          :text="messageText(message)"
          :images="imageUrls(message)"
          :attachments="attachments(message)"
          @preview-image="emit('previewImage', $event)"
        />
        <div
          v-if="message.tool_calls?.length"
          class="thread-tool-calls"
        >
          <details
            v-for="call in message.tool_calls"
            :key="call.id"
            class="thread-tool-call"
          >
            <summary>
              <Tag
                :value="call.function.name"
                severity="secondary"
              /><span>Arguments</span>
            </summary>
            <pre><code
class="hljs language-json"
                       :innerHTML="highlightCode(call.function.arguments || '{}', 'json')"
            /></pre>
          </details>
        </div>
      </li>
    </ol>
    <ol
      v-else-if="transcript.length"
      ref="list"
      class="thread-transcript"
    >
      <li
        v-for="(entry, index) in transcript"
        :key="index"
        :class="[`role-${entry.message.role}`, { 'context-selected': contextEntry === entry }]"
        @contextmenu.prevent="showMenu($event, entry)"
      >
        <div>
          <strong>{{ roleLabel(entry.message.role) }}</strong><span class="thread-message-actions"><code v-if="entry.message.tool_call_id">{{ entry.message.tool_call_id }}</code><Button
            v-if="transcriptMessageKey(entry)"
            icon="pi pi-comments"
            severity="secondary"
            text
            rounded
            size="small"
            aria-label="Open in chat logs"
            title="Open in chat logs"
            @click="router.push(chatLogLocation(transcriptMessageKey(entry)!))"
          /></span>
        </div>
        <MessageContent
          :text="messageText(entry.message)"
          :images="imageUrls(entry.message)"
          :attachments="attachments(entry.message)"
          @preview-image="emit('previewImage', $event)"
        />
        <div
          v-if="entry.message.tool_calls?.length"
          class="thread-tool-calls"
        >
          <details
            v-for="call in entry.message.tool_calls"
            :key="call.id"
            class="thread-tool-call"
          >
            <summary>
              <Tag
                :value="call.function.name"
                severity="secondary"
              /><span>Arguments</span>
            </summary>
            <pre><code
class="hljs language-json"
                       :innerHTML="highlightCode(call.function.arguments || '{}', 'json')"
            /></pre>
          </details>
        </div>
      </li>
    </ol>
  </section>
</template>
