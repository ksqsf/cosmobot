<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, reactive, ref, useTemplateRef, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useConfirm } from 'primevue/useconfirm'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import ContextMenu, { type ContextMenuMethods } from 'primevue/contextmenu'
import Dialog from 'primevue/dialog'
import IconField from 'primevue/iconfield'
import InputIcon from 'primevue/inputicon'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import ProgressSpinner from 'primevue/progressspinner'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import Textarea from 'primevue/textarea'
import type { MenuItem } from 'primevue/menuitem'
import PageHeading from '@/components/PageHeading.vue'
import ChatLogsPanel from '@/components/ChatLogsPanel.vue'
import { deleteChatSession, discardChatAttachment, forkChatSession, listChatSessions, loadChatHistory, openChatSession, renameChatSession, sendChatMessage, subscribeChat, uploadChatAttachment } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { mergeChatMessage, safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { useConnectionStore } from '@/stores/connection'
import { chatMethods } from '@/rpc/protocol'
import { renderMarkdown } from '@/markdown'
import type { ChatAttachment, ChatMessage, ChatSession } from '@/types/domain'

const maxAttachmentBytes = 25 * 1024 * 1024
const route = useRoute()
const router = useRouter()
const connection = useConnectionStore()
const confirm = useConfirm()
const toast = useToast()
const attachmentInput = useTemplateRef<HTMLInputElement>('attachmentInput')
const messagePane = useTemplateRef<HTMLElement>('messagePane')
const messageMenu = useTemplateRef<ContextMenuMethods>('messageMenu')
const sessions = ref<readonly ChatSession[]>([])
const messages = ref<readonly ChatMessage[]>([])
const selectedId = ref<string>()
const query = ref('')
const error = ref('')
const sessionsLoading = ref(true)
const sessionsLoaded = ref(false)
const loading = ref(false)
const loadingOlder = ref(false)
const hasOlder = ref(false)
const sending = ref(false)
const uploading = ref(false)
const pendingAttachments = ref<readonly ChatAttachment[]>([])
const previewImage = ref<string>()
const contextMessage = ref<ChatMessage>()
const drafts = reactive(new Map<string, string>())
const streamingMessageIds = ref<ReadonlySet<string>>(new Set())
let stopSubscription: (() => void) | undefined
let selectionGeneration = 0

const live = computed(() => connection.state === 'authenticated' && chatMethods.every((method) => connection.methods.has(method)))
const showingPlatformLogs = computed(() => route.query['view'] === 'logs')
const selectedSession = computed(() => sessions.value.find(({ sessionId }) => sessionId === selectedId.value))
const filteredSessions = computed(() => sessions.value.filter((session) => sessionName(session).toLowerCase().includes(query.value.toLowerCase())))
const draft = computed({
  get: () => selectedId.value === undefined ? '' : drafts.get(selectedId.value) ?? '',
  set: (value: string) => { if (selectedId.value !== undefined) drafts.set(selectedId.value, value) },
})
const messageMenuItems: MenuItem[] = [
  { label: 'Copy text', icon: 'pi pi-copy', command: () => { void copyContextText() } },
  { separator: true },
  { label: 'Fork conversation here', icon: 'pi pi-code-branch', command: () => { if (contextMessage.value !== undefined) void forkAt(contextMessage.value) } },
]

function sessionName(session: ChatSession): string {
  const label = session.label?.trim()
  return label === undefined || label === '' ? session.sessionId : label
}

async function showChatView(view: 'sessions' | 'logs'): Promise<void> {
  if (view === 'logs') await router.push({ name: 'chat', query: { view: 'logs' } })
  else {
    await router.push({ name: 'chat', params: selectedId.value === undefined ? {} : { sessionId: selectedId.value } })
    await loadSessions(selectedId.value)
  }
}

function mergeMessage(message: ChatMessage): void {
  messages.value = mergeChatMessage(messages.value, message)
}

function finishMessage(messageId: string): void {
  const next = new Set(streamingMessageIds.value)
  next.delete(messageId)
  streamingMessageIds.value = next
}

async function refreshHistory(sessionId: string, generation: number): Promise<void> {
  const result = await runBackend(loadChatHistory(sessionId))
  if (generation !== selectionGeneration || selectedId.value !== sessionId) return
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  streamingMessageIds.value = new Set()
  messages.value = [...result.value.messages]
  hasOlder.value = result.value.hasOlder
  await nextTick()
  messagePane.value?.querySelector('.message:last-of-type')?.scrollIntoView({ block: 'start' })
}

async function loadOlder(): Promise<void> {
  const sessionId = selectedId.value
  const generation = selectionGeneration
  const oldest = messages.value[0]
  const pane = messagePane.value
  if (sessionId === undefined || oldest === undefined || loadingOlder.value || !hasOlder.value) return
  const previousHeight = pane?.scrollHeight ?? 0
  loadingOlder.value = true
  const result = await runBackend(loadChatHistory(sessionId, oldest.messageId))
  if (generation !== selectionGeneration || selectedId.value !== sessionId) return
  loadingOlder.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  const known = new Set(messages.value.map(({ messageId }) => messageId))
  messages.value = [...result.value.messages.filter(({ messageId }) => !known.has(messageId)), ...messages.value]
  hasOlder.value = result.value.hasOlder
  await nextTick()
  if (pane !== null) pane.scrollTop += pane.scrollHeight - previousHeight
}

function loadOlderAtTop(event: Event): void {
  const pane = event.currentTarget
  if (pane instanceof HTMLElement && pane.scrollTop < 120) void loadOlder()
}

async function selectSession(sessionId: string): Promise<void> {
  if (selectedId.value === sessionId && stopSubscription !== undefined) return
  if (!await discardPendingAttachments()) return
  stopSubscription?.()
  stopSubscription = undefined
  selectedId.value = sessionId
  messages.value = []
  hasOlder.value = false
  loadingOlder.value = false
  streamingMessageIds.value = new Set()
  loading.value = true
  const generation = ++selectionGeneration
  await router.replace({ name: 'chat', params: { sessionId } })
  const result = await runBackend(subscribeChat(
    sessionId,
    () => refreshHistory(sessionId, generation),
    (message) => {
      if (selectedId.value !== sessionId) return
      mergeMessage(message)
      if (message.sender === 'assistant') streamingMessageIds.value = new Set(streamingMessageIds.value).add(message.messageId)
    },
    (messageId) => { if (selectedId.value === sessionId) finishMessage(messageId) },
  ))
  if (generation !== selectionGeneration) {
    if (result._tag === 'Success') result.value()
    return
  }
  if (result._tag === 'Success') stopSubscription = result.value
  else { loading.value = false; error.value = result.error.message }
}

async function loadSessions(preferredId?: string): Promise<void> {
  if (showingPlatformLogs.value) { sessionsLoading.value = false; return }
  if (connection.state === 'opening' || connection.state === 'reconnecting') { sessionsLoading.value = true; return }
  if (!live.value) {
    sessionsLoading.value = false
    error.value = connection.state === 'authenticated'
      ? 'The server does not provide every Chat RPC method required by this page.'
      : connection.error || 'Connect to cosmobot to load chat sessions.'
    return
  }
  sessionsLoading.value = true
  const result = await runBackend(listChatSessions)
  sessionsLoading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  sessions.value = [...result.value]
  sessionsLoaded.value = true
  const routeId = typeof route.params['sessionId'] === 'string' ? route.params['sessionId'] : undefined
  const requestedId = preferredId ?? routeId
  const nextId = requestedId !== undefined && result.value.some(({ sessionId }) => sessionId === requestedId)
    ? requestedId
    : result.value[0]?.sessionId
  if (nextId !== undefined) await selectSession(nextId)
  else {
    await discardPendingAttachments()
    stopSubscription?.()
    stopSubscription = undefined
    selectedId.value = undefined
    messages.value = []
    hasOlder.value = false
    loadingOlder.value = false
    await router.replace({ name: 'chat' })
  }
}

async function createSession(): Promise<void> {
  const result = await runBackend(openChatSession('New session'))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  await loadSessions(result.value.sessionId)
}

async function renameSelected(): Promise<void> {
  const session = selectedSession.value
  if (session === undefined) return
  const label = window.prompt('Session name', sessionName(session))?.trim()
  if (!label || label === session.label) return
  const result = await runBackend(renameChatSession(session.sessionId, label))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  sessions.value = sessions.value.map((item) => item.sessionId === result.value.sessionId ? result.value : item)
}

async function forkAt(message: ChatMessage): Promise<void> {
  const parentName = selectedSession.value === undefined ? message.sessionId : sessionName(selectedSession.value)
  const label = window.prompt('Fork name', `Fork of ${parentName}`)?.trim()
  if (label === undefined) return
  const result = await runBackend(forkChatSession(message.sessionId, message.messageId, label || undefined))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  await loadSessions(result.value.sessionId)
}

function showMessageMenu(event: Event, message: ChatMessage): void {
  contextMessage.value = message
  messageMenu.value?.show(event)
}

async function copyContextText(): Promise<void> {
  const text = contextMessage.value?.text
  if (!text) return
  try {
    await navigator.clipboard.writeText(text)
    toast.add({ severity: 'success', summary: 'Copied', detail: 'Message text copied.', life: 1800 })
  } catch {
    error.value = 'Could not copy the message text.'
  }
}

function requestDelete(): void {
  const session = selectedSession.value
  if (session === undefined) return
  confirm.require({
    header: `Delete “${sessionName(session)}”?`,
    message: 'This permanently deletes the session, its transcript, and stored attachment references.',
    rejectLabel: 'Keep session',
    acceptLabel: 'Delete session',
    acceptClass: 'p-button-danger',
    accept: () => { void removeSession(session.sessionId) },
  })
}

async function removeSession(sessionId: string): Promise<void> {
  const result = await runBackend(deleteChatSession(sessionId))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  if (!result.value) { error.value = 'The session no longer exists.'; return }
  drafts.delete(sessionId)
  await loadSessions()
}

async function attachFiles(event: Event): Promise<void> {
  const input = event.currentTarget
  if (!(input instanceof HTMLInputElement) || input.files === null) return
  const files = [...input.files]
  input.value = ''
  const oversized = files.find(({ size }) => size > maxAttachmentBytes)
  if (oversized !== undefined) {
    toast.add({ severity: 'error', summary: `${oversized.name} exceeds the 25 MiB limit`, life: 3500 })
    return
  }
  uploading.value = true
  for (const file of files) {
    const result = await runBackend(uploadChatAttachment(file))
    if (result._tag === 'Failure') { error.value = result.error.message; break }
    pendingAttachments.value = [...pendingAttachments.value, result.value]
  }
  uploading.value = false
}

async function discardAttachment(attachment: ChatAttachment): Promise<void> {
  const result = await runBackend(discardChatAttachment(attachment.attachmentId))
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  pendingAttachments.value = pendingAttachments.value.filter(({ attachmentId }) => attachmentId !== attachment.attachmentId)
}

async function discardPendingAttachments(): Promise<boolean> {
  const attachments = pendingAttachments.value
  const results = await Promise.all(attachments.map(({ attachmentId }) => runBackend(discardChatAttachment(attachmentId))))
  const retained = attachments.filter((_attachment, index) => results[index]?._tag === 'Failure')
  pendingAttachments.value = retained
  if (retained.length > 0) error.value = 'Could not discard every pending attachment.'
  return retained.length === 0
}

async function send(): Promise<void> {
  const sessionId = selectedId.value
  const text = draft.value.trim()
  const attachments = pendingAttachments.value
  if (sessionId === undefined || sending.value || (text === '' && attachments.length === 0)) return
  sending.value = true
  const result = await runBackend(sendChatMessage({ sessionId, text, attachments }))
  sending.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  if (!messages.value.some(({ messageId }) => messageId === result.value)) {
    mergeMessage({ sessionId, messageId: result.value, sender: 'user', text, imageUrls: [], attachments, replyToMessageId: null, parentMessageId: null })
  }
  draft.value = ''
  pendingAttachments.value = []
}

function imageUrls(message: ChatMessage): readonly string[] {
  return [...new Set([...message.imageUrls, ...message.attachments.filter(({ kind }) => kind === 'image').map(({ url }) => url)])]
    .flatMap((url) => safeImageUrl(url, window.location.href) ?? [])
}

function documentAttachments(message: ChatMessage): readonly ChatAttachment[] {
  return message.attachments.filter(({ kind }) => kind !== 'image')
}

function downloadUrl(value: string): string | undefined {
  return safeDownloadUrl(value, window.location.href)
}

function previewMarkdownImage(event: MouseEvent): void {
  if (!(event.target instanceof HTMLImageElement)) return
  previewImage.value = safeImageUrl(event.target.currentSrc || event.target.src, window.location.href)
}

watch([() => connection.state, () => connection.methods], ([state]) => {
  void loadSessions()
  if (state === 'offline' || state === 'failed') {
    stopSubscription?.()
    stopSubscription = undefined
    selectedId.value = undefined
    sessions.value = []
    messages.value = []
    hasOlder.value = false
    loadingOlder.value = false
    sessionsLoaded.value = false
  }
})
watch(() => route.params['sessionId'], (sessionId) => {
  if (typeof sessionId === 'string' && sessionId !== selectedId.value && sessions.value.some((session) => session.sessionId === sessionId)) void selectSession(sessionId)
})
onMounted(() => { void loadSessions() })
onUnmounted(() => { stopSubscription?.(); selectionGeneration += 1; void discardPendingAttachments() })
</script>

<template>
  <section class="page chat-page">
    <PageHeading
      eyebrow="Conversations"
      title="Chat"
      :description="showingPlatformLogs ? 'Inspect read-only conversations from connected platforms.' : 'Use durable RPC sessions and their live transcripts.'"
    >
      <Button
        label="Platform logs"
        icon="pi pi-history"
        :outlined="!showingPlatformLogs"
        @click="showChatView('logs')"
      /><Button
        label="RPC sessions"
        icon="pi pi-desktop"
        severity="secondary"
        :outlined="showingPlatformLogs"
        @click="showChatView('sessions')"
      /><Button
        v-if="!showingPlatformLogs"
        label="New session"
        icon="pi pi-plus"
        :disabled="!live"
        @click="createSession"
      />
    </PageHeading>
    <ChatLogsPanel
      v-if="showingPlatformLogs"
    />
    <template v-else>
      <article
        v-if="sessionsLoading && !sessionsLoaded"
        class="panel manager-loading"
        aria-label="Loading chat sessions"
      >
        <Skeleton height="3rem" /><Skeleton height="22rem" />
      </article>
      <Message
        v-if="!live"
        severity="error"
        :closable="false"
      >
        {{ error }}
      </Message>
      <Message
        v-else-if="error"
        severity="error"
        :closable="false"
      >
        {{ error }}
      </Message>
      <div
        v-if="sessionsLoaded"
        class="chat-layout panel"
      >
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
            @click="selectSession(session.sessionId)"
          >
            <span class="platform-icon">R</span>
            <span><strong>{{ sessionName(session) }}</strong><small><code>{{ session.sessionId }}</code></small></span>
          </Button>
          <Message
            v-if="sessions.length === 0"
            severity="secondary"
            :closable="false"
          >
            No RPC sessions yet.
          </Message>
        </aside>

        <section
          class="transcript"
          aria-label="Chat transcript"
        >
          <header class="transcript-header">
            <div><strong>{{ selectedSession ? sessionName(selectedSession) : 'Select a session' }}</strong><small v-if="selectedSession"><code>{{ selectedSession.sessionId }}</code></small></div>
            <div v-if="selectedSession">
              <Button
                icon="pi pi-pencil"
                text
                rounded
                aria-label="Rename session"
                @click="renameSelected"
              /><Button
                icon="pi pi-trash"
                severity="danger"
                text
                rounded
                aria-label="Delete session"
                @click="requestDelete"
              />
            </div>
          </header>
          <ContextMenu
            ref="messageMenu"
            :model="messageMenuItems"
            @hide="contextMessage = undefined"
          />
          <div
            ref="messagePane"
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
              @click="loadOlder"
            />
            <Message
              v-if="loading"
              severity="secondary"
              :closable="false"
            >
              Loading transcript…
            </Message>
            <Message
              v-else-if="selectedSession && messages.length === 0"
              severity="secondary"
              :closable="false"
            >
              This session has no messages yet.
            </Message>
            <article
              v-for="message in messages"
              :key="message.messageId"
              class="message"
              :class="[message.sender === 'user' ? 'user' : 'bot', { 'context-selected': contextMessage?.messageId === message.messageId }]"
              tabindex="0"
              @contextmenu.prevent="showMessageMenu($event, message)"
            >
              <header class="message-meta">
                <span class="avatar">{{ message.sender === 'user' ? 'Y' : 'C' }}</span><strong>{{ message.sender === 'user' ? 'You' : 'Cosmobot' }}</strong><ProgressSpinner
                  v-if="streamingMessageIds.has(message.messageId)"
                  aria-label="Streaming response"
                  class="chat-streaming"
                />
              </header>
              <div
                class="message-body"
                @click="previewMarkdownImage"
              >
                <div
                  v-if="message.text"
                  class="markdown-body"
                  :innerHTML="renderMarkdown(message.text)"
                />
                <div
                  v-if="imageUrls(message).length > 0"
                  class="chat-images"
                >
                  <button
                    v-for="url in imageUrls(message)"
                    :key="url"
                    type="button"
                    class="chat-image-button"
                    aria-label="Zoom image"
                    @click="previewImage = url"
                  >
                    <img
                      :src="url"
                      alt="Chat attachment"
                      loading="lazy"
                    />
                  </button>
                </div>
                <div
                  v-if="documentAttachments(message).length > 0"
                  class="chat-files"
                >
                  <template
                    v-for="attachment in documentAttachments(message)"
                    :key="attachment.attachmentId"
                  >
                    <a
                      v-if="downloadUrl(attachment.url)"
                      class="chat-file-card"
                      :href="downloadUrl(attachment.url)"
                      target="_blank"
                      rel="noopener noreferrer"
                      download
                    >
                      <i class="pi pi-file" /><span><strong>{{ attachment.name }}</strong><small>{{ attachment.mediaType }} · {{ Math.ceil(attachment.size / 1024) }} KiB</small></span><i class="pi pi-download" />
                    </a>
                    <div
                      v-else
                      class="chat-file-card"
                    >
                      <i class="pi pi-file" /><span><strong>{{ attachment.name }}</strong><small>Unsafe download URL rejected</small></span>
                    </div>
                  </template>
                </div>
              </div>
            </article>
          </div>
          <form
            class="composer"
            @submit.prevent="send"
          >
            <div
              v-if="pendingAttachments.length > 0"
              class="tag-list"
            >
              <span
                v-for="attachment in pendingAttachments"
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
                @click="discardAttachment(attachment)"
              /></span>
            </div>
            <Textarea
              v-model="draft"
              rows="3"
              placeholder="Message cosmobot…"
              aria-label="Message cosmobot"
              class="composer-input"
              :disabled="selectedSession === undefined"
              fluid
              @keydown.ctrl.enter.prevent="send"
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
                  :disabled="selectedSession === undefined || uploading"
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
                :disabled="selectedSession === undefined || uploading || sending || (draft.trim() === '' && pendingAttachments.length === 0)"
              />
            </div>
          </form>
        </section>
      </div>
      <Dialog
        :visible="previewImage !== undefined"
        modal
        dismissable-mask
        header="Image preview"
        class="image-preview-dialog"
        @update:visible="previewImage = undefined"
      >
        <img
          v-if="previewImage"
          :src="previewImage"
          alt="Full-size chat attachment"
          class="object-preview"
        />
      </Dialog>
    </template>
  </section>
</template>
