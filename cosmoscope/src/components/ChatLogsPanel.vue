<script setup lang="ts">
import { computed, nextTick, onMounted, ref, useTemplateRef, watch } from 'vue'
import { RouterLink, type RouteLocationRaw, useRoute, useRouter } from 'vue-router'
import Button from 'primevue/button'
import ContextMenu, { type ContextMenuMethods } from 'primevue/contextmenu'
import Dialog from 'primevue/dialog'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import DisplayIdentity from '@/components/DisplayIdentity.vue'
import MessageContent from '@/components/MessageContent.vue'
import type { MessageContentAttachment } from '@/components/messageContent'
import PlatformIcon from '@/components/PlatformIcon.vue'
import type { MenuItem } from 'primevue/menuitem'
import { listChatLogs, loadChatLogWindow } from '@/backend/AdminBackend'
import { mergeChatLogItems, safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { runBackend } from '@/backend/runBackend'
import { useConnectionStore } from '@/stores/connection'
import type { AuditPlatform, ChatKind, ChatLogScope, ChatLogSummary, ChatLogWindow, ChatLogWindowQuery } from '@/types/domain'
import { useOverlayLayer } from '@/overlay'


const platformLabels: Readonly<Record<AuditPlatform, string>> = {
  PlatformQQ: 'QQ', PlatformTelegram: 'Telegram', PlatformMatrix: 'Matrix', PlatformDiscord: 'Discord', PlatformRPC: 'RPC', PlatformACP: 'ACP',
}
const route = useRoute()
const router = useRouter()
const connection = useConnectionStore()
const chats = ref<readonly ChatLogSummary[]>([])
const window = ref<ChatLogWindow>()
const loading = ref(true)
const loadingWindow = ref(false)
const loadingDirection = ref<'older' | 'newer'>()
const retainedMessages = 150
const messagePane = ref<HTMLElement>()
const messageMenu = useTemplateRef<ContextMenuMethods>('messageMenu')
const contextItem = ref<ChatLogWindow['entries'][number]>()
const previewImage = ref<string>()
const error = ref('')
let requestGeneration = 0
let listRequest = 0
const { isTop: previewIsTop } = useOverlayLayer(computed(() => previewImage.value !== undefined))
const messageMenuLayer = useOverlayLayer()

const groupedChats = computed(() => [...chats.value.reduce((groups, chat) => {
  const entries = groups.get(chat.scope.platform) ?? []
  groups.set(chat.scope.platform, [...entries, chat])
  return groups
}, new Map<AuditPlatform, ChatLogSummary[]>())].map(([platform, entries]) => ({ platform, entries })))
const messageMenuItems = computed<MenuItem[]>(() => {
  const item = contextItem.value
  if (item === undefined) return []
  return [
    { label: 'Copy text', icon: 'pi pi-copy', disabled: item.entry.text === '', command: () => { void copyText(item.entry.text) } },
    { label: 'Copy message link', icon: 'pi pi-link', disabled: item.entry.messageId === null, command: () => { void copyMessageLink(item) } },
    ...(item.threadId === null ? [] : [{ separator: true }, { label: 'View agent thread', icon: 'pi pi-arrow-up-right', command: () => { void router.push({ name: 'threads', params: { threadId: String(item.threadId) } }) } }]),
  ]
})

function scopeKey(scope: ChatLogScope): string { return `${scope.platform}\u0000${scope.kind}\u0000${scope.chatId ?? ''}` }
function chatKindIcon(kind: ChatKind): string {
  if (kind === 'ChatPrivate') return 'pi pi-user'
  if (kind === 'ChatGroup') return 'pi pi-users'
  if (kind === 'ChatChannel') return 'pi pi-hashtag'
  return 'pi pi-comments'
}
function formatTime(value: string | null): string { return value === null ? 'Unknown time' : new Date(value).toLocaleString() }
function routeValue(value: unknown): string | undefined { return typeof value === 'string' ? value : undefined }
function scopeMatchesRoute(scope: ChatLogScope): boolean {
  return scope.platform === routeValue(route.query['platform'])
    && scope.chatId === (routeValue(route.query['chat']) ?? null)
    && (route.query['kind'] === undefined || scope.kind === routeValue(route.query['kind']))
}
function imageUrl(value: string): string | undefined { return safeImageUrl(value, globalThis.location.href) }
function downloadUrl(value: string): string | undefined { return safeDownloadUrl(value, globalThis.location.href) }
function fileKind(name: string): 'image' | 'audio' | 'video' | 'file' {
  const extension = /\.([^.]+)$/.exec(name.toLowerCase())?.[1] ?? ''
  if (['png', 'jpg', 'jpeg', 'gif', 'webp', 'avif', 'svg'].includes(extension)) return 'image'
  if (['mp3', 'm4a', 'aac', 'ogg', 'oga', 'wav', 'flac', 'opus'].includes(extension)) return 'audio'
  if (['mp4', 'webm', 'mov', 'm4v', 'ogv'].includes(extension)) return 'video'
  return 'file'
}
function entryImages(entry: ChatLogWindow['entries'][number]['entry']): readonly string[] {
  return [...new Set([...entry.imageUrls, ...entry.files.filter(({ name }) => fileKind(name) === 'image').map(({ ref }) => ref)])]
    .flatMap((ref) => imageUrl(ref) ?? [])
}
function entryAttachments(entry: ChatLogWindow['entries'][number]['entry']): MessageContentAttachment[] {
  return entry.files.flatMap((file) => {
    const kind = fileKind(file.name)
    const url = downloadUrl(file.ref)
    if (kind === 'image' || url === undefined) return []
    return [{ key: `${file.name}:${file.ref}`, name: file.name, detail: 'Attachment', mimeType: `${kind}/*`, url }]
  })
}
function messageRoute(entry: ChatLogWindow['entries'][number]['entry'], messageId: string): RouteLocationRaw {
  return { name: 'chat', query: { view: 'logs', platform: entry.platform, kind: entry.kind, ...(entry.chatId === null ? {} : { chat: entry.chatId }), message: messageId } }
}
function showMessageMenu(event: Event, item: ChatLogWindow['entries'][number]): void {
  contextItem.value = item
  messageMenu.value?.show(event)
}
async function copyText(value: string): Promise<void> {
  try { await navigator.clipboard.writeText(value) } catch { error.value = 'Could not copy the message text.' }
}
async function copyMessageLink(item: ChatLogWindow['entries'][number]): Promise<void> {
  if (item.entry.messageId === null) return
  const href = router.resolve(messageRoute(item.entry, item.entry.messageId)).href
  await copyText(new URL(href, globalThis.location.href).href)
}

async function loadWindow(query: ChatLogWindowQuery, generation: number): Promise<ChatLogWindow | undefined> {
  const result = await runBackend(loadChatLogWindow(query))
  if (result._tag === 'Failure') {
    if (generation === requestGeneration) error.value = result.error.message
    return undefined
  }
  return result.value
}

async function selectFromRoute(): Promise<void> {
  const generation = ++requestGeneration
  loadingDirection.value = undefined
  if (chats.value.length === 0) {
    loadingWindow.value = false
    return
  }
  const candidates = chats.value.filter(({ scope }) => scopeMatchesRoute(scope))
  if (candidates.length === 0) {
    loadingWindow.value = false
    const first = chats.value[0]
    if (first !== undefined) await selectChat(first.scope)
    return
  }
  const messageId = routeValue(route.query['message'])
  loadingWindow.value = true
  try {
    for (const { scope } of candidates) {
      const result = await loadWindow({ ...scope, ...(messageId === undefined ? {} : { messageId }), limit: 50 }, generation)
      if (generation !== requestGeneration) return
      if (result !== undefined && (messageId === undefined || result.anchorFound)) {
        window.value = result
        error.value = ''
        await nextTick()
        if (messageId === undefined) messagePane.value?.querySelector('.chat-log-message:last-of-type')?.scrollIntoView({ block: 'start' })
        else messagePane.value?.querySelector('.targeted')?.scrollIntoView({ block: 'center' })
        return
      }
    }
    if (messageId !== undefined) error.value = `Message ${messageId} was not found in this chat.`
  } finally {
    if (generation === requestGeneration) loadingWindow.value = false
  }
}

async function selectChat(scope: ChatLogScope): Promise<void> {
  await router.replace({ name: 'chat', query: { view: 'logs', platform: scope.platform, kind: scope.kind, ...(scope.chatId === null ? {} : { chat: scope.chatId }) } })
}

async function page(direction: 'older' | 'newer'): Promise<void> {
  const current = window.value
  const edge = direction === 'older' ? current?.entries[0] : current?.entries.at(-1)
  if (current === undefined || edge === undefined || loadingDirection.value !== undefined) return
  const generation = requestGeneration
  loadingDirection.value = direction
  const pane = messagePane.value
  const anchorTop = pane?.querySelector<HTMLElement>(`[data-row-id="${String(edge.rowId)}"]`)?.getBoundingClientRect().top
  try {
    const result = await loadWindow({ ...current.scope, [direction === 'older' ? 'beforeRow' : 'afterRow']: edge.rowId, limit: 50 }, generation)
    if (generation !== requestGeneration || window.value !== current || result === undefined) return
    const merged = mergeChatLogItems(current.entries, result.entries, direction, retainedMessages)
    window.value = {
      ...current,
      entries: merged.entries,
      hasOlder: direction === 'older' ? result.hasOlder : current.hasOlder || merged.pruned,
      hasNewer: direction === 'newer' ? result.hasNewer : current.hasNewer || merged.pruned,
    }
    error.value = ''
    if (anchorTop !== undefined) {
      await nextTick()
      const nextPane = messagePane.value
      const nextTop = nextPane?.querySelector<HTMLElement>(`[data-row-id="${String(edge.rowId)}"]`)?.getBoundingClientRect().top
      if (nextPane !== undefined && nextTop !== undefined) nextPane.scrollTop += nextTop - anchorTop
    }
  } finally {
    if (generation === requestGeneration && loadingDirection.value === direction) loadingDirection.value = undefined
  }
}

function loadAtScrollEdge(event: Event): void {
  const pane = event.currentTarget
  if (!(pane instanceof HTMLElement) || window.value === undefined) return
  if (pane.scrollTop < 120 && window.value.hasOlder) void page('older')
  else if (pane.scrollHeight - pane.scrollTop - pane.clientHeight < 120 && window.value.hasNewer) void page('newer')
}

async function refresh(): Promise<void> {
  const request = ++listRequest
  requestGeneration += 1
  loadingDirection.value = undefined
  loadingWindow.value = false
  if (connection.state === 'opening' || connection.state === 'reconnecting') {
    loading.value = chats.value.length === 0
    return
  }
  if (connection.state !== 'authenticated') {
    loading.value = false
    error.value = connection.error || 'Connect to cosmobot to load platform logs.'
    return
  }
  loading.value = chats.value.length === 0
  const result = await runBackend(listChatLogs)
  if (request !== listRequest) return
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  chats.value = result.value
  error.value = ''
  await selectFromRoute()
}

watch(() => [route.query['platform'], route.query['kind'], route.query['chat'], route.query['message']], () => { void selectFromRoute() })
watch([() => connection.state, () => connection.methods], () => { void refresh() })
onMounted(refresh)
</script>

<template>
  <section class="chat-logs-embedded">
    <ContextMenu
      ref="messageMenu"
      :model="messageMenuItems"
      @show="messageMenuLayer.show"
      @hide="messageMenuLayer.hide(); contextItem = undefined"
    />
    <div class="cluster chat-log-actions">
      <span>Read-only platform history</span><Button
        icon="pi pi-refresh"
        label="Refresh"
        severity="secondary"
        outlined
        size="small"
        :loading="loading"
        @click="refresh"
      />
    </div>
    <Message
      v-if="error"
      severity="error"
      :closable="false"
    >
      {{ error }}
    </Message>
    <div
      v-if="loading"
      class="chat-log-layout"
    >
      <Skeleton height="34rem" /><Skeleton height="34rem" />
    </div>
    <Message
      v-else-if="chats.length === 0"
      severity="secondary"
      :closable="false"
    >
      No chat messages have been recorded.
    </Message>
    <div
      v-else
      class="chat-log-layout"
    >
      <nav
        class="panel chat-log-nav"
        aria-label="Chat logs"
      >
        <section
          v-for="group in groupedChats"
          :key="group.platform"
        >
          <h2><PlatformIcon :platform="group.platform" />{{ platformLabels[group.platform] }}</h2>
          <button
            v-for="chat in group.entries"
            :key="scopeKey(chat.scope)"
            type="button"
            :class="{ active: window !== undefined && scopeKey(window.scope) === scopeKey(chat.scope) }"
            @click="selectChat(chat.scope)"
          >
            <span><strong class="chat-log-identity"><i
              :class="chatKindIcon(chat.scope.kind)"
              aria-hidden="true"
            /><DisplayIdentity
              :id="chat.scope.chatId"
              :name="chat.chatDisplayName"
              unknown="Unscoped chat"
            /></strong><small>{{ formatTime(chat.latestAt) }}</small></span><Tag
              :value="String(chat.messageCount)"
              severity="secondary"
              rounded
            />
          </button>
        </section>
      </nav>
      <main class="panel chat-log-view">
        <header
          v-if="window"
          class="chat-log-header"
        >
          <div>
            <strong class="chat-log-identity"><i
              :class="chatKindIcon(window.scope.kind)"
              aria-hidden="true"
            /><DisplayIdentity
              :id="window.scope.chatId"
              :name="window.chatDisplayName"
              unknown="Unscoped chat"
            /></strong><small>{{ platformLabels[window.scope.platform] }} · {{ window.entries.length }} messages in this window</small>
          </div>
          <i
            v-if="loadingWindow"
            class="pi pi-spin pi-spinner"
            aria-label="Loading messages"
          />
        </header>
        <div
          v-if="window"
          ref="messagePane"
          class="chat-log-messages"
          :aria-busy="loadingWindow"
          @scroll.passive="loadAtScrollEdge"
        >
          <Button
            v-if="window.hasOlder"
            label="Load older messages"
            icon="pi pi-angle-up"
            severity="secondary"
            text
            size="small"
            :loading="loadingDirection === 'older'"
            @click="page('older')"
          />
          <article
            v-for="item in window.entries"
            :id="item.entry.messageId === null ? undefined : `message-${item.entry.messageId}`"
            :key="item.rowId"
            :data-row-id="item.rowId"
            :class="['chat-log-message', { targeted: item.entry.messageId === window.anchorMessageId, 'context-selected': contextItem?.rowId === item.rowId }]"
            @contextmenu="showMessageMenu($event, item)"
          >
            <header>
              <span class="platform-icon"><i :class="item.entry.isBot ? 'pi pi-sparkles' : 'pi pi-user'" /></span>
              <div>
                <strong><template v-if="item.entry.isBot">Cosmobot</template><DisplayIdentity
                  v-else
                  :id="item.entry.senderId"
                  :name="item.entry.senderDisplayName"
                  unknown="Unknown sender"
                /></strong>
              </div>
              <time :datetime="item.entry.recordedAt ?? undefined">{{ formatTime(item.entry.recordedAt) }}</time>
              <RouterLink
                v-if="item.entry.messageId"
                :to="messageRoute(item.entry, item.entry.messageId)"
              >
                <code>{{ item.entry.messageId }}</code>
              </RouterLink>
              <Button
                v-if="item.threadId !== null"
                icon="pi pi-sitemap"
                severity="secondary"
                text
                rounded
                size="small"
                aria-label="View agent thread"
                title="View agent thread"
                @click="router.push({ name: 'threads', params: { threadId: String(item.threadId) } })"
              />
            </header>
            <RouterLink
              v-if="item.entry.replyToMessageId"
              class="chat-log-reply"
              :to="messageRoute(item.entry, item.entry.replyToMessageId)"
            >
              <i
                class="pi pi-reply"
                aria-hidden="true"
              />
              <span>In reply to</span>
              <code>{{ item.entry.replyToMessageId }}</code>
            </RouterLink>
            <MessageContent
              :text="item.entry.text"
              :images="entryImages(item.entry)"
              :attachments="entryAttachments(item.entry)"
              @preview-image="previewImage = $event"
            />
          </article>
          <Button
            v-if="window.hasNewer"
            label="Load newer messages"
            icon="pi pi-angle-down"
            icon-pos="right"
            severity="secondary"
            text
            size="small"
            :loading="loadingDirection === 'newer'"
            @click="page('newer')"
          />
        </div>
        <div
          v-else
          class="empty-state"
        >
          <i class="pi pi-comments" /><strong>Select a chat log</strong><span>Choose a conversation to inspect its messages.</span>
        </div>
      </main>
    </div>
    <Dialog
      :visible="previewImage !== undefined"
      modal
      dismissable-mask
      header="Image preview"
      class="image-preview-dialog"
      :close-on-escape="previewIsTop"
      @update:visible="previewImage = undefined"
    >
      <img
        v-if="previewImage"
        :src="previewImage"
        alt="Full-size chat image"
        class="object-preview"
      />
    </Dialog>
  </section>
</template>
