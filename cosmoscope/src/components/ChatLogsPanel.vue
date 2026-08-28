<script setup lang="ts">
import { computed, nextTick, onMounted, ref, useTemplateRef, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import Button from 'primevue/button'
import ContextMenu, { type ContextMenuMethods } from 'primevue/contextmenu'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import type { MenuItem } from 'primevue/menuitem'
import { listChatLogs, loadChatLogWindow } from '@/backend/AdminBackend'
import { safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { runBackend } from '@/backend/runBackend'
import { renderMarkdown } from '@/markdown'
import type { AuditPlatform, ChatLogScope, ChatLogSummary, ChatLogWindow, ChatLogWindowQuery } from '@/types/domain'


const platformLabels: Readonly<Record<AuditPlatform, string>> = {
  PlatformQQ: 'QQ', PlatformTelegram: 'Telegram', PlatformMatrix: 'Matrix', PlatformDiscord: 'Discord', PlatformRPC: 'RPC', PlatformACP: 'ACP',
}
const platformIcons: Readonly<Record<AuditPlatform, string>> = {
  PlatformQQ: 'pi pi-comments', PlatformTelegram: 'pi pi-send', PlatformMatrix: 'pi pi-th-large', PlatformDiscord: 'pi pi-comments', PlatformRPC: 'pi pi-desktop', PlatformACP: 'pi pi-code',
}
const route = useRoute()
const router = useRouter()
const chats = ref<readonly ChatLogSummary[]>([])
const window = ref<ChatLogWindow>()
const loading = ref(true)
const loadingWindow = ref(false)
const loadingDirection = ref<'older' | 'newer'>()
const messagePane = ref<HTMLElement>()
const messageMenu = useTemplateRef<ContextMenuMethods>('messageMenu')
const contextItem = ref<ChatLogWindow['entries'][number]>()
const error = ref('')
let requestGeneration = 0

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
function chatLabel(scope: ChatLogScope): string {
  const kind = scope.kind === 'ChatPrivate' ? 'Direct message' : scope.kind === 'ChatGroup' ? 'Group' : scope.kind === 'ChatChannel' ? 'Channel' : 'Chat'
  return `${kind} ${scope.chatId ?? 'without ID'}`
}
function formatTime(value: string | null): string { return value === null ? 'Unknown time' : new Date(value).toLocaleString() }
function senderLabel(entry: ChatLogWindow['entries'][number]['entry']): string { return entry.senderUsername ?? entry.senderId ?? (entry.isBot ? 'Cosmobot' : 'Unknown sender') }
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
function showMessageMenu(event: Event, item: ChatLogWindow['entries'][number]): void {
  contextItem.value = item
  messageMenu.value?.show(event)
}
async function copyText(value: string): Promise<void> {
  try { await navigator.clipboard.writeText(value) } catch { error.value = 'Could not copy the message text.' }
}
async function copyMessageLink(item: ChatLogWindow['entries'][number]): Promise<void> {
  if (item.entry.messageId === null) return
  const href = router.resolve({ name: 'chat', query: { view: 'logs', platform: item.entry.platform, kind: item.entry.kind, ...(item.entry.chatId === null ? {} : { chat: item.entry.chatId }), message: item.entry.messageId } }).href
  await copyText(new URL(href, globalThis.location.href).href)
}

async function loadWindow(query: ChatLogWindowQuery): Promise<ChatLogWindow | undefined> {
  const result = await runBackend(loadChatLogWindow(query))
  if (result._tag === 'Failure') { error.value = result.error.message; return undefined }
  return result.value
}

async function selectFromRoute(): Promise<void> {
  if (chats.value.length === 0) return
  const candidates = chats.value.filter(({ scope }) => scopeMatchesRoute(scope))
  if (candidates.length === 0) {
    const first = chats.value[0]
    if (first !== undefined) await selectChat(first.scope)
    return
  }
  const generation = ++requestGeneration
  const messageId = routeValue(route.query['message'])
  loadingWindow.value = true
  for (const { scope } of candidates) {
    const result = await loadWindow({ ...scope, ...(messageId === undefined ? {} : { messageId }), limit: 50 })
    if (generation !== requestGeneration) return
    if (result !== undefined && (messageId === undefined || result.anchorFound)) {
      window.value = result
      error.value = ''
      loadingWindow.value = false
      await nextTick()
      if (messageId === undefined && messagePane.value !== undefined) messagePane.value.scrollTop = messagePane.value.scrollHeight
      else messagePane.value?.querySelector('.targeted')?.scrollIntoView({ block: 'center' })
      return
    }
  }
  loadingWindow.value = false
  if (messageId !== undefined) error.value = `Message ${messageId} was not found in this chat.`
}

async function selectChat(scope: ChatLogScope): Promise<void> {
  await router.replace({ name: 'chat', query: { view: 'logs', platform: scope.platform, kind: scope.kind, ...(scope.chatId === null ? {} : { chat: scope.chatId }) } })
}

async function page(direction: 'older' | 'newer'): Promise<void> {
  const current = window.value
  const edge = direction === 'older' ? current?.entries[0] : current?.entries.at(-1)
  if (current === undefined || edge === undefined || loadingDirection.value !== undefined) return
  loadingDirection.value = direction
  const previousHeight = messagePane.value?.scrollHeight ?? 0
  const result = await loadWindow({ ...current.scope, [direction === 'older' ? 'beforeRow' : 'afterRow']: edge.rowId, limit: 50 })
  loadingDirection.value = undefined
  if (result === undefined) return
  const entries = [...new Map([...current.entries, ...result.entries].map((item) => [item.rowId, item])).values()].sort((left, right) => left.rowId - right.rowId)
  window.value = {
    ...current,
    entries,
    hasOlder: direction === 'older' ? result.hasOlder : current.hasOlder,
    hasNewer: direction === 'newer' ? result.hasNewer : current.hasNewer,
  }
  error.value = ''
  if (direction === 'older') {
    await nextTick()
    if (messagePane.value !== undefined) messagePane.value.scrollTop += messagePane.value.scrollHeight - previousHeight
  }
}

function loadAtScrollEdge(event: Event): void {
  const pane = event.currentTarget
  if (!(pane instanceof HTMLElement) || window.value === undefined) return
  if (pane.scrollTop < 120 && window.value.hasOlder) void page('older')
  else if (pane.scrollHeight - pane.scrollTop - pane.clientHeight < 120 && window.value.hasNewer) void page('newer')
}

async function refresh(): Promise<void> {
  const result = await runBackend(listChatLogs)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  chats.value = result.value
  error.value = ''
  await selectFromRoute()
}

watch(() => [route.query['platform'], route.query['kind'], route.query['chat'], route.query['message']], () => { void selectFromRoute() })
onMounted(refresh)
</script>

<!-- eslint-disable vue/no-v-html -- renderMarkdown disables raw HTML. -->
<template>
  <section class="chat-logs-embedded">
    <ContextMenu
      ref="messageMenu"
      :model="messageMenuItems"
      @hide="contextItem = undefined"
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
          <h2><i :class="platformIcons[group.platform]" />{{ platformLabels[group.platform] }}</h2>
          <button
            v-for="chat in group.entries"
            :key="scopeKey(chat.scope)"
            type="button"
            :class="{ active: window !== undefined && scopeKey(window.scope) === scopeKey(chat.scope) }"
            @click="selectChat(chat.scope)"
          >
            <span><strong>{{ chatLabel(chat.scope) }}</strong><small>{{ formatTime(chat.latestAt) }}</small></span><Tag
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
          <div><strong>{{ chatLabel(window.scope) }}</strong><small>{{ platformLabels[window.scope.platform] }} · {{ window.entries.length }} messages in this window</small></div>
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
            :class="['chat-log-message', { targeted: item.entry.messageId === window.anchorMessageId, 'context-selected': contextItem?.rowId === item.rowId }]"
            @contextmenu="showMessageMenu($event, item)"
          >
            <header>
              <span class="platform-icon"><i :class="item.entry.isBot ? 'pi pi-sparkles' : 'pi pi-user'" /></span>
              <div><strong>{{ senderLabel(item.entry) }}</strong><small>{{ item.entry.senderId ?? (item.entry.isBot ? 'bot' : 'unknown sender') }}</small></div>
              <time :datetime="item.entry.recordedAt ?? undefined">{{ formatTime(item.entry.recordedAt) }}</time>
              <RouterLink
                v-if="item.entry.messageId"
                :to="{ name: 'chat', query: { view: 'logs', platform: item.entry.platform, kind: item.entry.kind, ...(item.entry.chatId === null ? {} : { chat: item.entry.chatId }), message: item.entry.messageId } }"
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
            <div
              v-if="item.entry.text"
              class="markdown-body"
              v-html="renderMarkdown(item.entry.text)"
            />
            <div
              v-if="entryImages(item.entry).length > 0"
              class="chat-log-media"
            >
              <a
                v-for="url in entryImages(item.entry)"
                :key="url"
                :href="url"
                target="_blank"
                rel="noopener noreferrer"
              ><img
                :src="url"
                alt="Chat attachment"
                loading="lazy"
              /></a>
            </div>
            <div
              v-if="item.entry.files.some(({ name }) => fileKind(name) !== 'image')"
              class="chat-files"
            >
              <template
                v-for="file in item.entry.files.filter(({ name }) => fileKind(name) !== 'image')"
                :key="`${file.name}:${file.ref}`"
              >
                <audio
                  v-if="fileKind(file.name) === 'audio' && downloadUrl(file.ref)"
                  controls
                  preload="metadata"
                  :src="downloadUrl(file.ref)"
                />
                <video
                  v-else-if="fileKind(file.name) === 'video' && downloadUrl(file.ref)"
                  controls
                  preload="metadata"
                  :src="downloadUrl(file.ref)"
                />
                <a
                  v-else-if="downloadUrl(file.ref)"
                  class="chat-file-card"
                  :href="downloadUrl(file.ref)"
                  target="_blank"
                  rel="noopener noreferrer"
                  download
                ><i class="pi pi-file" /><span><strong>{{ file.name }}</strong><small>Attachment</small></span><i class="pi pi-download" /></a>
              </template>
            </div>
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
  </section>
</template>
