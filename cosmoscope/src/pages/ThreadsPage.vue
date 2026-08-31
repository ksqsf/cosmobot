<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Dialog from 'primevue/dialog'
import Drawer from 'primevue/drawer'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import type { TreeNode as PrimeTreeNode } from 'primevue/treenode'
import PageHeading from '@/components/PageHeading.vue'
import ChatLogMessageLink from '@/components/ChatLogMessageLink.vue'
import DisplayIdentity from '@/components/DisplayIdentity.vue'
import PlatformIcon from '@/components/PlatformIcon.vue'
import RunIdLink from '@/components/RunIdLink.vue'
import ActiveThreadsPanel from '@/components/threads/ActiveThreadsPanel.vue'
import ThreadListPanel from '@/components/threads/ThreadListPanel.vue'
import ThreadStatsPanel from '@/components/threads/ThreadStatsPanel.vue'
import ThreadTranscript from '@/components/threads/ThreadTranscript.vue'
import ThreadTreePanel from '@/components/threads/ThreadTreePanel.vue'
import { haltActiveThread, resolveThreadRun } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { buildActiveTree, activeTreeKey } from '@/domain/threadTree'
import { useActiveThreads } from '@/composables/useActiveThreads'
import { useThreadInspector } from '@/composables/useThreadInspector'
import { useThreadList } from '@/composables/useThreadList'
import { useTranscriptMedia } from '@/composables/useTranscriptMedia'
import { useConnectionStore } from '@/stores/connection'
import { useLayeredConfirm, useOverlayLayer } from '@/overlay'
import type { ActiveThread, AuditPlatform, ThreadSummary } from '@/types/domain'
import type { LatestToken } from '@/async'

interface TranscriptView { isPinned: () => boolean, scrollToEnd: () => Promise<void> }

const route = useRoute()
const router = useRouter()
const confirm = useLayeredConfirm()
const toast = useToast()
const connection = useConnectionStore()
const transcriptView = ref<TranscriptView>()
const haltingTaskId = ref<number>()
const media = useTranscriptMedia()
const inspector = useThreadInspector(clearActiveSelection, media.reset, media.loadMediaForMessages)
const active = useActiveThreads({
  visible: inspector.visible,
  inspectorLatest: inspector.latest,
  loadMediaForMessages: media.loadMediaForMessages,
  transcriptIsPinned: () => transcriptView.value?.isPinned() ?? true,
  scrollTranscriptToEnd: async () => { await transcriptView.value?.scrollToEnd() },
  onFinalized: openFinalizedThread,
})
const list = useThreadList(selectFromRoute)
let finalizingRunId: string | undefined
const { isTop: detailIsTop } = useOverlayLayer(inspector.visible)
const { isTop: previewIsTop } = useOverlayLayer(computed(() => media.previewImage.value !== undefined))

const platformNames = {
  PlatformQQ: 'QQ', PlatformTelegram: 'Telegram', PlatformMatrix: 'Matrix',
  PlatformDiscord: 'Discord', PlatformRPC: 'RPC', PlatformACP: 'ACP',
} satisfies Record<AuditPlatform, string>
const allPlatforms: readonly AuditPlatform[] = ['PlatformQQ', 'PlatformTelegram', 'PlatformMatrix', 'PlatformDiscord', 'PlatformRPC', 'PlatformACP']
const platformOptions = computed(() => allPlatforms.map((value) => ({ label: platformNames[value], value })))
const inspectedActiveThreads = computed(() => inspector.detail.value === undefined
  ? active.selected.value === undefined ? [] : [active.selected.value]
  : active.activeThreads.value.filter(({ parentThreadId }) => parentThreadId === inspector.detail.value?.summary.threadId))
const treeNodes = computed(() => buildActiveTree(inspector.detail.value?.nodes ?? [], inspectedActiveThreads.value))
const inspectorStats = computed(() => active.selected.value === undefined ? inspector.stats.value : active.stats.value)

async function monitor(thread: ActiveThread, token = inspector.latest.begin()): Promise<void> {
  if (!inspector.latest.current(token)) return
  if (thread.parentThreadId === null) {
    active.clear()
    inspector.detail.value = undefined
    inspector.detailError.value = ''
    inspector.detailLoading.value = false
    inspector.visible.value = true
  } else if (inspector.detail.value?.summary.threadId !== thread.parentThreadId) {
    await inspectThread(thread.parentThreadId, token)
  }
  if (!inspector.latest.current(token)) return
  active.select(thread, token)
  inspector.selectedNode.value = undefined
  inspector.selectedKeys.value = { [activeTreeKey(thread.taskId)]: true }
  void transcriptView.value?.scrollToEnd()
}

async function openFinalizedThread(thread: ActiveThread, token: LatestToken): Promise<void> {
  if (finalizingRunId === thread.runId) return
  finalizingRunId = thread.runId
  try {
    const result = await runBackend(resolveThreadRun(thread.runId))
    if (!inspector.latest.current(token) || active.selectionToken() !== token || !inspector.visible.value || active.activeTaskId.value !== thread.taskId) return
    if (result._tag === 'Failure') { active.auditError.value = result.error.message; return }
    if (result.value.threadId === null) return
    active.clear()
    await router.replace({ name: 'threads', params: { threadId: String(result.value.threadId) } })
  } finally { if (finalizingRunId === thread.runId) finalizingRunId = undefined }
}

async function selectFromRoute(): Promise<void> {
  const raw = route.params['threadId']
  if (typeof raw === 'string') {
    const threadId = Number(raw)
    if (!Number.isSafeInteger(threadId) || threadId < 1) { list.error.value = 'The thread ID is invalid.'; return }
    await inspectThread(threadId)
    return
  }
  const runId = route.query['run']
  if (typeof runId === 'string' && runId !== '') await inspectRun(runId)
}

async function inspectThread(threadId: number, token?: LatestToken): Promise<void> {
  await inspector.inspectThread(threadId, token)
  await transcriptView.value?.scrollToEnd()
}

async function inspectRun(runId: string): Promise<void> {
  const token = inspector.latest.begin()
  if (!inspector.latest.current(token)) return
  const result = await runBackend(resolveThreadRun(runId))
  if (!inspector.latest.current(token)) return
  if (result._tag === 'Failure') { list.error.value = result.error.message; return }
  if (result.value.taskId !== null) {
    await active.refresh()
    if (!inspector.latest.current(token)) return
    const thread = active.activeThreads.value.find(({ taskId }) => taskId === result.value.taskId)
    if (thread === undefined) { list.error.value = `Agent run ${runId} is no longer active.`; return }
    list.error.value = ''
    await monitor(thread, token)
    return
  }
  if (result.value.threadId !== null) { await router.replace({ name: 'threads', params: { threadId: String(result.value.threadId) } }); return }
  list.error.value = `No agent thread is linked to run ${runId}.`
}

function inspect(thread: ThreadSummary): void {
  inspector.latest.invalidate()
  void router.replace({ name: 'threads', params: { threadId: String(thread.threadId) } })
}

function selectTreeNode(node: PrimeTreeNode): void {
  const thread = active.activeThreads.value.find(({ taskId }) => activeTreeKey(taskId) === node.key)
  if (thread !== undefined) { void monitor(thread); return }
  if (active.activeTaskId.value !== undefined) inspector.latest.invalidate()
  active.clear()
  const selected = inspector.nodeLookup.value.get(node.key)
  if (selected !== undefined) { inspector.selectNode(selected); void transcriptView.value?.scrollToEnd() }
}

function closeDrawer(): void {
  inspector.reset()
  active.clear()
  if (route.params['threadId'] !== undefined) void router.replace({ name: 'threads' })
}

function clearActiveSelection(): void { active.clear() }

function requestHalt(thread: ActiveThread): void {
  confirm.require({
    header: `Halt task #${String(thread.taskId)}?`,
    message: 'Cancel this active agent thread and persist the transcript produced so far.',
    rejectLabel: 'Keep running', acceptLabel: 'Halt thread', acceptClass: 'p-button-danger',
    accept: () => { void halt(thread) },
  })
}

async function halt(thread: ActiveThread): Promise<void> {
  haltingTaskId.value = thread.taskId
  const result = await runBackend(haltActiveThread(thread.taskId))
  haltingTaskId.value = undefined
  if (result._tag === 'Failure') { toast.add({ severity: 'error', summary: result.error.message, life: 3500 }); return }
  toast.add({ severity: result.value ? 'success' : 'warn', summary: result.value ? 'Thread halted' : 'Thread was no longer active', life: 2500 })
  await Promise.all([active.refresh(), list.refresh()])
}

onMounted(() => { void list.refresh(); active.startPolling() })
watch([() => connection.state, () => connection.methods], () => { void list.refresh(); void active.poll() })
watch([() => route.params['threadId'], () => route.query['run']], () => { void selectFromRoute() })
watch([list.debouncedQuery, list.platforms], () => { list.first.value = 0; void list.refresh() })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Conversations"
      title="Threads"
      description="Explore persisted conversation branches and the model context behind each reply."
    >
      <Button
        label="Refresh snapshot"
        icon="pi pi-refresh"
        severity="secondary"
        :loading="list.loading.value"
        @click="list.refresh"
      />
    </PageHeading>
    <Message
      v-if="list.error.value"
      severity="error"
      :closable="false"
    >
      {{ list.error.value }}
    </Message>
    <article
      v-if="list.loading.value"
      class="panel manager-loading"
      aria-label="Loading threads"
    >
      <Skeleton
        v-for="index in 6"
        :key="index"
        height="3rem"
      />
    </article>
    <template v-else-if="list.loaded.value">
      <div
        class="manager-summary"
        aria-label="Thread summary"
      >
        <div><span class="summary-mark violet"><i class="pi pi-sitemap" /></span><span><strong>{{ list.summary.value.threads }}</strong><small>Threads</small></span></div>
        <div><span class="summary-mark info"><i class="pi pi-comments" /></span><span><strong>{{ list.summary.value.nodes }}</strong><small>Reply nodes</small></span></div>
        <div><span class="summary-mark success"><i class="pi pi-share-alt" /></span><span><strong>{{ list.summary.value.leaves }}</strong><small>Branch tips</small></span></div>
        <div><span class="summary-mark neutral"><i class="pi pi-globe" /></span><span><strong>{{ list.summary.value.platforms }}</strong><small>Platforms</small></span></div>
      </div>
      <ActiveThreadsPanel
        :threads="active.activeThreads.value"
        :error="active.error.value"
        :halting-task-id="haltingTaskId"
        @open="monitor"
        @halt="requestHalt"
      />
      <ThreadListPanel
        v-model:query="list.query.value"
        v-model:platforms="list.platforms.value"
        :threads="list.threads.value"
        :total="list.total.value"
        :first="list.first.value"
        :rows="list.rows.value"
        :loading="list.tableLoading.value"
        :platform-options="platformOptions"
        :platform-names="platformNames"
        @page="list.changePage"
        @inspect="inspect"
      />
    </template>
    <Drawer
      v-model:visible="inspector.visible.value"
      header="Thread inspector"
      position="right"
      :style="{ width: 'min(1100px, 100vw)' }"
      :close-on-escape="detailIsTop"
      @hide="closeDrawer"
    >
      <div
        v-if="inspector.detailLoading.value"
        class="manager-loading"
      >
        <Skeleton height="5rem" /><Skeleton height="18rem" />
      </div>
      <Message
        v-else-if="inspector.detailError.value"
        severity="error"
        :closable="false"
      >
        {{ inspector.detailError.value }}
      </Message>
      <div
        v-else-if="inspector.detail.value || active.selected.value"
        class="thread-inspector"
      >
        <header
          v-if="inspector.detail.value"
          class="drawer-hero"
        >
          <span class="platform-icon"><PlatformIcon :platform="inspector.detail.value.summary.rootKey.platform" /></span><div>
            <small>{{ platformNames[inspector.detail.value.summary.rootKey.platform] }} thread</small><h2>Thread #{{ inspector.detail.value.summary.threadId }}</h2>
            <div class="tag-list">
              <Tag
                :value="`${inspector.detail.value.summary.nodeCount} nodes`"
                severity="secondary"
              /><Tag
                :value="`${inspector.detail.value.summary.leafCount} branch tips`"
                severity="info"
              /><Tag
                v-if="active.selected.value"
                value="Generating"
                severity="success"
              />
            </div>
          </div><Button
            label="View audit"
            icon="pi pi-wave-pulse"
            severity="secondary"
            @click="router.push({ name: 'audit', query: { thread: String(inspector.detail.value.summary.threadId) } })"
          />
        </header>
        <header
          v-else-if="active.selected.value"
          class="drawer-hero"
        >
          <span class="platform-icon"><i class="pi pi-sparkles" /></span><div>
            <small>Task #{{ active.selected.value.taskId }} · live</small><h2>{{ active.selected.value.prompt || 'Active thread' }}</h2><Tag
              value="Generating"
              severity="success"
            />
          </div>
          <Button
            label="View audit"
            icon="pi pi-wave-pulse"
            severity="secondary"
            @click="router.push({ name: 'audit', query: { run: active.selected.value.runId } })"
          />
        </header>
        <dl
          v-if="inspector.detail.value"
          class="detail-list"
        >
          <div>
            <dt>Chat</dt><dd>
              <DisplayIdentity
                :id="inspector.detail.value.summary.rootKey.chatId"
                :name="inspector.detail.value.summary.chatDisplayName"
                unknown="Direct / unscoped"
              />
            </dd>
          </div>
          <div><dt>Root message</dt><dd><ChatLogMessageLink :message-key="inspector.detail.value.summary.rootKey" /></dd></div>
          <div><dt>Latest message</dt><dd><ChatLogMessageLink :message-key="inspector.detail.value.summary.latestKey" /></dd></div>
        </dl>
        <dl
          v-else-if="active.selected.value"
          class="detail-list"
        >
          <div><dt>Agent run</dt><dd><RunIdLink :run-id="active.selected.value.runId" /></dd></div><div><dt>Linked messages</dt><dd>{{ active.selected.value.messageKeys.length }}</dd></div><div><dt>Pending steers</dt><dd>{{ active.selected.value.pendingSteers }}</dd></div>
        </dl>
        <Message
          v-if="active.selected.value ? active.auditError.value : inspector.statsError.value"
          severity="warn"
          :closable="false"
        >
          {{ active.selected.value ? active.auditError.value : inspector.statsError.value }}
        </Message>
        <ThreadStatsPanel
          v-else
          :stats="inspectorStats"
          :audit-event-count="active.selected.value ? active.auditRecords.value.length : inspector.auditRecords.value.length"
        />
        <div
          class="thread-detail-grid"
          :class="{ 'tree-focused': inspector.treeFocused.value }"
        >
          <ThreadTreePanel
            v-model:selected-keys="inspector.selectedKeys.value"
            v-model:expanded-keys="inspector.expandedKeys.value"
            v-model:focused="inspector.treeFocused.value"
            v-model:zoom="inspector.treeZoom.value"
            :nodes="treeNodes"
            @select="selectTreeNode"
          />
          <ThreadTranscript
            ref="transcriptView"
            :active="active.selected.value"
            :active-messages="active.transcriptMessages.value"
            :active-tools="active.tools.value"
            :transcript="inspector.transcript.value"
            :selected-message-key="inspector.selectedNode.value?.messageKey"
            :image-urls="media.imageUrls"
            :attachments="media.messageAttachments"
            @preview-image="media.previewImage.value = $event"
          />
        </div>
        <Button
          v-if="active.selected.value"
          label="Halt thread"
          icon="pi pi-stop-circle"
          severity="danger"
          :loading="haltingTaskId === active.selected.value.taskId"
          @click="requestHalt(active.selected.value)"
        />
      </div>
    </Drawer>
    <Dialog
      :visible="media.previewImage.value !== undefined"
      modal
      dismissable-mask
      header="Image preview"
      class="image-preview-dialog"
      :close-on-escape="previewIsTop"
      @update:visible="media.previewImage.value = undefined"
    >
      <img
        v-if="media.previewImage.value"
        :src="media.previewImage.value"
        alt="Full-size thread image"
        class="object-preview"
      />
    </Dialog>
  </section>
</template>
