<script setup lang="ts">
import { computed, ref } from 'vue'
import Button from 'primevue/button'
import Dialog from 'primevue/dialog'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import PageHeading from '@/components/PageHeading.vue'
import ChatLogsPanel from '@/components/ChatLogsPanel.vue'
import ChatComposer from '@/components/chat/ChatComposer.vue'
import ChatSessionList from '@/components/chat/ChatSessionList.vue'
import ChatTranscript from '@/components/chat/ChatTranscript.vue'
import { useChatComposer } from '@/composables/useChatComposer'
import { useChatSession, type ChatSessionState } from '@/composables/useChatSession'
import { chatSessionName } from '@/domain/chat'
import { useLayeredConfirm, useOverlayLayer } from '@/overlay'
import type { ChatMessage } from '@/types/domain'

const error = ref('')
const previewImage = ref<string>()
const confirm = useLayeredConfirm()
const { isTop: previewIsTop } = useOverlayLayer(computed(() => previewImage.value !== undefined))

const sessionHolder: { current?: ChatSessionState } = {}
const composer = useChatComposer({
  error,
  selectedId: computed(() => sessionHolder.current?.selectedId.value),
  captureSelection: () => sessionHolder.current?.captureSelection(),
  isCurrentSelection: (selection) => sessionHolder.current?.isCurrentSelection(selection) ?? false,
  mergeMessage: (message) => { sessionHolder.current?.mergeMessage(message) },
})
const session = useChatSession({
  error,
  beforeSelectionChange: composer.prepareSelectionChange,
  selectionChanged: composer.selectionChanged,
  selectionInvalidated: composer.selectionInvalidated,
  sessionDeleted: composer.forgetSession,
})
sessionHolder.current = session
const {
  sessions, messages, selectedId, selectedSession, sessionsLoading, sessionsLoaded, loading, loadingOlder,
  hasOlder, streamingMessageIds, live, showingPlatformLogs, showChatView, selectSession, loadOlder, createSession,
} = session

async function renameSelected(): Promise<void> {
  const selected = selectedSession.value
  if (selected === undefined) return
  const label = window.prompt('Session name', chatSessionName(selected))?.trim()
  if (!label || label === selected.label) return
  await session.renameSession(selected.sessionId, label)
}

async function forkAt(message: ChatMessage): Promise<void> {
  const parentName = selectedSession.value === undefined ? message.sessionId : chatSessionName(selectedSession.value)
  const label = window.prompt('Fork name', `Fork of ${parentName}`)?.trim()
  if (label === undefined) return
  await session.forkSession(message, label || undefined)
}

function requestDelete(): void {
  const selected = selectedSession.value
  if (selected === undefined) return
  confirm.require({
    header: `Delete “${chatSessionName(selected)}”?`,
    message: 'This permanently deletes the session, its transcript, and stored attachment references.',
    rejectLabel: 'Keep session',
    acceptLabel: 'Delete session',
    acceptClass: 'p-button-danger',
    accept: () => { void session.removeSession(selected.sessionId) },
  })
}
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
    <ChatLogsPanel v-if="showingPlatformLogs" />
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
        <ChatSessionList
          :sessions
          :selected-id="selectedId"
          @select="selectSession"
        />
        <ChatTranscript
          :session="selectedSession"
          :messages
          :streaming-message-ids="streamingMessageIds"
          :loading
          :loading-older="loadingOlder"
          :has-older="hasOlder"
          :load-older="loadOlder"
          @rename="renameSelected"
          @delete="requestDelete"
          @fork="forkAt"
          @preview-image="previewImage = $event"
          @error="error = $event"
        >
          <ChatComposer
            v-model="composer.draft.value"
            :attachments="composer.pendingAttachments.value"
            :disabled="selectedSession === undefined"
            :sending="composer.sending.value"
            :uploading="composer.uploading.value"
            @attach="composer.attachFiles"
            @remove="composer.discardAttachment"
            @send="composer.send"
          />
        </ChatTranscript>
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
          alt="Full-size chat attachment"
          class="object-preview"
        />
      </Dialog>
    </template>
  </section>
</template>
