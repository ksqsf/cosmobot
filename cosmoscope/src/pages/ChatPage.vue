<script setup lang="ts">
import { computed, ref, useTemplateRef } from 'vue'
import type { ButtonDesignTokens } from '@primeuix/themes/types/button'
import type { SelectDesignTokens } from '@primeuix/themes/types/select'
import type { TextareaDesignTokens } from '@primeuix/themes/types/textarea'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import IconField from 'primevue/iconfield'
import InputText from 'primevue/inputtext'
import InputIcon from 'primevue/inputicon'
import Select from 'primevue/select'
import Tag from 'primevue/tag'
import Textarea from 'primevue/textarea'
import PageHeading from '@/components/PageHeading.vue'

interface Conversation {
  readonly id: string
  readonly title: string
  readonly preview: string
  readonly platform: string
  readonly time: string
  readonly group: 'Today' | 'Yesterday'
}

const conversations: readonly Conversation[] = [
  { id: 'storage', title: 'Review storage refactor', preview: 'Can you inspect the latest diff…', platform: 'R', time: '14:28', group: 'Today' },
  { id: 'release', title: 'Release announcement', preview: 'I drafted a short message for…', platform: 'T', time: '13:05', group: 'Today' },
  { id: 'timeout', title: 'Timeout investigation', preview: 'The HTTP call failed after…', platform: 'D', time: '11:42', group: 'Today' },
  { id: 'haskell', title: 'Haskell API lookup', preview: 'Here are the relevant types…', platform: 'M', time: 'Thu', group: 'Yesterday' },
  { id: 'image', title: 'Image identification', preview: 'This appears to be from…', platform: 'Q', time: 'Thu', group: 'Yesterday' },
]
const toast = useToast()
const draft = ref('')
const search = ref('')
const selectedId = ref('storage')
const selectedModel = ref('Auto')
const attachmentInput = useTemplateRef<HTMLInputElement>('attachmentInput')
const modelOptions: string[] = ['Auto', 'GPT-5.6', 'Claude Sonnet']
const groups = ['Today', 'Yesterday'] as const
const composerTextareaTokens = {
  root: {
    background: 'transparent',
    borderColor: 'transparent',
    hoverBorderColor: 'transparent',
    focusBorderColor: 'transparent',
    shadow: 'none',
    paddingX: '0.85rem',
    paddingY: '0.75rem',
    borderRadius: '10px',
    focusRing: { width: '0' },
  },
} satisfies TextareaDesignTokens
const composerSelectTokens = {
  root: {
    background: 'transparent',
    borderColor: 'transparent',
    hoverBorderColor: 'transparent',
    focusBorderColor: 'transparent',
    shadow: 'none',
    sm: { fontSize: '0.72rem', paddingX: '0.35rem', paddingY: '0.35rem' },
    focusRing: { width: '0' },
  },
  dropdown: { width: '1.6rem' },
} satisfies SelectDesignTokens
const composerButtonTokens = {
  root: { sm: { iconOnlyWidth: '2.15rem', paddingX: '0', paddingY: '0.5rem' } },
} satisfies ButtonDesignTokens
const filteredConversations = computed(() => conversations.filter((item) =>
  `${item.title} ${item.preview}`.toLowerCase().includes(search.value.toLowerCase()),
))

function send(): void {
  if (!draft.value.trim()) return
  draft.value = ''
  toast.add({ severity: 'success', summary: 'Message queued in demo', detail: 'The fixture transcript was not persisted.', life: 2500 })
}
function attached(event: Event): void {
  const input = event.currentTarget
  if (!(input instanceof HTMLInputElement) || !input.files?.[0]) return
  const summary = input.files.length === 1 ? input.files[0].name : `${String(input.files.length)} files selected`
  toast.add({ severity: 'info', summary, detail: 'Fixture attachments selected; no upload was performed.', life: 2200 })
  input.value = ''
}
</script>

<template>
  <section class="page chat-page">
    <PageHeading
      eyebrow="RPC workspace"
      title="Chat"
      description="Talk to cosmobot and inspect the resulting run."
    >
      <Button
        label="New session"
        icon="pi pi-plus"
        @click="toast.add({ severity: 'info', summary: 'New fixture session', life: 2000 })"
      />
    </PageHeading>
    <div class="chat-layout panel">
      <aside
        class="conversation-list"
        aria-label="Conversations"
      >
        <IconField class="conversation-search">
          <InputIcon class="pi pi-search" />
          <InputText
            v-model="search"
            placeholder="Find a conversation"
            size="small"
            variant="filled"
            fluid
          />
        </IconField>
        <section
          v-for="group in groups"
          :key="group"
          class="conversation-group"
        >
          <h2>{{ group }}</h2>
          <Button
            v-for="conversation in filteredConversations.filter((item) => item.group === group)"
            :key="conversation.id"
            class="conversation"
            :class="{ active: selectedId === conversation.id }"
            unstyled
            @click="selectedId = conversation.id"
          >
            <span class="platform-icon">{{ conversation.platform }}</span>
            <span><strong>{{ conversation.title }}</strong><small>{{ conversation.preview }}</small></span>
            <time>{{ conversation.time }}</time>
          </Button>
        </section>
      </aside>

      <section
        class="transcript"
        aria-label="Chat transcript"
      >
        <header class="transcript-header">
          <div><strong>Review storage refactor</strong><small><i class="status-dot online" />RPC session · 8 messages</small></div>
          <div>
            <Button
              icon="pi pi-info-circle"
              text
              rounded
              aria-label="Session information"
            /><Button
              icon="pi pi-ellipsis-h"
              text
              rounded
              aria-label="Session actions"
            />
          </div>
        </header>
        <div class="messages">
          <article class="message user">
            <header class="message-meta">
              <span class="avatar">KA</span><strong>You</strong><time datetime="2026-08-28T14:28:00">14:28</time>
            </header>
            <div class="message-body">
              Can you inspect the latest storage refactor and focus on resource cleanup?
            </div>
          </article>
          <div class="run-divider">
            <span>Agent run <code>run_8f2c91</code> started</span>
          </div>
          <article class="message bot">
            <header class="message-meta">
              <span class="brand-mark small">C</span><strong>Cosmobot</strong><time datetime="2026-08-28T14:28:00">14:28</time>
            </header>
            <div class="message-body">
              <p>I’ll trace the lifecycle from acquisition through cleanup, then check the focused tests.</p>
              <details
                open
                class="tool-notification"
              >
                <summary><i class="pi pi-bolt tool-indicator" /><strong>Read 4 files</strong><span>1.2s</span></summary>
                <div class="tool-detail">
                  <code>Bot.Resource</code><code>Bot.Storage.Resource</code><code>ResourceSpec.hs</code><code>Bot.Main</code>
                </div>
              </details>
              <details class="tool-notification">
                <summary><i class="pi pi-check tool-indicator success" /><strong>Tests passed</strong><span>6.4s</span></summary>
                <p>Focused resource lifecycle checks passed.</p>
              </details>
              <p>The main lifecycle is structured correctly. One medium-risk path remains: explicit removal is restored after cleanup failure, but the error loses the resource identity before reaching the audit observer.</p>
            </div>
          </article>
        </div>
        <form
          class="composer"
          @submit.prevent="send"
        >
          <Textarea
            v-model="draft"
            rows="3"
            placeholder="Message cosmobot…"
            aria-label="Message cosmobot"
            class="composer-input"
            :dt="composerTextareaTokens"
            fluid
          />
          <div class="composer-actions">
            <div>
              <Button
                label="Attach"
                icon="pi pi-plus"
                size="small"
                severity="secondary"
                text
                @click="attachmentInput?.click()"
              /><input
                ref="attachmentInput"
                type="file"
                multiple
                hidden
                @change="attached"
              />
              <Select
                v-model="selectedModel"
                :options="modelOptions"
                aria-label="Model selection"
                class="model-select"
                size="small"
                :dt="composerSelectTokens"
              >
                <template #value="slotProps">
                  Model · {{ slotProps.value.toLowerCase() }}
                </template>
              </Select>
            </div>
            <Button
              type="submit"
              icon="pi pi-arrow-up"
              aria-label="Send message"
              size="small"
              :dt="composerButtonTokens"
            />
          </div>
        </form>
      </section>

      <aside
        class="context-panel"
        aria-label="Run context"
      >
        <div class="context-heading">
          <h2>Run context</h2><Button
            icon="pi pi-times"
            text
            rounded
            aria-label="Close run context"
          />
        </div>
        <dl class="detail-list">
          <div>
            <dt>Status</dt><dd>
              <Tag
                value="Completed"
                severity="success"
              />
            </dd>
          </div>
          <div><dt>Run ID</dt><dd><code>run_8f2c91</code></dd></div>
          <div><dt>Started</dt><dd>14:28:16</dd></div>
          <div><dt>Duration</dt><dd>8.1 seconds</dd></div>
          <div><dt>Model turns</dt><dd>2</dd></div>
          <div><dt>Tool calls</dt><dd>5</dd></div>
        </dl>
        <h3>Tools used</h3><div class="tag-list">
          <Tag
            value="read_file × 4"
            severity="secondary"
          /><Tag
            value="run_test × 1"
            severity="secondary"
          />
        </div>
        <h3>Related audit</h3>
        <ol class="mini-timeline">
          <li><strong>Run started</strong><small>14:28:16.108</small></li><li><strong>Model response</strong><small>14:28:17.442</small></li><li><strong>Run completed</strong><small>14:28:24.201</small></li>
        </ol>
      </aside>
    </div>
  </section>
</template>
