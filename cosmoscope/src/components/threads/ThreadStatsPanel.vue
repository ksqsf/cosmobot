<script setup lang="ts">
import Tag from 'primevue/tag'
import { formatDuration } from '@/domain/threadTranscript'
import type { ThreadStats } from '@/backend/threadStats'

defineProps<{ stats: ThreadStats, auditEventCount: number }>()
</script>

<template>
  <section
    class="thread-stats"
    aria-label="Thread execution statistics"
  >
    <div><span class="summary-mark violet"><i class="pi pi-chart-bar" /></span><span><strong>{{ stats.tokens?.total_tokens.toLocaleString() ?? 'Unreported' }}</strong><small v-if="stats.tokens">Tokens · {{ stats.tokens.prompt_tokens.toLocaleString() }} prompt / {{ stats.tokens.completion_tokens.toLocaleString() }} completion</small><small v-else>Token usage was not returned by the model provider</small><small v-if="stats.promptCacheHitRate !== null">Prompt cache · {{ (stats.promptCacheHitRate * 100).toFixed(1) }}% hit · {{ stats.cachedPromptTokens?.toLocaleString() }} cached</small><small v-else>Prompt cache · Unreported</small></span></div>
    <div><span class="summary-mark success"><i class="pi pi-arrow-right" /></span><span><strong v-if="stats.latestTokens">{{ stats.latestTokens.prompt_tokens.toLocaleString() }} input / {{ stats.latestTokens.completion_tokens.toLocaleString() }} output</strong><strong v-else>Unreported</strong><small>Latest model request</small></span></div>
    <div><span class="summary-mark warning"><i class="pi pi-wrench" /></span><span><strong>{{ formatDuration(stats.toolMilliseconds, stats.unreportedToolCalls) }}</strong><small>Tool time · {{ stats.toolCalls }} calls / {{ stats.failedTools }} failed</small></span></div>
    <div><span class="summary-mark info"><i class="pi pi-sparkles" /></span><span><strong>{{ formatDuration(stats.modelMilliseconds, stats.unreportedModelTurns) }}</strong><small>Model time · {{ stats.modelTurns }} turns</small></span></div>
    <footer>
      <Tag
        :value="`${stats.contextMessages} peak context messages`"
        severity="secondary"
      />
      <Tag
        :value="`${stats.compactions} compactions`"
        severity="secondary"
      />
      <Tag
        :value="`${stats.subagents} subagents`"
        severity="secondary"
      />
      <Tag
        :value="`${auditEventCount} audit events`"
        severity="secondary"
      />
    </footer>
  </section>
</template>
