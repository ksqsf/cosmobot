<script setup lang="ts">
import { RouterLink } from 'vue-router'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import RunIdLink from '@/components/RunIdLink.vue'
import type { Activity } from '@/types/domain'

defineProps<{ activities: readonly Activity[]; error: string; loading: boolean; live: boolean }>()
</script>

<template>
  <article class="panel activity-panel">
    <div class="panel-heading">
      <div><h2>Recent activity</h2><p>Agent audit events</p></div><Tag
        :value="live ? 'Live' : 'Snapshot'"
        :severity="live ? 'success' : 'secondary'"
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
      v-else-if="loading"
      class="manager-loading"
    >
      <Skeleton
        v-for="index in 4"
        :key="index"
        height="2.5rem"
      />
    </div>
    <ol
      v-else
      class="activity-list"
    >
      <li
        v-for="item in activities.slice(0, 8)"
        :key="item.id"
      >
        <i
          class="pi pi-circle-fill"
          :class="item.tone"
        /><div>
          <p>
            <RouterLink :to="`/audit/${item.id}`">
              <strong>{{ item.kind }}</strong> {{ item.summary }}
            </RouterLink>
          </p><small><RunIdLink :run-id="item.source" /> · {{ item.time }}</small>
        </div>
      </li>
    </ol>
  </article>
</template>
