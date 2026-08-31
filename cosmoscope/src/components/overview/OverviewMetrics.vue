<script setup lang="ts">
import { RouterLink } from 'vue-router'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'

export interface OverviewMetric {
  to: string
  icon: string
  tone: string
  available: boolean
  loading?: boolean
  value: string | number
  label: string
  detail: string
  error: string
}

defineProps<{ metrics: readonly OverviewMetric[] }>()
</script>

<template>
  <div class="metric-grid overview-summary-grid">
    <RouterLink
      v-for="metric in metrics"
      :key="metric.to"
      class="metric"
      :to="metric.to"
    >
      <div class="metric-top">
        <span :class="[metric.icon, 'metric-icon', metric.tone]" />
        <Tag
          :value="metric.available ? 'Live' : 'Unavailable'"
          :severity="metric.available ? 'success' : 'warn'"
        />
      </div>
      <Skeleton
        v-if="metric.loading"
        width="4rem"
        height="2.2rem"
      />
      <strong v-else>{{ metric.value }}</strong>
      <p>{{ metric.label }}</p>
      <small
        v-if="metric.error"
        class="metric-error"
      >{{ metric.error }}</small>
      <small v-else>{{ metric.detail }}</small>
    </RouterLink>
  </div>
</template>
