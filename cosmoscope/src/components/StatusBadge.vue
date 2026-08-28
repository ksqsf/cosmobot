<script setup lang="ts">
import Tag from 'primevue/tag'
import type { Status } from '@/types/domain'

const props = defineProps<{ status: Status | 'Loaded' | 'Stopped' | 'Connected' | 'Degraded' }>()
type StatusLabel = typeof props.status
const severity = {
  running: 'info', waiting: 'warn', completed: 'success', failed: 'danger', stopped: 'secondary', degraded: 'warn',
  Loaded: 'success', Stopped: 'danger', Connected: 'success', Degraded: 'warn',
} satisfies Record<StatusLabel, 'info' | 'warn' | 'success' | 'danger' | 'secondary'>
</script>

<template>
  <Tag
    :value="status"
    :severity="severity[props.status]"
  />
</template>
