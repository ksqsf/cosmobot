<script setup lang="ts">
import { useRouter } from 'vue-router'
import Button from 'primevue/button'
import Drawer from 'primevue/drawer'
import StatusBadge from '@/components/StatusBadge.vue'
import { useOverlayLayer } from '@/overlay'
import type { Task } from '@/types/domain'

defineProps<{ task: Task | undefined }>()
const emit = defineEmits<{ hide: [] }>()
const visible = defineModel<boolean>('visible', { required: true })
const router = useRouter()
const { isTop } = useOverlayLayer(visible)
function formatTime(value: string): string { return new Date(value).toLocaleTimeString() }
function elapsed(task: Task): string {
  const end = task.finishedAt === null ? Date.now() : Date.parse(task.finishedAt)
  return `${String(Math.max(0, Math.round((end - Date.parse(task.startedAt)) / 60_000)))}m`
}
</script>

<template>
  <Drawer
    v-model:visible="visible"
    position="right"
    header="Task detail"
    :close-on-escape="isTop"
    aria-label="Task detail"
    :style="{ width: 'min(420px, 100vw)' }"
    @hide="emit('hide')"
  >
    <div
      v-if="task"
      class="stack stack-loose"
    >
      <div class="drawer-hero">
        <span class="platform-icon"><i class="pi pi-bolt" /></span><div><h2>{{ task.label }}</h2><StatusBadge :status="task.status" /></div>
      </div>
      <dl class="detail-list">
        <div><dt>ID</dt><dd>#{{ task.id }}</dd></div><div><dt>Started</dt><dd>{{ formatTime(task.startedAt) }}</dd></div><div><dt>Elapsed</dt><dd>{{ elapsed(task) }}</dd></div>
      </dl>
      <Button
        label="Open task page"
        @click="router.push(`/tasks/${task.id}`)"
      />
    </div>
  </Drawer>
</template>
