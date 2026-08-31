<script setup lang="ts">
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Message from 'primevue/message'
import StatusBadge from '@/components/StatusBadge.vue'
import type { Task } from '@/types/domain'

defineProps<{ tasks: readonly Task[]; error: string }>()
const emit = defineEmits<{ inspect: [task: Task]; viewAll: [] }>()
function formatTime(value: string): string { return new Date(value).toLocaleTimeString() }
function elapsed(task: Task): string {
  const end = task.finishedAt === null ? Date.now() : Date.parse(task.finishedAt)
  return `${String(Math.max(0, Math.round((end - Date.parse(task.startedAt)) / 60_000)))}m`
}
</script>

<template>
  <article class="panel">
    <div class="panel-heading">
      <div><h2>Active tasks</h2><p>Work managed by Concurrency</p></div><Button
        label="View all"
        text
        @click="emit('viewAll')"
      />
    </div>
    <Message
      v-if="error"
      severity="error"
      :closable="false"
    >
      {{ error }}
    </Message>
    <DataTable
      v-else
      :value="tasks.filter(({ status }) => status === 'running').slice(0, 8)"
      data-key="id"
      selection-mode="single"
      @row-select="emit('inspect', $event.data)"
    >
      <Column
        field="label"
        header="Task"
      >
        <template #body="{ data }">
          <span class="task-name"><span class="platform-icon"><i class="pi pi-bolt" /></span><span><strong>{{ data.label }}</strong><small>Task #{{ data.id }}</small></span></span>
        </template>
      </Column>
      <Column
        field="status"
        header="Status"
      >
        <template #body="{ data }">
          <StatusBadge :status="data.status" />
        </template>
      </Column>
      <Column header="Started">
        <template #body="{ data }">
          {{ formatTime(data.startedAt) }}
        </template>
      </Column>
      <Column header="Elapsed">
        <template #body="{ data }">
          {{ elapsed(data) }}
        </template>
      </Column>
    </DataTable>
  </article>
</template>
