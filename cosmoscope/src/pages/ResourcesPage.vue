<script setup lang="ts">
import { ref } from 'vue'
import Button from 'primevue/button'
import DataTable from 'primevue/datatable'
import Column from 'primevue/column'
import Drawer from 'primevue/drawer'
import Message from 'primevue/message'
import PageHeading from '@/components/PageHeading.vue'
import StatusBadge from '@/components/StatusBadge.vue'

const resources = [
  { id: 'workspace-8f2c', kind: 'Workspace', name: 'cosmobot review', owner: 'run_8f2c', status: 'running' as const, users: 1 },
  { id: 'browser-b41d', kind: 'Browser', name: 'Research session', owner: 'run_b41d', status: 'waiting' as const, users: 0 },
  { id: 'process-2e09', kind: 'Process', name: 'test runner', owner: 'run_2e09', status: 'failed' as const, users: 0 },
]
const selected = ref<(typeof resources)[number]>()
const visible = ref(false)
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Runtime"
      title="Resources"
      description="Inspect long-running objects and their active owners."
    >
      <Button
        label="Refresh snapshot"
        severity="secondary"
      />
    </PageHeading>
    <article class="panel">
      <DataTable
        :value="resources"
        selection-mode="single"
        @row-select="selected = $event.data; visible = true"
      >
        <Column
          field="id"
          header="ID"
        /><Column
          field="kind"
          header="Kind"
        /><Column
          field="name"
          header="Name"
        /><Column
          field="owner"
          header="Owner"
        /><Column
          field="users"
          header="Active users"
        /><Column
          field="status"
          header="Status"
        >
          <template #body="{ data }">
            <StatusBadge :status="data.status" />
          </template>
        </Column>
      </DataTable>
    </article><Drawer
      v-model:visible="visible"
      header="Resource detail"
      aria-label="Resource detail"
      position="right"
      :style="{ width: 'min(420px, 100vw)' }"
    >
      <template v-if="selected">
        <div class="stack stack-tight">
          <h2>{{ selected.name }}</h2><p><code>{{ selected.id }}</code></p><StatusBadge :status="selected.status" /><Message
            severity="secondary"
            :closable="false"
          >
            Demo resource actions never affect a real object.
          </Message>
        </div>
      </template>
    </Drawer>
  </section>
</template>
