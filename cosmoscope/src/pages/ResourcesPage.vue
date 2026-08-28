<script setup lang="ts">
import { onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import DataTable from 'primevue/datatable'
import Column from 'primevue/column'
import Drawer from 'primevue/drawer'
import Message from 'primevue/message'
import PageHeading from '@/components/PageHeading.vue'
import StatusBadge from '@/components/StatusBadge.vue'
import type { Status } from '@/types/domain'

interface ResourceFixture { id: string; kind: string; name: string; owner: string; status: Status; users: number }
const resources = [
  { id: 'workspace-8f2c', kind: 'Workspace', name: 'cosmobot review', owner: 'run_8f2c', status: 'running', users: 1 },
  { id: 'browser-b41d', kind: 'Browser', name: 'Research session', owner: 'run_b41d', status: 'waiting', users: 0 },
  { id: 'process-2e09', kind: 'Process', name: 'test runner', owner: 'run_2e09', status: 'failed', users: 0 },
] satisfies readonly ResourceFixture[]
const selected = ref<(typeof resources)[number]>()
const visible = ref(false)
const route = useRoute()
const router = useRouter()

function selectFromRoute(): void {
  const resourceId = route.params['resourceId']
  if (typeof resourceId !== 'string') return
  const resource = resources.find(({ id }) => id === resourceId)
  if (resource !== undefined) { selected.value = resource; visible.value = true }
}
function inspect(resource: (typeof resources)[number]): void {
  selected.value = resource
  visible.value = true
  void router.replace(`/resources/${resource.id}`)
}
function closeDrawer(): void {
  selected.value = undefined
  if (route.params['resourceId'] !== undefined) void router.replace('/resources')
}
watch(() => route.params['resourceId'], selectFromRoute)
onMounted(selectFromRoute)
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Runtime"
      title="Resources"
      description="Inspect long-running objects and their active owners."
    />
    <article class="panel">
      <DataTable
        :value="resources"
        selection-mode="single"
        @row-select="inspect($event.data)"
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
      @hide="closeDrawer"
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
