<script setup lang="ts">
import Column from 'primevue/column'
import DataTable, { type DataTablePageEvent } from 'primevue/datatable'
import FloatLabel from 'primevue/floatlabel'
import InputText from 'primevue/inputtext'
import MultiSelect from 'primevue/multiselect'
import Tag from 'primevue/tag'
import DisplayIdentity from '@/components/DisplayIdentity.vue'
import PlatformIcon from '@/components/PlatformIcon.vue'
import type { AuditPlatform, ThreadSummary } from '@/types/domain'

defineProps<{
  threads: readonly ThreadSummary[]
  total: number
  first: number
  rows: number
  loading: boolean
  platformOptions: { label: string, value: AuditPlatform }[]
  platformNames: Readonly<Record<AuditPlatform, string>>
}>()
const emit = defineEmits<{
  page: [event: DataTablePageEvent]
  inspect: [thread: ThreadSummary]
}>()
const query = defineModel<string>('query', { required: true })
const platforms = defineModel<AuditPlatform[]>('platforms', { required: true })
</script>

<template>
  <article class="panel manager-table">
    <div class="table-toolbar thread-toolbar">
      <InputText
        v-model="query"
        placeholder="Filter by thread, chat, or message"
        aria-label="Filter threads"
      />
      <FloatLabel variant="on">
        <MultiSelect
          v-model="platforms"
          :options="platformOptions"
          input-id="thread-platform-filter"
          option-label="label"
          option-value="value"
          display="chip"
          placeholder="All"
        />
        <label for="thread-platform-filter">Platform</label>
      </FloatLabel>
    </div>
    <DataTable
      :value="threads"
      data-key="threadId"
      selection-mode="single"
      lazy
      paginator
      :first="first"
      :rows="rows"
      :total-records="total"
      :loading="loading"
      :rows-per-page-options="[25, 50, 100, 200]"
      @page="emit('page', $event)"
      @row-select="emit('inspect', $event.data)"
    >
      <Column
        field="threadId"
        header="Thread"
      >
        <template #body="{ data }">
          <span class="manager-identity"><span class="manager-type-icon"><PlatformIcon :platform="data.rootKey.platform" /></span><span><strong>#{{ data.threadId }}</strong><small>{{ platformNames[data.rootKey.platform as AuditPlatform] }}</small></span></span>
        </template>
      </Column>
      <Column header="Chat">
        <template #body="{ data }">
          <DisplayIdentity
            :id="data.rootKey.chatId"
            :name="data.chatDisplayName"
            unknown="Direct / unscoped"
          />
        </template>
      </Column>
      <Column header="Latest message">
        <template #body="{ data }">
          <span class="thread-message-id">{{ data.latestPreview || 'No text content' }}</span>
        </template>
      </Column>
      <Column
        field="nodeCount"
        header="Nodes"
      />
      <Column header="Shape">
        <template #body="{ data }">
          <Tag
            :value="data.leafCount === 1 ? 'Linear' : `${data.leafCount} branches`"
            :severity="data.leafCount === 1 ? 'secondary' : 'info'"
          />
        </template>
      </Column>
    </DataTable>
  </article>
</template>
