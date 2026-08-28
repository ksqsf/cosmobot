<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import type { DataTableDesignTokens } from '@primeuix/themes/types/datatable'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import InputText from 'primevue/inputtext'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import { listLogs } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { LogEntry } from '@/types/domain'
const logs = ref<LogEntry[]>([])
const query = ref('')
const logTableTokens = {
  headerCell: { padding: '0.75rem 0.9rem' },
  bodyCell: { padding: '0.8rem 0.9rem' },
} satisfies DataTableDesignTokens
const filtered = computed(() => logs.value.filter((entry) => Object.values(entry).join(' ').toLowerCase().includes(query.value.toLowerCase())))
onMounted(async () => {
  const result = await runBackend(listLogs)
  if (result._tag === 'Success') logs.value = [...result.value]
})
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Diagnostics"
      title="Logs"
      description="Inspect normalized chat history and structured service output."
    />
    <div class="stack">
      <div class="filter-bar panel">
        <InputText
          v-model="query"
          placeholder="Search visible logs"
          aria-label="Search visible logs"
          size="small"
          fluid
        /><Tag
          value="Service log demo snapshot"
          severity="secondary"
        />
      </div>
      <div class="panel table-wrap log-stream">
        <DataTable
          :value="filtered"
          data-key="id"
          :dt="logTableTokens"
        >
          <Column
            field="time"
            header="Time"
          /><Column
            field="level"
            header="Level"
          >
            <template #body="{ data }">
              <Tag
                :value="data.level"
                :severity="data.level === 'ERROR' ? 'danger' : data.level === 'WARN' ? 'warn' : data.level === 'INFO' ? 'success' : 'secondary'"
              />
            </template>
          </Column><Column
            field="source"
            header="Source"
          >
            <template #body="{ data }">
              <code>{{ data.source }}</code>
            </template>
          </Column><Column
            field="message"
            header="Message"
          >
            <template #body="{ data }">
              {{ data.message }} <small>{{ data.fields }}</small>
            </template>
          </Column>
        </DataTable>
        <footer class="log-footer">
          <span><i class="status-dot online" />{{ filtered.length }} visible entries</span>
          <span>Fixture stream · newest first</span>
        </footer>
      </div>
    </div>
  </section>
</template>
