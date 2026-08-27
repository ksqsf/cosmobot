<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import type { DataTableDesignTokens } from '@primeuix/themes/types/datatable'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import InputText from 'primevue/inputtext'
import SelectButton from 'primevue/selectbutton'
import Tag from 'primevue/tag'
import PageHeading from '@/components/PageHeading.vue'
import { listLogs } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { LogEntry } from '@/types/domain'
const logs = ref<LogEntry[]>([])
const query = ref('')
const paused = ref(false)
const kind = ref('Service logs')
const logTableTokens = {
  headerCell: { padding: '0.75rem 0.9rem' },
  bodyCell: { padding: '0.8rem 0.9rem' },
} satisfies DataTableDesignTokens
const filtered = computed(() => logs.value.filter((entry) => Object.values(entry).join(' ').toLowerCase().includes(query.value.toLowerCase())))
onMounted(async (): Promise<void> => {
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
    >
      <Button
        :label="paused ? 'Resume tail' : 'Pause tail'"
        :icon="paused ? 'pi pi-play' : 'pi pi-pause'"
        severity="secondary"
        @click="paused = !paused"
      /><Button label="Download view" />
    </PageHeading>
    <div class="stack">
      <SelectButton
        v-model="kind"
        :options="['Service logs', 'Chat logs']"
        class="log-tabs"
        size="small"
        aria-label="Log type"
      />
      <div class="filter-bar panel">
        <InputText
          v-model="query"
          placeholder="Search visible logs"
          aria-label="Search visible logs"
          size="small"
          fluid
        /><Tag
          :value="paused ? 'Paused' : 'Following fixture output'"
          :severity="paused ? 'warn' : 'success'"
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
