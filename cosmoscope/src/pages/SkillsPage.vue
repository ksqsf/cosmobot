<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Drawer from 'primevue/drawer'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import PageHeading from '@/components/PageHeading.vue'
import { getSkill, listSkills, removeSkill } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { renderMarkdown } from '@/markdown'
import { useConnectionStore } from '@/stores/connection'
import { useLayeredConfirm, useOverlayLayer } from '@/overlay'
import type { SkillDetail, SkillSummary } from '@/types/domain'

const skills = ref<readonly SkillSummary[]>([])
const selected = ref<SkillDetail | null>(null)
const query = ref('')
const loading = ref(true)
const loaded = ref(false)
const detailLoading = ref(false)
const removing = ref('')
const error = ref('')
const connection = useConnectionStore()
const confirm = useLayeredConfirm()
const toast = useToast()
let detailRequest = 0
const drawerVisible = computed({
  get: () => selected.value !== null,
  set: (visible) => {
    if (!visible) {
      detailRequest += 1
      selected.value = null
    }
  },
})
const { isTop: drawerIsTop } = useOverlayLayer(drawerVisible)
const filtered = computed(() => {
  const needle = query.value.trim().toLocaleLowerCase()
  return needle === '' ? skills.value : skills.value.filter((skill) =>
    `${skill.name} ${skill.description ?? ''}`.toLocaleLowerCase().includes(needle))
})

async function refresh(): Promise<void> {
  if (connection.state !== 'authenticated') {
    loading.value = false
    error.value = connection.error || 'Connect to cosmobot to load skills.'
    return
  }
  loading.value = true
  const result = await runBackend(listSkills)
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  error.value = ''
  skills.value = result.value
  loaded.value = true
}

async function inspect(skill: SkillSummary): Promise<void> {
  const request = ++detailRequest
  selected.value = { name: skill.name, content: '' }
  detailLoading.value = true
  const result = await runBackend(getSkill(skill.name))
  if (request !== detailRequest) return
  detailLoading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; selected.value = null; return }
  selected.value = result.value
}

function requestRemove(skill: SkillSummary): void {
  confirm.require({
    header: `Remove skill “${skill.name}”?`,
    message: 'This permanently deletes the skill directory. The loaded skills snapshot is refreshed automatically.',
    rejectLabel: 'Keep skill',
    acceptLabel: 'Remove skill',
    acceptClass: 'p-button-danger',
    accept: () => { void doRemove(skill) },
  })
}

async function doRemove(skill: SkillSummary): Promise<void> {
  if (removing.value !== '') return
  removing.value = skill.name
  const result = await runBackend(removeSkill(skill.name))
  removing.value = ''
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  if (!result.value) { error.value = `Skill “${skill.name}” is no longer loaded.`; await refresh(); return }
  if (selected.value?.name === skill.name) drawerVisible.value = false
  toast.add({ severity: 'success', summary: `Removed “${skill.name}”`, life: 3000 })
  await refresh()
}

onMounted(refresh)
watch([() => connection.state, () => connection.methods], () => { void refresh() })
</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Agents"
      title="Skills"
      description="Inspect the skills currently available to agents."
    >
      <Button
        label="Refresh"
        icon="pi pi-refresh"
        severity="secondary"
        :loading="loading"
        @click="refresh"
      />
    </PageHeading>
    <Message
      v-if="error"
      severity="error"
      closable
      @close="error = ''"
    >
      {{ error }}
    </Message>
    <article
      v-if="loading && !loaded"
      class="panel manager-loading"
      aria-label="Loading skills"
    >
      <Skeleton
        v-for="index in 6"
        :key="index"
        height="3rem"
      />
    </article>
    <article
      v-else-if="loaded"
      class="panel manager-table"
    >
      <div class="table-toolbar">
        <InputText
          v-model="query"
          placeholder="Filter skills"
          aria-label="Filter skills"
        />
        <span class="muted">{{ filtered.length }} skills</span>
      </div>
      <DataTable
        :value="filtered"
        data-key="name"
        :paginator="filtered.length > 25"
        :rows="25"
        :rows-per-page-options="[25, 50, 100, 200]"
        selection-mode="single"
        @row-click="inspect($event.data)"
      >
        <Column
          field="name"
          header="Skill"
        >
          <template #body="{ data }">
            <strong><code>{{ data.name }}</code></strong>
          </template>
        </Column>
        <Column
          field="description"
          header="Description"
        >
          <template #body="{ data }">
            {{ data.description || 'No description' }}
          </template>
        </Column>
        <Column header="Actions">
          <template #body="{ data }">
            <Button
              label="Remove"
              icon="pi pi-trash"
              severity="danger"
              size="small"
              :loading="removing === data.name"
              :disabled="removing !== ''"
              @click.stop="requestRemove(data)"
            />
          </template>
        </Column>
      </DataTable>
    </article>
    <Drawer
      v-model:visible="drawerVisible"
      position="right"
      class="inspector-drawer"
      :header="selected?.name ?? 'Skill'"
      :close-on-escape="drawerIsTop"
    >
      <Skeleton
        v-if="detailLoading"
        height="12rem"
      />
      <div
        v-else-if="selected"
        class="markdown-body"
        :innerHTML="renderMarkdown(selected.content)"
      />
    </Drawer>
  </section>
</template>
