<script setup lang="ts">
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Message from 'primevue/message'
import PageHeading from '@/components/PageHeading.vue'
import ConfigProviderEditor from '@/components/configuration/ConfigProviderEditor.vue'
import ConfigSectionEditor from '@/components/configuration/ConfigSectionEditor.vue'
import { restartCosmobot } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { useLatest } from '@/async'
import { useConfigurationDraft, pathKey } from '@/composables/useConfigurationDraft'
import { configSectionTitle } from '@/configuration/navigation'
import { useLayeredConfirm } from '@/overlay'
import { useConnectionStore } from '@/stores/connection'

const connection = useConnectionStore()
const confirm = useLayeredConfirm()
const toast = useToast()
const restartLatest = useLatest()
const draft = useConfigurationDraft()
const {
  snapshot, selectedPath, drafts, validation, providerNames, loading, validating, error,
  navigationGroups, selected, changes, selectedEnabled, busy,
  canManage, controlsDisabled, managementDisabled, canRollback, canRestart, applyReady,
} = draft

function requestRefresh(): void {
  if (changes.value.length === 0) { void draft.refresh(true); return }
  confirm.require({
    header: 'Discard drafts and refresh?',
    message: 'Refreshing loads the current file and discards every unapplied configuration change.',
    rejectLabel: 'Keep drafts', acceptLabel: 'Discard and refresh', acceptClass: 'p-button-danger',
    accept: () => { void draft.refresh(true) },
  })
}

function requestRollback(): void {
  const backup = snapshot.value?.backup
  if (!backup) return
  confirm.require({
    header: 'Roll back configuration?',
    message: 'Swap the current file with the previous valid configuration? A restart is still required.',
    rejectLabel: 'Cancel', acceptLabel: 'Roll back', acceptClass: 'p-button-danger',
    accept: () => { void draft.rollback(backup.revision) },
  })
}

function requestRestart(): void {
  confirm.require({
    header: 'Restart cosmobot?', message: 'Active conversations may be interrupted while the service reconnects.',
    rejectLabel: 'Cancel', acceptLabel: 'Restart', acceptClass: 'p-button-danger',
    accept: () => { void restart() },
  })
}

async function restart(): Promise<void> {
  if (busy.value || connection.state !== 'authenticated') return
  const token = restartLatest.begin()
  loading.value = true
  const result = await runBackend(restartCosmobot)
  if (!restartLatest.current(token)) return
  loading.value = false
  if (result._tag === 'Failure') { error.value = result.error.message; return }
  toast.add({ severity: 'info', summary: 'Restart requested', detail: 'The server acknowledged the request.', life: 3500 })
}

</script>

<template>
  <section class="page">
    <PageHeading
      eyebrow="Settings"
      title="Configuration"
      description="Inspect typed source and effective settings. Every change activates after restart."
    >
      <div class="action-row">
        <Button
          label="Refresh"
          icon="pi pi-refresh"
          severity="secondary"
          :loading="loading"
          :disabled="busy"
          @click="requestRefresh"
        />
        <Button
          v-if="canRollback"
          label="Roll back"
          severity="secondary"
          @click="requestRollback"
        />
        <Button
          v-if="canRestart"
          label="Restart"
          severity="danger"
          outlined
          @click="requestRestart"
        />
      </div>
    </PageHeading>
    <Message
      v-if="error"
      severity="error"
      closable
      @close="error = ''"
    >
      {{ error }}
    </Message>
    <Message
      v-if="snapshot?.sourceState === 'invalid'"
      severity="error"
      :closable="false"
    >
      The current TOML is invalid. Showing the active startup snapshot; editing is disabled.
      <ul>
        <li
          v-for="diagnostic in snapshot.diagnostics"
          :key="`${diagnostic.line}:${diagnostic.column}:${diagnostic.code}`"
        >
          {{ diagnostic.message }}<template v-if="diagnostic.line !== null">
            (line {{ diagnostic.line }}, column {{ diagnostic.column }})
          </template>
        </li>
      </ul>
    </Message>
    <div class="config-layout panel">
      <aside class="config-nav">
        <div
          v-for="group in navigationGroups"
          :key="group.key"
          class="config-nav-group"
        >
          <p class="nav-label">
            {{ group.label }}
          </p>
          <div
            v-for="cluster in group.clusters"
            :key="cluster.key"
            class="config-nav-cluster"
          >
            <small v-if="cluster.label">{{ cluster.label }}</small>
            <button
              v-for="section in cluster.sections"
              :key="pathKey(section.path)"
              class="config-nav-item"
              :class="{ active: pathKey(section.path) === selectedPath }"
              @click="selectedPath = pathKey(section.path)"
            >
              {{ configSectionTitle(section) }}
            </button>
            <ConfigProviderEditor
              v-if="canManage && cluster.repeatable"
              :template="cluster.repeatable"
              :model-value="providerNames[pathKey(cluster.repeatable.path)]"
              :disabled="managementDisabled"
              @update:model-value="providerNames = { ...providerNames, [pathKey(cluster.repeatable!.path)]: $event ?? '' }"
              @add="draft.addProvider(cluster.repeatable)"
            />
          </div>
        </div>
      </aside>
      <ConfigSectionEditor
        v-if="selected"
        :section="selected"
        :enabled="selectedEnabled"
        :can-manage="canManage"
        :busy="busy"
        :controls-disabled="controlsDisabled"
        :changes-count="changes.length"
        :apply-ready="applyReady"
        :loading="loading"
        :validating="validating"
        :validation="validation"
        :drafts="drafts"
        :value="draft.draftValue"
        @add-optional="draft.addOptionalSection(selected)"
        @remove-optional="draft.removeOptionalSection(selected)"
        @remove-provider="draft.removeProvider(selected)"
        @update-option="draft.setOption"
        @replace-secret="draft.replaceSecret"
        @remove-option="draft.removeOption"
        @reset-option="draft.resetOption"
        @validate="draft.validate"
        @apply="draft.apply"
        @clear="draft.clearDrafts"
      />
      <aside
        v-if="selected"
        class="config-meta"
      >
        <h2>Section details</h2>
        <dl class="detail-list">
          <div><dt>Revision</dt><dd><code>{{ snapshot?.revision.slice(0, 12) }}</code></dd></div>
          <div><dt>Active revision</dt><dd><code>{{ snapshot?.activeRevision.slice(0, 12) }}</code></dd></div>
          <div><dt>Activation</dt><dd>Restart</dd></div>
          <div><dt>Draft changes</dt><dd>{{ changes.length }}</dd></div>
        </dl>
      </aside>
    </div>
  </section>
</template>
