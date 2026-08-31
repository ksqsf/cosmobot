<script setup lang="ts">
import Column from 'primevue/column'
import DataTable from 'primevue/datatable'
import Tag from 'primevue/tag'
import { formatBytes } from '@/format'
import { effectiveSourceKinds, formatMediaTime, mediaIcon, platformIcon, platformLabel, sourceKindIcons, sourceKindLabels } from '@/domain/media'
import type { MediaItem, MediaSourceKind } from '@/types/domain'

defineProps<{ items: readonly MediaItem[]; loading: boolean }>()
const emit = defineEmits<{ inspect: [item: MediaItem] }>()
const selection = defineModel<MediaItem[]>('selection', { required: true })
</script>

<template>
  <DataTable
    v-model:selection="selection"
    :value="items"
    :loading="loading"
    data-key="mediaId"
    :paginator="items.length > 25"
    :rows="25"
    :rows-per-page-options="[25, 50, 100, 200]"
    @row-click="emit('inspect', $event.data)"
  >
    <Column
      selection-mode="multiple"
      header-style="width: 3rem"
    />
    <Column
      field="sourceName"
      header="Media"
    >
      <template #body="{ data }">
        <span class="manager-identity"><span class="manager-type-icon"><i :class="mediaIcon(data.mimeType)" /></span><span><strong>{{ data.sourceName || data.mediaId }}</strong><small>{{ data.mediaId }}</small></span></span>
      </template>
    </Column>
    <Column header="Platform">
      <template #body="{ data }">
        <span class="tag-list"><Tag
          v-for="name in data.platforms"
          :key="name"
          :value="platformLabel(name)"
          :icon="platformIcon(name)"
          severity="secondary"
        /><small v-if="data.platforms.length === 0">No platform</small></span>
      </template>
    </Column>
    <Column header="Source">
      <template #body="{ data }">
        <span class="tag-list"><Tag
          v-for="kind in effectiveSourceKinds(data)"
          :key="kind"
          :value="sourceKindLabels[kind as MediaSourceKind]"
          :icon="sourceKindIcons[kind as MediaSourceKind]"
          severity="secondary"
        /></span>
      </template>
    </Column>
    <Column
      field="mimeType"
      header="Type"
    />
    <Column header="Size">
      <template #body="{ data }">
        {{ formatBytes(data.size) }}
      </template>
    </Column>
    <Column header="Added">
      <template #body="{ data }">
        {{ formatMediaTime(data.createdAtUnix) }}
      </template>
    </Column>
    <Column header="State">
      <template #body="{ data }">
        <Tag
          :value="data.exists ? 'Available' : 'Missing'"
          :severity="data.exists ? 'success' : 'danger'"
        />
      </template>
    </Column>
  </DataTable>
</template>
