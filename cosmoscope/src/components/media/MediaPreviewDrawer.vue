<script setup lang="ts">
import { computed, ref } from 'vue'
import Button from 'primevue/button'
import Dialog from 'primevue/dialog'
import Drawer from 'primevue/drawer'
import InputText from 'primevue/inputtext'
import Skeleton from 'primevue/skeleton'
import Tag from 'primevue/tag'
import { safeDownloadUrl } from '@/backend/chat'
import { effectiveSourceKinds, formatMediaTime, mediaIcon, platformIcon, platformLabel, sourceKindIcons, sourceKindLabels, sourcePlatform } from '@/domain/media'
import { formatBytes } from '@/format'
import { useOverlayLayer } from '@/overlay'
import type { MediaDetail } from '@/types/domain'

const props = defineProps<{ detail: MediaDetail | undefined; loading: boolean; deleting: boolean }>()
const emit = defineEmits<{ hide: []; delete: []; copy: [] }>()
const visible = defineModel<boolean>('visible', { required: true })
const zoomOpen = ref(false)
const publicUrl = computed(() => props.detail === undefined ? undefined : safeDownloadUrl(props.detail.publicUrl, window.location.href))
const { isTop: drawerIsTop } = useOverlayLayer(visible)
const { isTop: zoomIsTop } = useOverlayLayer(zoomOpen)

function hide(): void { zoomOpen.value = false; emit('hide') }
</script>

<template>
  <Drawer
    v-model:visible="visible"
    header="Media detail"
    aria-label="Media detail"
    position="right"
    :style="{ width: 'min(520px, 100vw)' }"
    :close-on-escape="drawerIsTop"
    @hide="hide"
  >
    <div
      v-if="loading"
      class="manager-loading"
    >
      <Skeleton
        v-for="index in 5"
        :key="index"
        height="3rem"
      />
    </div>
    <div
      v-else-if="detail"
      class="stack stack-loose"
    >
      <button
        v-if="detail.exists && publicUrl && detail.mimeType.startsWith('image/')"
        class="chat-image-button"
        type="button"
        aria-label="Zoom image"
        @click="zoomOpen = true"
      >
        <img
          :src="publicUrl"
          :alt="detail.sourceName || detail.mediaId"
        />
      </button>
      <video
        v-else-if="detail.exists && publicUrl && detail.mimeType.startsWith('video/')"
        class="object-preview"
        controls
        preload="metadata"
      ><source
        :src="publicUrl"
        :type="detail.mimeType"
      />Your browser cannot play this video.</video>
      <audio
        v-else-if="detail.exists && publicUrl && detail.mimeType.startsWith('audio/')"
        controls
        preload="metadata"
      ><source
        :src="publicUrl"
        :type="detail.mimeType"
      />Your browser cannot play this audio.</audio>
      <header class="drawer-hero">
        <span class="platform-icon"><i :class="mediaIcon(detail.mimeType)" /></span><div>
          <small>{{ detail.mimeType }}</small><h2>{{ detail.sourceName || detail.mediaId }}</h2><Tag
            :value="detail.exists ? 'Available' : 'Missing'"
            :severity="detail.exists ? 'success' : 'danger'"
          />
        </div>
      </header>
      <dl class="detail-list">
        <div><dt>Media ID</dt><dd>{{ detail.mediaId }}</dd></div><div><dt>File ID</dt><dd>{{ detail.fileId }}</dd></div><div><dt>Size</dt><dd>{{ formatBytes(detail.size) }}</dd></div><div><dt>Created</dt><dd>{{ formatMediaTime(detail.createdAtUnix) }}</dd></div><div><dt>Last used</dt><dd>{{ formatMediaTime(detail.lastUsedAtUnix) }}</dd></div><div><dt>Digest</dt><dd>{{ detail.digest }}</dd></div>
      </dl>
      <div class="stack">
        <strong>Public URL</strong><div class="resource-rename">
          <InputText
            :model-value="detail.publicUrl"
            readonly
            aria-label="Public URL"
          /><Button
            icon="pi pi-copy"
            aria-label="Copy public URL"
            severity="secondary"
            @click="emit('copy')"
          /><Button
            icon="pi pi-external-link"
            aria-label="Open public URL"
            as="a"
            :href="publicUrl"
            :disabled="publicUrl === undefined"
            target="_blank"
            rel="noopener"
          />
        </div>
      </div>
      <div class="stack">
        <strong>Source tags</strong><span class="tag-list"><Tag
          v-for="kind in effectiveSourceKinds(detail)"
          :key="kind"
          :value="sourceKindLabels[kind]"
          :icon="sourceKindIcons[kind]"
          severity="secondary"
        /></span>
      </div>
      <div
        v-if="detail.platforms.length"
        class="stack"
      >
        <strong>Platforms</strong><span class="tag-list"><Tag
          v-for="name in detail.platforms"
          :key="name"
          :value="platformLabel(name)"
          :icon="platformIcon(name)"
          severity="secondary"
        /></span>
      </div>
      <div
        v-if="detail.platformRefs.length"
        class="stack"
      >
        <strong>Platform upload cache</strong><ul class="associated-resource-list">
          <li
            v-for="platformRef in detail.platformRefs"
            :key="`${platformRef.platform}:${platformRef.scope}:${platformRef.platformRef}`"
          >
            <span><strong>{{ platformLabel(platformRef.platform) }}</strong><small class="block">{{ platformRef.scope }}</small></span><code>{{ platformRef.platformRef }}</code>
          </li>
        </ul>
      </div>
      <div
        v-if="detail.sourceRefs.length"
        class="stack"
      >
        <strong>Source references</strong><span class="tag-list"><Tag
          v-for="source in detail.sourceRefs"
          :key="source"
          :value="source"
          :icon="platformIcon(sourcePlatform(source))"
          severity="secondary"
        /></span>
      </div>
      <Button
        label="Delete media"
        icon="pi pi-trash"
        severity="danger"
        :loading="deleting"
        @click="emit('delete')"
      />
    </div>
  </Drawer>
  <Dialog
    v-model:visible="zoomOpen"
    modal
    dismissable-mask
    header="Image preview"
    :close-on-escape="zoomIsTop"
  >
    <img
      v-if="detail && publicUrl"
      class="object-preview"
      :src="publicUrl"
      :alt="detail.sourceName || detail.mediaId"
    />
  </Dialog>
</template>
