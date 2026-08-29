<script setup lang="ts">
import type { AuditPlatform } from '@/types/domain'

const props = defineProps<{ platform: AuditPlatform }>()

const icons = {
  PlatformTelegram: 'pi pi-send',
  PlatformMatrix: 'pi pi-th-large',
  PlatformDiscord: 'pi pi-discord',
  PlatformRPC: 'pi pi-desktop',
  PlatformACP: 'pi pi-code',
} satisfies Record<Exclude<AuditPlatform, 'PlatformQQ'>, string>
</script>

<template>
  <svg
    v-if="props.platform === 'PlatformQQ'"
    class="platform-brand-icon"
    viewBox="0 0 24 24"
    role="img"
    aria-label="QQ"
    fill="none"
    stroke="currentColor"
    stroke-width="1.7"
    stroke-linecap="round"
    stroke-linejoin="round"
  >
    <path d="M12 3C8.4 3 6.7 5.7 6.7 9.5 5.8 11.7 3.6 15.8 4 18c.1.7 1.5-.4 2.4-1.8.2 2 1.2 3.4 2.6 4.1-1.9.4-3 .8-2.9 1.3.2.5 3.4.3 5.2.1l.7-.4.7.4c1.8.2 5 .4 5.2-.1.1-.5-1-1-2.9-1.3 1.4-.7 2.4-2.1 2.6-4.1.9 1.4 2.3 2.5 2.4 1.8.4-2.2-1.8-6.3-2.7-8.5C17.3 5.7 15.6 3 12 3Z" />
  </svg>
  <i
    v-else
    :class="icons[props.platform]"
    aria-hidden="true"
  />
</template>

<style scoped>
.platform-brand-icon {
  display: block;
  width: 1em;
  height: 1em;
  overflow: visible;
}
</style>
