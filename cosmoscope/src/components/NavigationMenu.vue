<script setup lang="ts">
import { computed } from 'vue'
import { RouterLink } from 'vue-router'
import type { MenuDesignTokens } from '@primeuix/themes/types/menu'
import type { MenuItem } from 'primevue/menuitem'
import Menu from 'primevue/menu'

export interface NavigationItem {
  readonly label: string
  readonly icon: string
  readonly route?: string
  readonly badge?: string
  readonly indicator?: 'danger' | 'warning'
  readonly command?: () => void
}

const props = defineProps<{
  readonly items: readonly NavigationItem[]
  readonly activeLabel: string
  readonly navigationLabel: string
}>()
const menuTokens = {
  root: { background: 'transparent', borderColor: 'transparent', borderRadius: '0' },
  list: { padding: '0', gap: '0.15rem' },
  item: { padding: '0.7rem 0.8rem', borderRadius: '8px', gap: '0.7rem', label: { fontSize: '0.86rem' } },
} satisfies MenuDesignTokens
const model = computed<MenuItem[]>(() => props.items.map((item) => item.command === undefined
  ? { label: item.label, icon: item.icon }
  : { label: item.label, icon: item.icon, command: item.command }))

function source(label: string | undefined): NavigationItem | undefined {
  return props.items.find((item) => item.label === label)
}
function labelOf(label: MenuItem['label']): string | undefined {
  return typeof label === 'string' ? label : undefined
}
function iconOf(icon: MenuItem['icon']): string | undefined {
  return typeof icon === 'string' ? icon : undefined
}
</script>

<template>
  <Menu
    :model="model"
    :dt="menuTokens"
    :aria-label="navigationLabel"
    class="navigation-menu"
  >
    <template #item="{ item, props: itemProps }">
      <RouterLink
        v-if="source(labelOf(item.label))?.route"
        v-slot="{ href, navigate }"
        :to="source(labelOf(item.label))?.route ?? '/'"
        custom
      >
        <a
          :href="href"
          v-bind="itemProps.action"
          class="navigation-menu-item"
          :class="{ active: activeLabel === labelOf(item.label) }"
          :aria-current="activeLabel === labelOf(item.label) ? 'page' : undefined"
          @click="navigate"
        >
          <span :class="iconOf(item.icon)" /><span>{{ labelOf(item.label) }}</span>
          <span
            v-if="source(labelOf(item.label))?.badge"
            class="nav-count"
          >{{ source(labelOf(item.label))?.badge }}</span>
          <span
            v-if="source(labelOf(item.label))?.indicator"
            class="nav-dot"
            :class="source(labelOf(item.label))?.indicator"
          />
        </a>
      </RouterLink>
      <a
        v-else
        v-bind="itemProps.action"
        class="navigation-menu-item"
        :class="{ active: activeLabel === labelOf(item.label) }"
        :aria-current="activeLabel === labelOf(item.label) ? 'page' : undefined"
      >
        <span :class="iconOf(item.icon)" /><span>{{ labelOf(item.label) }}</span>
      </a>
    </template>
  </Menu>
</template>

<style scoped>
.navigation-menu { width: 100%; }
.navigation-menu-item.active { color: var(--text); background: var(--accent-soft); }
.navigation-menu-item > span:nth-child(2) { min-width: 0; }
</style>
