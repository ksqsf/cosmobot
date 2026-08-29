<script setup lang="ts">
import { RouterLink } from 'vue-router'

export interface NavigationItem {
  readonly label: string
  readonly icon: string
  readonly route: string
}

defineProps<{
  readonly items: readonly NavigationItem[]
  readonly activeLabel: string
  readonly navigationLabel: string
}>()
</script>

<template>
  <ul
    :aria-label="navigationLabel"
    class="navigation-menu"
  >
    <li
      v-for="item in items"
      :key="item.route"
    >
      <RouterLink
        :to="item.route"
        class="navigation-menu-item"
        :class="{ active: activeLabel === item.label }"
        :aria-label="item.label"
        :aria-current="activeLabel === item.label ? 'page' : undefined"
        :title="item.label"
      >
        <span :class="item.icon" /><span>{{ item.label }}</span>
      </RouterLink>
    </li>
  </ul>
</template>

<style scoped>
.navigation-menu { display: grid; gap: 0.15rem; width: 100%; margin: 0; padding: 0; list-style: none; }
.navigation-menu-item { display: flex; gap: 0.7rem; align-items: center; padding: 0.7rem 0.8rem; border-radius: 8px; color: var(--muted); font-size: 0.86rem; text-decoration: none; }
.navigation-menu-item:hover { color: var(--text); background: var(--surface-2); }
.navigation-menu-item.active { color: var(--text); background: var(--accent-soft); }
.navigation-menu-item > span:nth-child(2) { min-width: 0; }
</style>
