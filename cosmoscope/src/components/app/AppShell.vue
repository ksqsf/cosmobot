<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, ref, useTemplateRef } from 'vue'
import { RouterLink, RouterView, useRoute, useRouter } from 'vue-router'
import { useDark, useToggle } from '@vueuse/core'
import Button from 'primevue/button'
import Dialog from 'primevue/dialog'
import Drawer from 'primevue/drawer'
import InputText from 'primevue/inputtext'
import { pages } from '@/app/pages'
import NavigationMenu from '@/components/NavigationMenu.vue'
import type { NavigationItem } from '@/components/NavigationMenu.vue'
import type { NavigationGroup } from '@/app/pages'
import { useConnectionStore } from '@/stores/connection'

const route = useRoute()
const router = useRouter()
const connectionStore = useConnectionStore()
const mobileNavigation = ref(false)
const paletteOpen = ref(false)
const query = ref('')
interface ButtonRef { readonly $el: HTMLButtonElement }
interface InputTextRef { readonly $el: HTMLInputElement }
const commandButton = useTemplateRef<ButtonRef>('commandButton')
const commandInput = useTemplateRef<InputTextRef>('commandInput')
const isDark = useDark({ selector: 'html', attribute: 'data-theme', valueDark: 'dark', valueLight: 'light', initialValue: 'dark', storageKey: 'cosmoscope-theme' })
const toggleDark = useToggle(isDark)
const currentTitle = computed(() => {
  const title = route.meta['title']
  return typeof title === 'string' ? title : 'Cosmoscope'
})
const currentPage = computed(() => pages.find((page) => page.name === route.name))
const pageIsReal = computed(() => connectionStore.state === 'authenticated' && currentPage.value?.requiredCapabilities.every((method) => connectionStore.methods.has(method)) === true)
const connectionLabel = computed(() => ({
  offline: 'Offline', opening: 'Connecting', authenticated: 'Connected', reconnecting: 'Reconnecting', failed: 'Failed',
})[connectionStore.state])
const filteredPages = computed(() => pages.filter((page) => page.title.toLowerCase().includes(query.value.toLowerCase())))
function navigationItems(group: NavigationGroup): NavigationItem[] {
  return pages
    .filter((page) => page.navigationGroup === group)
    .map((page) => {
      const item = { label: page.title, icon: page.icon, route: page.path.replace(/:\w+\??/, '') }
      if (page.name === 'audit') return { ...item, badge: '12' }
      if (page.name === 'tasks') return { ...item, indicator: 'danger' as const }
      if (page.name === 'configuration') return { ...item, indicator: 'warning' as const }
      return item
    })
}

function openPalette(): void { query.value = ''; paletteOpen.value = true; void nextTick(() => commandInput.value?.$el.focus()) }
function choose(path: string): void { paletteOpen.value = false; void router.push(path) }
function onKeydown(event: KeyboardEvent): void {
  if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === 'k') { event.preventDefault(); openPalette() }
}
onMounted(() => window.addEventListener('keydown', onKeydown))
onUnmounted(() => window.removeEventListener('keydown', onKeydown))
</script>

<template>
  <a
    class="skip-link"
    href="#main"
  >Skip to content</a>
  <div class="shell">
    <aside
      class="sidebar desktop-nav"
      aria-label="Primary navigation"
    >
      <div class="brand">
        <span class="brand-mark">C</span><span><strong>Cosmobot</strong><small>Control room</small></span>
      </div>
      <nav>
        <template
          v-for="group in ['workspace', 'operations'] as const"
          :key="group"
        >
          <p class="nav-label">
            {{ group }}
          </p>
          <NavigationMenu
            :items="navigationItems(group)"
            :active-label="currentTitle"
            :navigation-label="`${group} navigation`"
          />
        </template>
      </nav>
      <div class="sidebar-footer">
        <div class="environment">
          <span
            class="status-dot"
            :class="pageIsReal ? 'online' : 'warning'"
          /><span><strong>{{ pageIsReal ? 'Live data' : 'Demo data' }}</strong><small>{{ pageIsReal ? `cosmobot ${connectionStore.serverVersion}` : 'Deterministic fixtures' }}</small></span>
        </div>
        <div class="user-button">
          <span class="avatar">KA</span><span><strong>kosmos</strong><small>Administrator</small></span>
        </div>
      </div>
    </aside>

    <section class="workspace">
      <header class="topbar">
        <Button
          class="mobile-only"
          icon="pi pi-bars"
          severity="secondary"
          text
          rounded
          aria-label="Open navigation"
          @click="mobileNavigation = true"
        />
        <div class="breadcrumbs">
          <span>cosmobot</span><b>/</b><strong>{{ currentTitle }}</strong>
        </div>
        <div class="topbar-actions">
          <RouterLink
            class="connection"
            to="/login"
            :aria-label="`${connectionLabel}. Open connection setup`"
          >
            <i
              class="status-dot"
              :class="connectionStore.state === 'authenticated' ? 'online' : connectionStore.state === 'failed' ? 'danger' : 'warning'"
            />{{ connectionLabel }}<span v-if="connectionStore.stale"> · stale</span>
          </RouterLink>
          <Button
            v-if="connectionStore.stale"
            icon="pi pi-refresh"
            text
            rounded
            aria-label="Retry RPC connection"
            @click="connectionStore.retry"
          />
          <Button
            ref="commandButton"
            class="command-button"
            severity="secondary"
            outlined
            aria-haspopup="dialog"
            @click="openPalette"
          >
            <span>Search or jump to…</span><kbd>⌘ K</kbd>
          </Button>
          <Button
            :icon="isDark ? 'pi pi-sun' : 'pi pi-moon'"
            text
            rounded
            aria-label="Toggle color theme"
            @click="toggleDark()"
          />
        </div>
      </header>
      <main
        id="main"
        tabindex="-1"
      >
        <RouterView />
      </main>
    </section>
  </div>

  <Drawer
    v-model:visible="mobileNavigation"
    header="Navigate"
    aria-label="Navigate"
    position="left"
    class="mobile-drawer"
  >
    <nav class="mobile-links">
      <RouterLink
        v-for="page in pages"
        :key="page.name"
        :to="page.path.replace(/:\w+\??/, '')"
        @click="mobileNavigation = false"
      >
        <i :class="page.icon" />{{ page.title }}
      </RouterLink>
    </nav>
  </Drawer>

  <Dialog
    v-model:visible="paletteOpen"
    modal
    header="Command palette"
    class="command-dialog"
    @after-hide="commandButton?.$el.focus()"
    @show="commandInput?.$el.focus()"
  >
    <InputText
      ref="commandInput"
      v-model="query"
      fluid
      placeholder="Search pages and actions…"
      aria-label="Search commands"
    />
    <div class="command-results">
      <Button
        v-for="page in filteredPages"
        :key="page.name"
        class="command-result"
        :label="page.title"
        :icon="page.icon"
        severity="secondary"
        text
        @click="choose(page.path.replace(/:\w+\??/, ''))"
      />
    </div>
  </Dialog>
  <div
    class="sr-live"
    aria-live="polite"
  >
    {{ connectionLabel }}. {{ pageIsReal ? 'Live data' : 'Demo data' }} environment.
  </div>
</template>
