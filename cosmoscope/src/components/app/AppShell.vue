<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, ref, useTemplateRef, watch } from 'vue'
import { RouterLink, RouterView, useRoute, useRouter } from 'vue-router'
import { useDark, useToggle } from '@vueuse/core'
import Button from 'primevue/button'
import Dialog from 'primevue/dialog'
import Drawer from 'primevue/drawer'
import InputText from 'primevue/inputtext'
import { pages } from '@/app/pages'
import NavigationMenu from '@/components/NavigationMenu.vue'
import type { NavigationItem } from '@/components/NavigationMenu.vue'
import { useConnectionStore } from '@/stores/connection'
import type { ConnectionState } from '@/rpc/client'

const route = useRoute()
const router = useRouter()
const connectionStore = useConnectionStore()
const mobileNavigation = ref(false)
const sidebarCollapsed = ref(false)
const paletteOpen = ref(false)
const query = ref('')
const selectedCommand = ref(0)
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
const pageIsDemo = computed(() => currentPage.value?.requiredCapabilities.some((method) => method.endsWith('.demo')) === true)
const pageIsReal = computed(() => connectionStore.state === 'authenticated' && currentPage.value?.requiredCapabilities.every((method) => connectionStore.methods.has(method)) === true)
const pageIsPartial = computed(() => !pageIsDemo.value && connectionStore.state === 'authenticated' && !pageIsReal.value)
const dataLabel = computed(() => {
  if (pageIsDemo.value) return 'Demo data'
  if (connectionStore.state === 'opening' || connectionStore.state === 'reconnecting') return 'Loading data'
  if (pageIsReal.value) return 'Live data'
  return pageIsPartial.value ? 'Partial data' : 'Unavailable'
})
const dataDetail = computed(() => pageIsDemo.value ? 'Deterministic fixtures' : pageIsReal.value ? `cosmobot ${connectionStore.serverVersion}` : 'Waiting for RPC data')
const connectionLabel = computed(() => ({
  offline: 'Disconnected', opening: 'Connecting', authenticated: 'Connected', reconnecting: 'Reconnecting', failed: 'Connection failed',
})[connectionStore.state])
const connectionPresentation: Record<ConnectionState, { readonly icon: string, readonly tone: string }> = {
  offline: { icon: 'pi pi-circle', tone: 'connection-muted' },
  opening: { icon: 'pi pi-spinner pi-spin', tone: 'connection-warning' },
  authenticated: { icon: 'pi pi-check-circle', tone: 'connection-success' },
  reconnecting: { icon: 'pi pi-spinner pi-spin', tone: 'connection-warning' },
  failed: { icon: 'pi pi-times-circle', tone: 'connection-danger' },
}
const connectionStatus = computed(() => connectionPresentation[connectionStore.state])
const filteredPages = computed(() => {
  const needle = query.value.trim().toLowerCase()
  return needle === '' ? pages : pages.filter((page) => `${page.title} ${page.name} ${page.path}`.toLowerCase().includes(needle))
})
const selectedPage = computed(() => filteredPages.value[selectedCommand.value])
const navigationItems: NavigationItem[] = pages.map((page) => ({ label: page.title, icon: page.icon, route: router.resolve({ name: page.name }).path }))

function openPalette(): void {
  query.value = ''
  selectedCommand.value = Math.max(0, pages.findIndex((page) => page.name === route.name))
  paletteOpen.value = true
}
function focusCommandInput(): void { void nextTick(() => commandInput.value?.$el.focus()) }
function choose(name: string): void { paletteOpen.value = false; void router.push({ name }) }
function moveCommand(offset: number): void {
  const count = filteredPages.value.length
  if (count > 0) selectedCommand.value = (selectedCommand.value + offset + count) % count
}
function onCommandKeydown(event: KeyboardEvent): void {
  if (event.key === 'ArrowDown') { event.preventDefault(); moveCommand(1) }
  if (event.key === 'ArrowUp') { event.preventDefault(); moveCommand(-1) }
  if (event.key === 'Enter' && selectedPage.value) { event.preventDefault(); choose(selectedPage.value.name) }
}
function onKeydown(event: KeyboardEvent): void {
  if ((event.metaKey || event.ctrlKey) && event.code === 'KeyK') { event.preventDefault(); openPalette() }
}
watch(query, () => { selectedCommand.value = 0 })
onMounted(() => window.addEventListener('keydown', onKeydown))
onUnmounted(() => window.removeEventListener('keydown', onKeydown))
</script>

<template>
  <a
    class="skip-link"
    href="#main"
  >Skip to content</a>
  <div
    class="shell"
    :class="{ 'sidebar-collapsed': sidebarCollapsed }"
  >
    <aside
      class="sidebar desktop-nav"
      :class="{ collapsed: sidebarCollapsed }"
      aria-label="Primary navigation"
    >
      <Button
        class="sidebar-toggle"
        :icon="sidebarCollapsed ? 'pi pi-angle-right' : 'pi pi-angle-left'"
        severity="secondary"
        rounded
        :aria-label="sidebarCollapsed ? 'Expand sidebar' : 'Collapse sidebar'"
        :aria-expanded="!sidebarCollapsed"
        @click="sidebarCollapsed = !sidebarCollapsed"
      />
      <div class="brand">
        <span class="brand-mark">C</span><span v-if="!sidebarCollapsed"><strong>Cosmobot</strong><small>Control room</small></span>
      </div>
      <Button
        ref="commandButton"
        class="command-button sidebar-command"
        severity="secondary"
        outlined
        aria-label="Search or jump to"
        aria-haspopup="dialog"
        @click="openPalette"
      >
        <i class="pi pi-search" />
        <span v-if="!sidebarCollapsed">Search</span>
        <kbd v-if="!sidebarCollapsed">⌘ K</kbd>
      </Button>
      <nav>
        <NavigationMenu
          :items="navigationItems"
          :active-label="currentTitle"
          navigation-label="Primary navigation"
        />
      </nav>
      <div class="sidebar-footer">
        <div class="environment">
          <span
            class="status-dot"
            :class="pageIsReal ? 'online' : 'warning'"
          /><span><strong>{{ dataLabel }}</strong><small>{{ dataDetail }}</small></span>
        </div>
        <div class="user-button">
          <span class="avatar">A</span><span><strong>RPC access</strong><small>Administrator token</small></span>
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
            class="connection-status"
            :class="connectionStatus.tone"
            to="/login"
            :aria-label="`${connectionLabel}. Open connection setup`"
          >
            <i
              class="connection-status-icon"
              :class="connectionStatus.icon"
            /><span>{{ connectionLabel }}<small v-if="connectionStore.stale">Stale</small></span>
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
        :to="{ name: page.name }"
        @click="mobileNavigation = false"
      >
        <i :class="page.icon" />{{ page.title }}
      </RouterLink>
    </nav>
  </Drawer>

  <Dialog
    v-model:visible="paletteOpen"
    modal
    dismissable-mask
    header="Go to"
    class="command-dialog"
    @after-hide="commandButton?.$el.focus()"
    @after-show="focusCommandInput"
  >
    <InputText
      ref="commandInput"
      v-model="query"
      autofocus
      fluid
      placeholder="Search pages…"
      aria-label="Search pages"
      role="combobox"
      aria-controls="command-results"
      :aria-activedescendant="selectedPage ? `command-${selectedPage.name}` : undefined"
      :aria-expanded="paletteOpen"
      @keydown="onCommandKeydown"
    />
    <div
      id="command-results"
      class="command-results"
      role="listbox"
      aria-label="Pages"
    >
      <Button
        v-for="(page, index) in filteredPages"
        :id="`command-${page.name}`"
        :key="page.name"
        class="command-result"
        :class="{ selected: selectedCommand === index }"
        severity="secondary"
        text
        role="option"
        :aria-selected="selectedCommand === index"
        @mouseenter="selectedCommand = index"
        @click="choose(page.name)"
      >
        <span class="command-result-icon"><i :class="page.icon" /></span>
        <span class="command-result-copy"><strong>{{ page.title }}</strong><small>{{ router.resolve({ name: page.name }).path }}</small></span>
        <kbd v-if="selectedCommand === index">↵</kbd>
      </Button>
      <div
        v-if="filteredPages.length === 0"
        class="command-empty"
      >
        <i class="pi pi-search" /><strong>No matching pages</strong><small>Try a page name such as Tasks or Resources.</small>
      </div>
    </div>
    <div class="command-help">
      <span><kbd>↑</kbd><kbd>↓</kbd> Navigate</span><span><kbd>↵</kbd> Open</span><span><kbd>Esc</kbd> Close</span>
    </div>
  </Dialog>
  <div
    class="sr-live"
    aria-live="polite"
  >
    {{ connectionLabel }}. {{ dataLabel }} environment.
  </div>
</template>
