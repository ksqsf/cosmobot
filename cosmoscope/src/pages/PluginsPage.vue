<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import Card from 'primevue/card'
import Message from 'primevue/message'
import PageHeading from '@/components/PageHeading.vue'
import StatusBadge from '@/components/StatusBadge.vue'
import { listPlugins } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { Plugin } from '@/types/domain'
const plugins = ref<Plugin[]>([])
const totals = computed(() => plugins.value.reduce((summary, plugin) => ({
  installed: summary.installed + 1,
  loaded: summary.loaded + Number(plugin.status === 'Loaded'),
  tools: summary.tools + plugin.tools,
  routes: summary.routes + plugin.routes,
}), { installed: 0, loaded: 0, tools: 0, routes: 0 }))
onMounted(async () => {
  const result = await runBackend(listPlugins)
  if (result._tag === 'Success') plugins.value = [...result.value]
})
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Extensions"
      title="Plugins"
      description="Inspect installed capabilities and their current lifecycle."
    /><div class="summary-strip panel">
      <div><strong>{{ totals.installed }}</strong><span>Installed</span></div><div><strong>{{ totals.loaded }}</strong><span>Loaded</span></div><div><strong>{{ totals.tools }}</strong><span>Tools</span></div><div><strong>{{ totals.routes }}</strong><span>Routes</span></div>
    </div><div class="plugin-grid">
      <Card
        v-for="plugin in plugins"
        :key="plugin.id"
      >
        <template #title>
          {{ plugin.name }}
        </template><template #subtitle>
          <code>{{ plugin.id }}</code>
        </template><template #content>
          <div class="stack stack-tight">
            <p>{{ plugin.description }}</p><Message
              v-if="plugin.error"
              severity="error"
              :closable="false"
            >
              {{ plugin.error }}
            </Message><div class="plugin-stats">
              <span><b>{{ plugin.tools }}</b> tools</span><span><b>{{ plugin.routes }}</b> routes</span>
            </div>
          </div>
        </template><template #footer>
          <div class="plugin-footer">
            <StatusBadge :status="plugin.status" /><small>v{{ plugin.version }} · demo</small>
          </div>
        </template>
      </Card>
    </div>
  </section>
</template>
