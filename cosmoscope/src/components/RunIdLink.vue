<script setup lang="ts">
import { RouterLink } from 'vue-router'

defineProps<{ readonly runId: string }>()
</script>

<template>
  <RouterLink
    class="run-id-link"
    :to="{ name: 'threads', query: { run: runId } }"
    :title="`Open agent thread ${runId}`"
    @click.stop
  >
    {{ runId }}
  </RouterLink>
</template>

<style scoped>
.run-id-link {
  color: var(--p-primary-color);
  font-family: monospace;
  text-decoration: none;
}

.run-id-link:hover {
  text-decoration: underline;
}
</style>
