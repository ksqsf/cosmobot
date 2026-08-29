<script setup lang="ts">
defineProps<{ name?: string | null; id?: string | null; unknown?: string }>()
</script>

<template>
  <span :title="name && id ? id : undefined">{{ name || id || unknown || 'Unknown' }}</span>
</template>
