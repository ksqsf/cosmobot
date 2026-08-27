<script setup lang="ts">
defineProps<{ eyebrow: string; title: string; description: string }>()
</script>

<template>
  <header class="page-heading">
    <div>
      <p class="eyebrow">
        {{ eyebrow }}
      </p><h1>{{ title }}</h1><p>{{ description }}</p>
    </div>
    <div class="heading-actions">
      <slot />
    </div>
  </header>
</template>
