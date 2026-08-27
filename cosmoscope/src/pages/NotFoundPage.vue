<script setup lang="ts">
import { useRouter } from 'vue-router'
import Button from 'primevue/button'

const router = useRouter()
</script>

<template>
  <main class="not-found">
    <h1>Page not found</h1><p>The requested Cosmoscope route does not exist.</p><Button
      label="Return to overview"
      @click="router.push('/overview')"
    />
  </main>
</template>
