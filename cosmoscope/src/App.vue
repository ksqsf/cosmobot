<script setup lang="ts">
import Toast from 'primevue/toast'
import ConfirmDialog from 'primevue/confirmdialog'
import { RouterView } from 'vue-router'
</script>

<template>
  <RouterView />
  <Toast />
  <ConfirmDialog />
</template>
