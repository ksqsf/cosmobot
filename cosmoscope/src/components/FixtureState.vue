<script setup lang="ts">
import Button from 'primevue/button'
import Message from 'primevue/message'
import Skeleton from 'primevue/skeleton'
import type { FixtureScenario } from '@/types/domain'

defineProps<{ state: FixtureScenario; message?: string }>()
defineEmits<{ retry: [] }>()
</script>

<template>
  <div
    v-if="state === 'loading'"
    class="state-card"
    aria-label="Loading fixture data"
  >
    <Skeleton
      width="35%"
      height="1.4rem"
    /><Skeleton /><Skeleton width="75%" />
  </div>
  <Message
    v-else-if="state === 'empty'"
    severity="secondary"
  >
    No fixture records match this view.
  </Message>
  <Message
    v-else-if="state === 'error'"
    severity="error"
    :closable="false"
  >
    {{ message ?? 'The fixture request failed.' }} <Button
      label="Retry"
      size="small"
      text
      @click="$emit('retry')"
    />
  </Message>
  <Message
    v-else-if="state === 'offline'"
    severity="warn"
    :closable="false"
  >
    Offline — the last known fixture snapshot is stale. <Button
      label="Retry"
      size="small"
      text
      @click="$emit('retry')"
    />
  </Message>
  <Message
    v-else-if="state === 'forbidden'"
    severity="error"
    :closable="false"
  >
    Administrator permission is required for this fixture.
  </Message>
  <slot v-else />
</template>
