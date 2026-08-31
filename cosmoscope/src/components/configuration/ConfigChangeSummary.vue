<script setup lang="ts">
import Message from 'primevue/message'
import { displayConfigValue } from '@/configuration/values'
import { pathKey } from '@/composables/useConfigurationDraft'
import type { ConfigurationValidation } from '@/rpc/schemas'

defineProps<{ validation: ConfigurationValidation | undefined }>()
const displayPath = (path: readonly string[]): string => path.join('.')
</script>

<template>
  <Message
    v-if="validation"
    :severity="validation.valid ? 'success' : 'error'"
    :closable="false"
  >
    {{ validation.valid ? `${validation.diff.length} semantic change(s) validated.` : 'Validation failed.' }}
    <ul v-if="validation.diagnostics.length">
      <li
        v-for="diagnostic in validation.diagnostics"
        :key="`${pathKey(diagnostic.path)}:${diagnostic.code}`"
      >
        {{ displayPath(diagnostic.path) }}: {{ diagnostic.message }}
      </li>
    </ul>
    <ul v-else-if="validation.diff.length">
      <li
        v-for="change in validation.diff"
        :key="pathKey(change.path)"
      >
        <code>{{ displayPath(change.path) }}</code>:
        {{ displayConfigValue(change.before) }} → {{ displayConfigValue(change.after) }}
      </li>
    </ul>
  </Message>
</template>
