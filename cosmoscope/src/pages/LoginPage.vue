<script setup lang="ts">
import { ref } from 'vue'
import { useRouter } from 'vue-router'
import Button from 'primevue/button'
import Card from 'primevue/card'
import InputGroup from 'primevue/inputgroup'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import { useConnectionStore } from '@/stores/connection'

const router = useRouter()
const connection = useConnectionStore()
const endpoint = ref('ws://127.0.0.1:38765/rpc')
const credential = ref('')
const passwordVisible = ref(false)
const failed = ref('')
async function login(): Promise<void> {
  failed.value = ''
  if (!endpoint.value.trim() || !credential.value) { failed.value = 'Enter the endpoint and RPC token.'; return }
  try {
    await connection.connect(endpoint.value.trim(), credential.value)
    credential.value = ''
    await router.push('/overview')
  } catch {
    credential.value = ''
    failed.value = 'Could not authenticate with cosmobot. Check the endpoint, token, and allowed browser origin.'
  }
}
</script>

<template>
  <main class="login-page">
    <Card class="login-card">
      <template #title>
        <h1 class="login-title">
          <span class="brand-mark">C</span> Cosmoscope
        </h1>
      </template>
      <template #subtitle>
        Connect to cosmobot or continue with demo-backed pages
      </template>
      <template #content>
        <form
          class="login-form stack stack-tight"
          @submit.prevent="login"
        >
          <Message
            severity="warn"
            :closable="false"
          >
            The RPC token is kept in memory only and cleared on disconnect.
          </Message>
          <Message
            v-if="failed !== ''"
            severity="error"
            :closable="false"
          >
            {{ failed }}
          </Message>
          <label for="endpoint">RPC endpoint</label><InputText
            id="endpoint"
            v-model="endpoint"
            inputmode="url"
            autocomplete="url"
          />
          <label for="credential">RPC token</label><InputGroup>
            <InputText
              id="credential"
              v-model="credential"
              :type="passwordVisible ? 'text' : 'password'"
              fluid
              autocomplete="off"
            /><Button
              type="button"
              :icon="passwordVisible ? 'pi pi-eye-slash' : 'pi pi-eye'"
              severity="secondary"
              :aria-label="passwordVisible ? 'Hide password' : 'Show password'"
              @click="passwordVisible = !passwordVisible"
            />
          </InputGroup>
          <Button
            label="Connect"
            type="submit"
            fluid
            :loading="connection.state === 'opening'"
          />
          <Button
            v-if="connection.state !== 'offline'"
            label="Disconnect"
            type="button"
            severity="secondary"
            fluid
            @click="connection.disconnect"
          />
        </form>
      </template>
    </Card>
  </main>
</template>
