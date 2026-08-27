<script setup lang="ts">
import { ref } from 'vue'
import { useRouter } from 'vue-router'
import Button from 'primevue/button'
import Card from 'primevue/card'
import InputGroup from 'primevue/inputgroup'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import { useAuthStore } from '@/stores/auth'

const router = useRouter()
const auth = useAuthStore()
const user = ref('kosmos')
const password = ref('demo')
const passwordVisible = ref(false)
const failed = ref(false)
function login(): void {
  failed.value = !user.value.trim() || !password.value
  if (!failed.value) { auth.login(); void router.push('/overview') }
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
        Fixture-backed administration preview
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
            Demo authentication only. No credential leaves this browser.
          </Message>
          <Message
            v-if="failed"
            severity="error"
            :closable="false"
          >
            Enter both fields.
          </Message>
          <label for="user">User</label><InputText
            id="user"
            v-model="user"
            autocomplete="username"
          />
          <label for="password">Password</label><InputGroup>
            <InputText
              id="password"
              v-model="password"
              :type="passwordVisible ? 'text' : 'password'"
              fluid
              autocomplete="current-password"
            /><Button
              :icon="passwordVisible ? 'pi pi-eye-slash' : 'pi pi-eye'"
              severity="secondary"
              :aria-label="passwordVisible ? 'Hide password' : 'Show password'"
              @click="passwordVisible = !passwordVisible"
            />
          </InputGroup>
          <Button
            label="Sign in to demo"
            type="submit"
            fluid
          />
        </form>
      </template>
    </Card>
  </main>
</template>
