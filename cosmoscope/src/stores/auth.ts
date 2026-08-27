import { defineStore } from 'pinia'
import { ref } from 'vue'

export const useAuthStore = defineStore('auth', () => {
  const authenticated = ref(sessionStorage.getItem('cosmoscope-demo-auth') === 'yes')
  function login(): void { authenticated.value = true; sessionStorage.setItem('cosmoscope-demo-auth', 'yes') }
  function logout(): void { authenticated.value = false; sessionStorage.removeItem('cosmoscope-demo-auth') }
  return { authenticated, login, logout }
})
