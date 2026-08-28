import { createApp } from 'vue'
import { createPinia } from 'pinia'
import PrimeVue from 'primevue/config'
import ToastService from 'primevue/toastservice'
import ConfirmationService from 'primevue/confirmationservice'
import 'primeicons/primeicons.css'
import App from './App.vue'
import router from './app/router'
import { CosmoscopePreset } from './app/primevue'
import { disposeBackendRuntime } from './backend/runBackend'
import { useConnectionStore } from './stores/connection'
import './styles/tokens.css'
import './styles/base.css'
import './styles/shell.css'
import './styles/pages.css'

const pinia = createPinia()
createApp(App)
  .use(pinia)
  .use(router)
  .use(PrimeVue, {
    theme: { preset: CosmoscopePreset, options: { darkModeSelector: '[data-theme="dark"]' } },
  })
  .use(ToastService)
  .use(ConfirmationService)
  .mount('#app')

window.addEventListener('pagehide', () => {
  useConnectionStore(pinia).disconnect()
  void disposeBackendRuntime()
}, { once: true })
