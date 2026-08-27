<script setup lang="ts">
import { computed, ref } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Checkbox from 'primevue/checkbox'
import InputGroup from 'primevue/inputgroup'
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import ToggleSwitch from 'primevue/toggleswitch'
import PageHeading from '@/components/PageHeading.vue'
import NavigationMenu from '@/components/NavigationMenu.vue'
import type { NavigationItem } from '@/components/NavigationMenu.vue'
const toast = useToast()
const section = ref('Drivers')
const settingsQuery = ref('')
const enabled = ref(true)
const token = ref('configured-secret')
const tokenVisible = ref(false)
const timeout = ref(30)
const polling = ref(25)
const replyTopics = ref(true)
const edited = ref(false)
const dirty = ref(true)
const sectionDefinitions = [
  ['General', 'pi pi-cog'], ['Drivers', 'pi pi-send'], ['LLM', 'pi pi-sparkles'],
  ['Agent tools', 'pi pi-wrench'], ['Handlers', 'pi pi-directions'], ['Plugins', 'pi pi-objects-column'],
  ['Storage & media', 'pi pi-database'], ['RPC & ACP', 'pi pi-code'], ['Logging', 'pi pi-align-left'],
] as const
const sectionItems = computed<NavigationItem[]>(() => sectionDefinitions
  .filter(([label]) => label.toLowerCase().includes(settingsQuery.value.toLowerCase()))
  .map(([label, icon]) => ({ label, icon, command: () => { section.value = label } })))
function discard(): void { polling.value = 20; dirty.value = false; toast.add({ severity: 'success', summary: 'Fixture changes discarded', life: 2000 }) }
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Settings"
      title="Configuration"
      description="Edit typed options and preview their activation impact."
    >
      <span
        v-if="dirty"
        class="unsaved-badge"
      >2 unsaved changes</span><Button
        label="Discard"
        severity="secondary"
        @click="discard"
      /><Button
        label="Review & save"
        @click="toast.add({ severity: 'success', summary: 'Fixture configuration validated', life: 2500 })"
      />
    </PageHeading><div class="config-layout panel">
      <aside class="config-nav">
        <InputText
          v-model="settingsQuery"
          placeholder="Find a setting"
          aria-label="Find a setting"
          size="small"
          fluid
        /><NavigationMenu
          :items="sectionItems"
          :active-label="section"
          navigation-label="Configuration sections"
        />
      </aside><section class="config-form stack">
        <div class="config-section-heading">
          <div><h2>Telegram driver</h2><p>Connection and access policy for the Telegram bot.</p></div><label class="switch-row"><ToggleSwitch v-model="enabled" /> Enabled</label>
        </div><Message
          severity="warn"
          :closable="false"
        >
          Changes in this section require a cosmobot restart.
        </Message><form class="stack">
          <fieldset>
            <legend>Connection</legend><label>Bot token<InputGroup>
              <InputText
                v-model="token"
                :type="tokenVisible ? 'text' : 'password'"
                fluid
                autocomplete="off"
              /><Button
                :icon="tokenVisible ? 'pi pi-eye-slash' : 'pi pi-eye'"
                severity="secondary"
                :aria-label="tokenVisible ? 'Hide bot token' : 'Show bot token'"
                @click="tokenVisible = !tokenVisible"
              />
            </InputGroup><small>Fixture secret only. Real secrets will be write-only.</small></label><div class="field-row">
              <label>API timeout<InputNumber
                v-model="timeout"
                suffix=" seconds"
                fluid
              /></label><label>Polling timeout<InputNumber
                v-model="polling"
                suffix=" seconds"
                fluid
              /><small>Changed from 20</small></label>
            </div>
          </fieldset><fieldset>
            <legend>Access policy</legend><label>Superusers<InputText
              model-value="8872104, 1924401"
              fluid
            /></label><label>Allowed chats<InputText
              model-value="-10018291, -10084720"
              fluid
            /></label>
          </fieldset><fieldset>
            <legend>Message behavior</legend><label class="checkbox-row"><Checkbox
              v-model="replyTopics"
              binary
            /> Reply in topics</label><label class="checkbox-row"><Checkbox
              v-model="edited"
              binary
            /> Process edited messages</label>
          </fieldset>
        </form>
      </section><aside class="config-meta">
        <h2>Option details</h2><dl class="detail-list">
          <div><dt>Owner</dt><dd><code>Bot.Chat.Driver.Telegram.Config</code></dd></div><div><dt>File section</dt><dd><code>[driver.telegram]</code></dd></div><div><dt>Activation</dt><dd>Restart</dd></div><div><dt>Source</dt><dd><code>config.toml</code></dd></div>
        </dl>
      </aside>
    </div>
  </section>
</template>
