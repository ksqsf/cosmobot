<script setup lang="ts">
import { ref } from 'vue'
import { useToast } from 'primevue/usetoast'
import Button from 'primevue/button'
import Checkbox from 'primevue/checkbox'
import InputGroup from 'primevue/inputgroup'
import InputNumber from 'primevue/inputnumber'
import InputText from 'primevue/inputtext'
import Message from 'primevue/message'
import ToggleSwitch from 'primevue/toggleswitch'
import PageHeading from '@/components/PageHeading.vue'
const toast = useToast()
const enabled = ref(true)
const token = ref('configured-secret')
const tokenVisible = ref(false)
const timeout = ref(30)
const polling = ref(25)
const replyTopics = ref(true)
const edited = ref(false)
</script>
<template>
  <section class="page">
    <PageHeading
      eyebrow="Settings"
      title="Configuration"
      description="Edit typed options and preview their activation impact."
    >
      <Button
        label="Validate fixture"
        @click="toast.add({ severity: 'success', summary: 'Fixture configuration validated', life: 2500 })"
      />
    </PageHeading><div class="config-layout panel">
      <aside class="config-nav">
        <p class="nav-label">
          Available fixture
        </p><strong>Drivers</strong><small class="block">Telegram</small>
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
                type="button"
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
