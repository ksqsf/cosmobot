// @vitest-environment jsdom

import { mount } from '@vue/test-utils'
import PrimeVue from 'primevue/config'
import { describe, expect, it } from 'vitest'
import ConfigListInput from '@/components/configuration/ConfigListInput.vue'

describe('configuration list editor', () => {
  it('edits mixed identities without comma-separated string syntax', async () => {
    const wrapper = mount(ConfigListInput, {
      props: { modelValue: [123, '@room'], itemKind: 'identity', label: 'Allowed chats', disabled: false },
      global: { plugins: [PrimeVue] },
    })
    const inputs = wrapper.findAll('input')
    expect(inputs.map((input) => input.element.value)).toEqual(['123', '@room'])
    await wrapper.findAll('select')[0]?.setValue('text')
    expect(wrapper.emitted('update:modelValue')?.at(-1)?.[0]).toEqual(['123', '@room'])
    const textWrapper = mount(ConfigListInput, {
      props: { modelValue: ['123', '@room'], itemKind: 'identity', label: 'Allowed chats', disabled: false },
      global: { plugins: [PrimeVue] },
    })
    await textWrapper.findAll('input')[0]?.setValue('00456')
    expect(textWrapper.emitted('update:modelValue')?.at(-1)?.[0]).toEqual(['00456', '@room'])
  })

  it('adds and removes integer list rows', async () => {
    const wrapper = mount(ConfigListInput, {
      props: { modelValue: [10], itemKind: 'integer', label: 'Allowed groups', disabled: false },
      global: { plugins: [PrimeVue] },
    })
    await wrapper.get('button[aria-label="Remove Allowed groups item 1"]').trigger('click')
    expect(wrapper.emitted('update:modelValue')?.at(-1)?.[0]).toEqual([])
    await wrapper.get('button[aria-label="Add entry"]').trigger('click')
    expect(wrapper.emitted('update:modelValue')?.at(-1)?.[0]).toEqual([10, null])
  })
})
