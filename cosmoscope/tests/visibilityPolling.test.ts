import { defineComponent, h, onMounted } from 'vue'
import { flushPromises, mount } from '@vue/test-utils'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { useVisibilityPolling } from '@/composables/useVisibilityPolling'

describe('visibility polling', () => {
  afterEach(() => { vi.useRealTimers() })

  it('runs on its interval and stops with the component scope', async () => {
    vi.useFakeTimers()
    const task = vi.fn(async () => Promise.resolve())
    const component = defineComponent({
      setup() {
        const polling = useVisibilityPolling(task, { interval: 1_000 })
        onMounted(polling.start)
        return () => h('div')
      },
    })
    const wrapper = mount(component)
    await vi.advanceTimersByTimeAsync(1_000)
    expect(task).toHaveBeenCalledOnce()
    wrapper.unmount()
    await vi.advanceTimersByTimeAsync(2_000)
    await flushPromises()
    expect(task).toHaveBeenCalledOnce()
  })
})
