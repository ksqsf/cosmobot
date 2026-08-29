import { effectScope, nextTick, ref } from 'vue'
import { describe, expect, it } from 'vitest'
import { useOverlayLayer } from '@/overlay'

describe('overlay stack', () => {
  it('gives Escape ownership only to the topmost visible layer', async () => {
    const firstVisible = ref(true)
    const secondVisible = ref(false)
    const scope = effectScope()
    const layers = scope.run(() => ({ first: useOverlayLayer(firstVisible), second: useOverlayLayer(secondVisible) }))
    if (layers === undefined) throw new Error('effect scope did not run')

    expect(layers.first.isTop.value).toBe(true)
    secondVisible.value = true
    await nextTick()
    expect(layers.first.isTop.value).toBe(false)
    expect(layers.second.isTop.value).toBe(true)

    secondVisible.value = false
    await nextTick()
    expect(layers.first.isTop.value).toBe(true)
    scope.stop()
  })
})
