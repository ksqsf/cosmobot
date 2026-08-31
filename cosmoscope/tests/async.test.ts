import { effectScope } from 'vue'
import { describe, expect, it, vi } from 'vitest'
import { useLatest, useLatestSubscription } from '@/async'

describe('latest async ownership', () => {
  it('keeps only the latest token and invalidates it with its scope', () => {
    const scope = effectScope()
    const latest = scope.run(useLatest)
    if (latest === undefined) throw new Error('Expected an active scope')
    const first = latest.begin()
    const second = latest.begin()
    expect(latest.current(first)).toBe(false)
    expect(latest.current(second)).toBe(true)
    scope.stop()
    expect(latest.current(second)).toBe(false)
    expect(latest.current(latest.begin())).toBe(false)
  })

  it('cleans replaced and late subscriptions', () => {
    const scope = effectScope()
    const owner = scope.run(useLatestSubscription)
    if (owner === undefined) throw new Error('Expected an active scope')
    const stopFirst = vi.fn()
    const stopLate = vi.fn()
    const stopSecond = vi.fn()
    const first = owner.begin()
    expect(owner.own(first, stopFirst)).toBe(true)
    const second = owner.begin()
    expect(stopFirst).toHaveBeenCalledOnce()
    expect(owner.own(first, stopLate)).toBe(false)
    expect(stopLate).toHaveBeenCalledOnce()
    expect(owner.own(second, stopSecond)).toBe(true)
    scope.stop()
    expect(stopSecond).toHaveBeenCalledOnce()
    const stopAfterDispose = vi.fn()
    expect(owner.own(owner.begin(), stopAfterDispose)).toBe(false)
    expect(stopAfterDispose).toHaveBeenCalledOnce()
  })
})
